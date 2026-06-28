{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | WAI integration for the lobby and per-game WebSockets.
--
-- The entry point 'wsMiddleware' wraps a WAI app and intercepts upgrade
-- requests for @/ws/lobby@ and @/ws/games/:id@. Anything else falls
-- through to the underlying Yesod app.
module Invasion.Server.WebSocket
  ( WsEnv (..)
  , wsMiddleware
  , idleSweeperLoop
  ) where

import Control.Concurrent (forkIO, threadDelay)
import Control.Concurrent.Async (race_)
import Control.Concurrent.STM
import Control.Exception (SomeException, try)
import Control.Monad (forever)
import Crypto.Random (getRandomBytes)
import Data.Aeson qualified as Aeson
import Data.Aeson.Key qualified as Aeson.Key
import Data.Aeson.KeyMap qualified as KM
import Data.ByteArray.Encoding qualified as BAE
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Lazy qualified as BSL
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import Data.UUID qualified as UUID
import Data.UUID.V4 (nextRandom)
import Database.Persist qualified as P
import Invasion.Auth.Jwt
  ( JwtClaims (..)
  , JwtSecret
  , verifyJwt
  )
import Invasion.Card (Card (..), SomeCardDef (..), asUnit)
import Invasion.CardDef (CardDef (..), Trait (Attachment))
import Invasion.CardDef qualified as CardDef
import Invasion.DB (DbPool, runDB)
import Invasion.Engine
  ( Message
      ( BeginCombat
      , BeginGame
      , PassPriority
      , PlayAttachment
      , PlayDevelopment
      , PlayLegend
      , PlayQuest
      , PlaySupport
      , PlayTactic
      , PlayUnit
      , Setup
      , TriggerCardAction
      )
  , newGame
  )
import Invasion.Engine qualified as Engine
import Invasion.Game
  ( ActionWindow (..)
  , ActionWindowTrigger (BattlefieldActionWindow)
  , Game (..)
  , Prompt (..)
  , PromptResult (..)
  )
import Invasion.Model
import Invasion.Player (Player (..))
import Invasion.Prelude
import Invasion.Server.Lobby
import Invasion.Server.Protocol
import Invasion.Server.Protocol qualified as Proto
import Invasion.Types
  ( CardCode (..)
  , Phase (BattlefieldPhase)
  , PlayerKey (..)
  , Race (..)
  , UnitKey
  , ZoneKind
  )
import Network.HTTP.Types (Query, parseQuery)
import Network.Wai (Application)
import Network.Wai.Handler.WebSockets (websocketsOr)
import Network.WebSockets qualified as WS

-- ----------------------------------------------------------------------------
-- Wiring

data WsEnv = WsEnv
  { lobby :: LobbyState
  , dbPool :: DbPool
  , jwtSecret :: JwtSecret
  }

-- | Wrap a WAI app so WS upgrades for our routes are intercepted.
wsMiddleware :: WsEnv -> Application -> Application
wsMiddleware env = websocketsOr WS.defaultConnectionOptions (dispatch env)

-- | Decide which handler to run based on the upgrade request URI.
dispatch :: WsEnv -> WS.ServerApp
dispatch env pending = do
  let req = WS.pendingRequest pending
      path = WS.requestPath req
      (pathOnly, qsBs) = BS.break (== 0x3F) path -- '?'
      qs = parseQuery (BS.drop 1 qsBs)
  case BS.split 0x2F (BS.dropWhile (== 0x2F) pathOnly) of
    ["ws", "lobby"] -> handleLobby env pending qs
    ["ws", "games", gidBs] -> handleGame env pending qs gidBs
    _ -> WS.rejectRequest pending "not found"

-- ----------------------------------------------------------------------------
-- Authentication

queryText :: ByteString -> Query -> Maybe Text
queryText key qs = case lookup key qs of
  Just (Just v) -> Just (decodeUtf8 v)
  _ -> Nothing

resolveUser :: WsEnv -> Query -> IO (Maybe UserInfo)
resolveUser env qs = case queryText "token" qs of
  Nothing -> pure Nothing
  Just tok -> do
    now <- getCurrentTime
    case verifyJwt env.jwtSecret now tok of
      Left _ -> pure Nothing
      Right claims -> do
        mu <- runDB env.dbPool (P.get (UserKey claims.sub))
        pure $ case mu of
          Just u -> Just UserInfo
            { userId = claims.sub
            , displayName = userDisplayName u
            }
          Nothing -> Nothing

-- ----------------------------------------------------------------------------
-- Lobby handler

handleLobby :: WsEnv -> WS.PendingConnection -> Query -> IO ()
handleLobby env pending qs = do
  -- Guests (no token, or an invalid/expired one) are accepted as
  -- read-only viewers — they see chat + games but can't post or host.
  muser <- resolveUser env qs
  conn <- WS.acceptRequest pending
  WS.withPingThread conn 25 (pure ()) (runLobbyConn env muser conn)

runLobbyConn :: WsEnv -> Maybe UserInfo -> WS.Connection -> IO ()
runLobbyConn env muser conn = do
  outbox <- atomically newTQueue
  cid <- atomically $ addLobbyConn env.lobby muser outbox
  -- Build the welcome inside one transaction so the snapshot is consistent.
  (welcome, usersNow) <- atomically do
    hist <- chatHistorySTM env.lobby.chat
    games <- summariesSTM env.lobby
    users <- uniqueUsersSTM env.lobby
    maint <- readMaintenanceSTM env.lobby
    pure
      ( LobbyWelcome {you = muser, users, games, chat = hist, maintenance = maint}
      , users
      )
  -- Send welcome only to this connection.
  atomically $ sendTo outbox welcome
  -- Notify everyone (including us) of the new user list.
  atomically $ broadcastLobby env.lobby LobbyUsersUpdate {users = usersNow}

  _ <- try @SomeException $
    race_ (lobbyWriter conn outbox) (lobbyReader env muser outbox conn)
  atomically do
    removeLobbyConn env.lobby cid
    us <- uniqueUsersSTM env.lobby
    broadcastLobby env.lobby LobbyUsersUpdate {users = us}

lobbyWriter :: WS.Connection -> TQueue LobbyOut -> IO ()
lobbyWriter conn outbox = forever do
  msg <- atomically $ readTQueue outbox
  WS.sendTextData conn (Aeson.encode msg)

lobbyReader :: WsEnv -> Maybe UserInfo -> TQueue LobbyOut -> WS.Connection -> IO ()
lobbyReader env muser outbox conn = forever do
  raw <- WS.receiveData conn :: IO BSL.ByteString
  case Aeson.eitherDecode raw of
    Left _ -> pure ()
    Right msg -> case muser of
      -- Guests reach this only if they raced the UI (we hide the
      -- affordances). Surface an explicit code rather than dropping it.
      Nothing -> atomically $ sendTo outbox LobbyError {code = "unauthorized"}
      Just user -> handleLobbyIn env user msg

handleLobbyIn :: WsEnv -> UserInfo -> LobbyIn -> IO ()
handleLobbyIn env user = \case
  LobbyChatSend {text}
    | T.null (T.strip text) -> pure ()
    | T.length text > 1000 -> pure ()
    | otherwise -> do
        now <- getCurrentTime
        let line = ChatLine {from = user, text, at = now}
        atomically do
          pushLobbyChat env.lobby line
          broadcastLobby env.lobby LobbyChatNew {line}
  LobbyCreateGame
    { name = gname
    , visibility
    , password
    , allowSpectators = mAllow
    , autoSkipActionWindows = mAutoSkip
    , useStarterDecks = mStarters
    } -> do
    let trimmedName = T.strip gname
    inMaint <- atomically $ isJust <$> readMaintenanceSTM env.lobby
    case validateGameName trimmedName of
      Just code -> notifyError env user code
      Nothing
        | inMaint -> notifyError env user "maintenance_in_progress"
        | otherwise -> case visibility of
        Private | maybe False badPw password -> notifyError env user "invalid_password"
        _ -> do
          gid <- nextRandom
          token <- randomTokenText 12
          -- Default: public games allow spectators, private games don't.
          let allowSpec = case mAllow of
                Just b -> b
                Nothing -> case visibility of
                  Public -> True
                  Private -> False
              autoSkip = fromMaybe False mAutoSkip
              starters = fromMaybe False mStarters
          slot <- atomically do
            slot <- createGame env.lobby gid trimmedName user visibility password allowSpec autoSkip starters token
            sm <- summariesSTM env.lobby
            broadcastLobby env.lobby LobbyGamesUpdate {games = sm}
            pure slot
          -- Reply directly to the creator with the created game + token.
          notifyMe env user
            LobbyGameCreated {gameId = slot.gameId, inviteToken = Just token}
  LobbyJoinPublic {gameId = gid} ->
    withSlot env user gid \slot -> case slot.visibility of
      Private -> notifyError env user "game_is_private"
      Public -> tryJoin env user slot gid
  LobbyJoinWithPassword {gameId = gid, password = mpw} ->
    withSlot env user gid \slot -> case (slot.password, mpw) of
      (Just expected, Just pw) | expected == pw -> tryJoin env user slot gid
      _ -> notifyError env user "wrong_password"
  where
    badPw t = T.length t < 1 || T.length t > 60

-- | Look up a game slot by id and pass it to the action; reply
-- @game_not_found@ if missing.
withSlot :: WsEnv -> UserInfo -> UUID -> (GameSlot -> IO ()) -> IO ()
withSlot env user gid action = do
  mslot <- atomically $ gameLookup env.lobby gid
  case mslot of
    Nothing -> notifyError env user "game_not_found"
    Just slot -> action slot

-- | Reply with 'LobbyGameJoinOk' or @game_full@ depending on slot capacity.
tryJoin :: WsEnv -> UserInfo -> GameSlot -> UUID -> IO ()
tryJoin env user slot gid = do
  ok <- atomically $ canJoinAsAnything slot user
  if ok
    then notifyMe env user LobbyGameJoinOk {gameId = gid, inviteToken = Nothing}
    else notifyError env user "game_full"

notifyMe :: WsEnv -> UserInfo -> LobbyOut -> IO ()
notifyMe env user msg = atomically do
  cs <- readTVar env.lobby.connections
  for_ (Map.elems cs) \c -> case c.user of
    Just u | u.userId == user.userId -> sendTo c.outbox msg
    _ -> pure ()

notifyError :: WsEnv -> UserInfo -> Text -> IO ()
notifyError env user code = notifyMe env user LobbyError {code}

validateGameName :: Text -> Maybe Text
validateGameName n
  | T.length n < 1 = Just "name_too_short"
  | T.length n > 80 = Just "name_too_long"
  | otherwise = Nothing

-- ----------------------------------------------------------------------------
-- Game handler

handleGame :: WsEnv -> WS.PendingConnection -> Query -> ByteString -> IO ()
handleGame env pending qs gidBs = case UUID.fromASCIIBytes gidBs of
  Nothing -> WS.rejectRequest pending "bad game id"
  Just gid -> do
    -- Guests are accepted as spectators only (see 'canEnter').
    muser <- resolveUser env qs
    mslot <- atomically $ gameLookup env.lobby gid
    case mslot of
      Nothing -> WS.rejectRequest pending "game not found"
      Just slot -> do
        let mPw = queryText "password" qs
            mInvite = queryText "t" qs
        authed <- canEnter slot muser mPw mInvite
        if not authed
          then WS.rejectRequest pending "forbidden"
          else do
            conn <- WS.acceptRequest pending
            WS.withPingThread conn 25 (pure ()) (runGameConn env slot muser conn)

canEnter :: GameSlot -> Maybe UserInfo -> Maybe Text -> Maybe Text -> IO Bool
canEnter slot muser mPw mInvite = atomically do
  sm <- readTVar slot.seats
  case muser of
    -- Guest: spectator-only. The visibility/password gate still applies.
    Nothing
      | not slot.allowSpectators -> pure False
      | otherwise -> case slot.visibility of
          Public -> pure True
          Private -> case (mInvite, mPw, slot.password) of
            (Just t, _, _) | t == slot.inviteToken -> pure True
            (_, Just pw, Just expected) | pw == expected -> pure True
            _ -> pure False
    Just user -> do
      let alreadySeated = any (\r -> r.user.userId == user.userId) (Map.elems sm)
          isHost = user.userId == slot.host.userId
          hasSlot = Map.size sm < 2
          -- A signed-in viewer who isn't seated and isn't the host may
          -- still enter as a spectator if the game allows it.
          canBeHere = hasSlot || slot.allowSpectators
      if alreadySeated || isHost
        then pure True
        else case slot.visibility of
          Public -> pure canBeHere
          Private -> case (mInvite, mPw, slot.password) of
            (Just t, _, _) | t == slot.inviteToken -> pure canBeHere
            (_, Just pw, Just expected) | pw == expected -> pure canBeHere
            _ -> pure False

-- | Lobby-side check used by 'LobbyJoinPublic' / 'LobbyJoinWithPassword'
-- after the visibility/password gate has passed. A user can enter the
-- slot if they already have a seat, if a seat is available, or if the
-- game permits spectators.
canJoinAsAnything :: GameSlot -> UserInfo -> STM Bool
canJoinAsAnything slot user = do
  sm <- readTVar slot.seats
  let alreadySeated = any (\r -> r.user.userId == user.userId) (Map.elems sm)
      hasSlot = Map.size sm < 2
  pure (alreadySeated || hasSlot || slot.allowSpectators)

runGameConn :: WsEnv -> GameSlot -> Maybe UserInfo -> WS.Connection -> IO ()
runGameConn env slot muser conn = do
  outbox <- atomically newTQueue
  -- Try to reserve a seat (or reclaim existing one). Guests skip this
  -- entirely and always attach as spectators. If both seats are taken
  -- for an authed viewer and the slot allows spectators, attach as a
  -- spectator instead.
  mSeat <- case muser of
    Nothing -> pure Nothing
    Just user -> atomically $ reserveSeat slot user
  let kind = case mSeat of
        Just _ -> ConnSeated
        Nothing -> ConnSpectator
  case (mSeat, slot.allowSpectators) of
    (Nothing, False) -> do
      WS.sendTextData conn (Aeson.encode GameError {code = "no_seat"})
      WS.sendClose conn ("full" :: ByteString)
    _ -> do
      cid <- atomically $ attachGameConn slot muser outbox kind
      (welcomeView, maint) <- atomically do
        v <- gameViewSTM slot muser
        m <- readMaintenanceSTM env.lobby
        pure (v, m)
      atomically $ sendTo outbox GameWelcome {you = muser, game = welcomeView, maintenance = maint}
      -- Inform other connections + lobby listings about the new viewer.
      atomically do
        v <- gameViewSTM slot muser
        broadcastGameWithView slot v
        sm <- summariesSTM env.lobby
        broadcastLobby env.lobby LobbyGamesUpdate {games = sm}
      _ <- try @SomeException
        (race_ (gameWriter conn outbox) (gameReader env slot muser outbox conn))
      now <- getCurrentTime
      atomically do
        detachGameConn slot cid now
        -- Slot view changes when conns drop (live count). Re-broadcast.
        v <- gameViewSTM slot muser
        broadcastGameWithView slot v
        sm <- summariesSTM env.lobby
        broadcastLobby env.lobby LobbyGamesUpdate {games = sm}

-- | The game view depends on the viewer (host gets the invite token).
-- For broadcasts, build a per-recipient view; the host token only leaks
-- to the host's outbox.
broadcastGameWithView :: GameSlot -> GameView -> STM ()
broadcastGameWithView slot _ = do
  conns <- readTVar slot.connections
  traverse_ sendForConn (Map.elems conns)
  where
    sendForConn c = do
      v <- gameViewSTM slot c.user
      sendTo c.outbox GameUpdate {game = v}

-- | Start the long-lived engine worker for this game. Seeds Setup +
-- BeginGame, wires the broadcast hook so the engine publishes after
-- every message (or askPrompt suspension), and stores the mailbox
-- on the slot so WebSocket handlers can post to it.
startEngineWorker :: GameSlot -> Game -> IO ()
startEngineWorker slot initial = do
  mb <- newTQueueIO
  pubState <- newTVarIO initial
  let hook = do
        g <- readTVar pubState
        -- 'attachHandPlayability' is a view-only annotation: it walks
        -- the just-published 'Game' and stamps each hand card with the
        -- reason it isn't playable right now. We do it here (not inside
        -- the engine) so the engine's own state never reads from the
        -- annotated copy.
        writeTVar slot.engine (Just (Engine.attachHandPlayability g))
        conns <- readTVar slot.connections
        traverse_ (publishForConn slot) (Map.elems conns)
      ctx =
        Engine.EngineCtx
          { Engine.mailbox = mb
          , Engine.publishedState = pubState
          , Engine.broadcastUpdate = hook
          }
  atomically do
    writeTQueue mb (Engine.EngineMsg Setup)
    writeTQueue mb (Engine.EngineMsg BeginGame)
    writeTVar slot.engineMailbox (Just mb)
  tid <- forkIO (Engine.runEngineWorker initial ctx)
  atomically (writeTVar slot.engineWorker (Just tid))
  where
    publishForConn s c = do
      v <- gameViewSTM s c.user
      sendTo c.outbox GameUpdate {game = v}

-- | Post a single piece of engine mail to the slot's worker.
-- Returns False if the worker hasn't been started yet.
postToEngine :: GameSlot -> Engine.EngineMail -> IO Bool
postToEngine slot mail = do
  mmb <- readTVarIO slot.engineMailbox
  case mmb of
    Nothing -> pure False
    Just mb -> do
      atomically $ writeTQueue mb mail
      pure True

gameWriter :: WS.Connection -> TQueue GameOut -> IO ()
gameWriter conn outbox = forever do
  msg <- atomically $ readTQueue outbox
  WS.sendTextData conn (Aeson.encode msg)

gameReader :: WsEnv -> GameSlot -> Maybe UserInfo -> TQueue GameOut -> WS.Connection -> IO ()
gameReader env slot muser outbox conn = forever do
  raw <- WS.receiveData conn :: IO BSL.ByteString
  case Aeson.eitherDecode raw of
    Left _ -> pure ()
    Right msg -> case muser of
      Nothing -> atomically $ sendTo outbox GameError {code = "unauthorized"}
      Just user -> handleGameIn env slot user msg

handleGameIn :: WsEnv -> GameSlot -> UserInfo -> GameIn -> IO ()
handleGameIn env slot user = \case
  GameChatSend {text}
    | T.null (T.strip text) -> pure ()
    | T.length text > 1000 -> pure ()
    | otherwise -> do
        now <- getCurrentTime
        let line = ChatLine {from = user, text, at = now}
        atomically do
          pushGameChat slot line
          broadcastGame slot GameChatNew {line}
  GameSelectDeck {deckId = did}
    | slot.useStarterDecks -> sendGameError slot user "starter_mode"
    | otherwise -> do
        mDeck <- runDB env.dbPool (P.get (DeckKey did))
        case mDeck of
          Nothing -> sendGameError slot user "deck_not_found"
          Just deck | deckUserId deck /= UserKey user.userId ->
            sendGameError slot user "deck_not_owned"
          Just deck -> do
            sts <- readTVarIO slot.status
            if sts /= StatusWaiting
              then sendGameError slot user "game_started"
              else do
                seatKey <- atomically do
                  sm <- readTVar slot.seats
                  pure $ findSeatFor user sm
                case seatKey of
                  Nothing -> sendGameError slot user "not_seated"
                  Just k -> do
                    let dv = DeckView
                          { deckId = Just did
                          , starterRace = Nothing
                          , name = deckName deck
                          , capital = deckCapital deck
                          , size = countCards (deckCards deck)
                          }
                    atomically do
                      setSeatDeck slot k dv
                      v <- gameViewSTM slot (Just user)
                      broadcastGameWithView slot v
  GameSelectStarter {race}
    | not slot.useStarterDecks -> sendGameError slot user "starter_mode_off"
    | otherwise -> do
        sts <- readTVarIO slot.status
        if sts /= StatusWaiting
          then sendGameError slot user "game_started"
          else do
            seatKey <- atomically do
              sm <- readTVar slot.seats
              pure $ findSeatFor user sm
            case seatKey of
              Nothing -> sendGameError slot user "not_seated"
              Just k -> do
                let deck = Engine.starterDeckFor race
                    dv = DeckView
                      { deckId = Nothing
                      , starterRace = Just race
                      , name = raceStarterName race
                      , capital = Just (raceCapitalSlug race)
                      , size = length deck.cards
                      }
                atomically do
                  setSeatDeck slot k dv
                  v <- gameViewSTM slot (Just user)
                  broadcastGameWithView slot v
  GameClearDeck -> do
    sts <- readTVarIO slot.status
    when (sts == StatusWaiting) do
      seatKey <- atomically do
        sm <- readTVar slot.seats
        pure $ findSeatFor user sm
      case seatKey of
        Nothing -> pure ()
        Just k -> atomically do
          clearSeatDeck slot k
          v <- gameViewSTM slot (Just user)
          broadcastGameWithView slot v
  GameStart
    | user.userId /= slot.host.userId -> sendGameError slot user "not_host"
    | otherwise -> do
        sts <- readTVarIO slot.status
        if sts /= StatusWaiting
          then sendGameError slot user "already_started"
          else do
            sm <- readTVarIO slot.seats
            case (,) <$> (Map.lookup "Player1" sm >>= (.deck))
                     <*> (Map.lookup "Player2" sm >>= (.deck)) of
              Nothing -> sendGameError slot user "not_ready"
              Just (dv1, dv2) -> do
                  eDecks <- resolveDecks env slot dv1 dv2
                  case eDecks of
                    Left code -> sendGameError slot user code
                    Right (ed1, ed2) -> do
                      let opts =
                            Engine.GameOptions
                              { autoSkipActionWindows = slot.autoSkipActionWindows
                              }
                      case newGame ed1 ed2 opts of
                        Left err -> sendGameError slot user (T.pack err)
                        Right g0 -> do
                          -- Spin up the per-game engine worker. It
                          -- owns the engine state from here on; the
                          -- WebSocket handlers post to its mailbox
                          -- via 'postToEngine'. Setup + BeginGame
                          -- are seeded by 'startEngineWorker'.
                          startEngineWorker slot g0
                          atomically do
                            trySetStatus slot StatusPlaying
                            summaries <- summariesSTM env.lobby
                            broadcastLobby env.lobby LobbyGamesUpdate {games = summaries}
  GamePassPriority ->
    withSeatedPlayer slot user \pk ->
      postEngineMsg slot user (PassPriority pk)
  GamePlayCard {cardKey, zone, target} ->
    withSeatedPlayer slot user \pk -> do
      mGame <- readTVarIO slot.engine
      case mGame of
        Nothing -> sendGameError slot user "game_not_started"
        Just g -> case findHandCard pk cardKey g of
          Just someCard -> case playMessageFor pk cardKey zone target someCard of
            Left code -> sendGameError slot user code
            Right msg -> postEngineMsg slot user msg
          -- Not in hand: a Necromancy unit may be played from the
          -- discard pile instead.
          Nothing
            | isNecromancyDiscardCard pk cardKey g -> case zone of
                Just z -> postEngineMsg slot user (Engine.PlayUnitFromDiscard pk cardKey z)
                Nothing -> sendGameError slot user "zone_required"
            -- Mortis Engine: a unit in the opponent's discard pile.
            | isMortisReanimatable pk cardKey g -> case zone of
                Just z -> postEngineMsg slot user (Engine.MortisReanimate pk cardKey z)
                Nothing -> sendGameError slot user "zone_required"
            -- Lord of Change: the top card of the player's own deck may
            -- be played as though it were in hand. Route it through the
            -- same kind-based dispatch the hand path uses; the engine's
            -- play handlers pull it from the deck top.
            | Just someCard <- lordOfChangeTopCard pk cardKey g ->
                case playMessageFor pk cardKey zone target someCard of
                  Left code -> sendGameError slot user code
                  Right msg -> postEngineMsg slot user msg
            | otherwise -> sendGameError slot user "card_not_in_hand"
  GameTriggerAction {source, actionIndex, target, targetZone} ->
    withSeatedPlayer slot user \pk -> do
      let actionTarget = case (target, targetZone) of
            (Just u, _) -> CardDef.TargetUnit u
            (_, Just (Proto.ZoneTarget {player = zp, kind = zk})) ->
              CardDef.TargetZone zp zk
            _ -> CardDef.NoTarget
      postEngineMsg slot user $ TriggerCardAction pk source actionIndex actionTarget
  GameResolvePrompt {result} ->
    withSeatedPlayer slot user \pk -> do
      mGame <- readTVarIO slot.engine
      case mGame of
        Nothing -> sendGameError slot user "game_not_started"
        Just g -> case g.pendingPrompt of
          Nothing -> sendGameError slot user "no_pending_prompt"
          Just p
            | p.player /= pk -> sendGameError slot user "not_your_prompt"
            | otherwise -> do
                let engineResult = case result of
                      PromptUnitsWire {unitKeys = ks} -> PickUnits ks
                      PromptBoolWire {yes = b} -> PickBool b
                      PromptTargetOptionWire {option = o} -> PickTargetOption o
                      PromptAmountWire {amount = n} -> PickAmount n
                      PromptNoneWire -> PickNone
                ok <- postToEngine slot (Engine.EnginePromptAnswer engineResult)
                unless ok $ sendGameError slot user "game_not_started"
  GamePlayDevelopment {cardKey = ck, developmentZone = zk} ->
    withSeatedPlayer slot user \pk ->
      postEngineMsg slot user (PlayDevelopment pk ck zk)
  GameDeclareAttack {attackZone = tz, attackerKeys = ks} ->
    withSeatedPlayer slot user \pk -> do
      mGame <- readTVarIO slot.engine
      case mGame of
        Nothing -> sendGameError slot user "game_not_started"
        Just g
          | g.phase /= Just BattlefieldPhase ->
              sendGameError slot user "not_battlefield_phase"
          | g.currentPlayer /= pk ->
              sendGameError slot user "not_your_turn"
          | isJust g.combat ->
              sendGameError slot user "combat_in_progress"
          | maybe True (\aw -> aw.trigger /= BattlefieldActionWindow) g.actionWindow ->
              sendGameError slot user "wrong_action_window"
          | null ks ->
              sendGameError slot user "no_attackers"
          | otherwise ->
              postEngineMsg slot user (BeginCombat pk tz ks)
  GameLeave -> do
    sts <- readTVarIO slot.status
    -- A "leave" while playing ends the game — but only if the leaver
    -- was actually seated. Spectators dropping out just close their own
    -- connection.
    atomically do
      wasSeated <- removeSeat slot user.userId
      when (wasSeated && sts == StatusPlaying) (trySetStatus slot StatusEnded)
      v <- gameViewSTM slot (Just user)
      broadcastGameWithView slot v
      summaries <- summariesSTM env.lobby
      broadcastLobby env.lobby LobbyGamesUpdate {games = summaries}
      -- Push a Closed notice to this user's connections so the UI can
      -- redirect them back to the lobby cleanly.
      sendToUserConnsSTM slot user GameClosed {reason = "left"}

findSeatFor :: UserInfo -> Map.Map Text SeatRow -> Maybe Text
findSeatFor user =
  fmap fst . find (\(_, r) -> r.user.userId == user.userId) . Map.toList

seatKeyToPlayerKey :: Text -> Maybe PlayerKey
seatKeyToPlayerKey = \case
  "Player1" -> Just Player1
  "Player2" -> Just Player2
  _ -> Nothing

-- | Resolve the caller to the 'PlayerKey' for their seat in this slot
-- and pass it to the action. Emits @\"not_seated\"@ to the caller if
-- they aren't seated (spectators or wrong slot).
withSeatedPlayer :: GameSlot -> UserInfo -> (PlayerKey -> IO ()) -> IO ()
withSeatedPlayer slot user action = do
  seatKey <- atomically $ findSeatFor user <$> readTVar slot.seats
  case seatKey >>= seatKeyToPlayerKey of
    Nothing -> sendGameError slot user "not_seated"
    Just pk -> action pk

-- | Post an engine 'Message' to this slot's worker. Emits
-- @\"game_not_started\"@ to the caller if the engine isn't running.
postEngineMsg :: GameSlot -> UserInfo -> Message -> IO ()
postEngineMsg slot user msg = do
  ok <- postToEngine slot $ Engine.EngineMsg msg
  unless ok $ sendGameError slot user "game_not_started"

-- | Find the card instance in a player's hand by its 'UnitKey'.
findHandCard :: PlayerKey -> UnitKey -> Game -> Maybe SomeCardDef
findHandCard pk k g =
  let player = case pk of
        Player1 -> g.player1
        Player2 -> g.player2
   in (.def) <$> find ((== k) . (.key)) player.hand

-- | Is there a Necromancy unit with this key in the player's discard
-- pile? Used to route a 'GamePlayCard' for a card that isn't in hand to
-- the discard-play path.
isNecromancyDiscardCard :: PlayerKey -> UnitKey -> Game -> Bool
isNecromancyDiscardCard pk k g =
  let player = case pk of
        Player1 -> g.player1
        Player2 -> g.player2
   in case find ((== k) . (.key)) player.discard of
        Just c
          | Just cd <- asUnit c.def ->
              CardDef.Necromancy `elem` cd.keywords || k `elem` g.grantedNecromancy
        _ -> False

-- | Is there a unit with this key in the OPPONENT's discard pile while
-- the player controls a Mortis Engine? Routes a 'GamePlayCard' to the
-- Mortis Engine reanimate path.
isMortisReanimatable :: PlayerKey -> UnitKey -> Game -> Bool
isMortisReanimatable pk k g =
  Engine.controlsMortisEngine pk g
    && let opp = case pk of Player1 -> g.player2; Player2 -> g.player1
        in case find ((== k) . (.key)) opp.discard of
             Just c -> isJust (asUnit c.def)
             _ -> False

-- | When the player controls a Lord of Change, the top card of their
-- own deck may be played as though it were in hand. Returns that card's
-- definition if its key matches the deck top and the permission holds.
lordOfChangeTopCard :: PlayerKey -> UnitKey -> Game -> Maybe SomeCardDef
lordOfChangeTopCard pk k g
  | not (Engine.controlsLordOfChange pk g) = Nothing
  | otherwise =
      let player = case pk of
            Player1 -> g.player1
            Player2 -> g.player2
       in case player.deck of
            c : _ | c.key == k -> Just c.def
            _ -> Nothing

-- | Choose the engine 'Message' that corresponds to the player's
-- 'GamePlayCard' request, based on the card's static kind. Returns a
-- protocol error code if the request doesn't carry the data the kind
-- needs (e.g. attachments must include a target unit).
playMessageFor
  :: PlayerKey
  -> UnitKey
  -> Maybe ZoneKind
  -> Maybe UnitKey
  -> SomeCardDef
  -> Either Text Message
playMessageFor pk cardKey mZone mTarget = \case
  UnitCardDef _ -> case mZone of
    Just z -> Right (PlayUnit pk cardKey z)
    Nothing -> Left "zone_required"
  SupportCardDef cd
    | Attachment `elem` cd.traits -> case mTarget of
        Just t -> Right (PlayAttachment pk cardKey t)
        Nothing -> Left "target_required"
    | otherwise -> case mZone of
        Just z -> Right (PlaySupport pk cardKey z)
        Nothing -> Left "zone_required"
  QuestCardDef _ -> Right (PlayQuest pk cardKey)
  TacticCardDef _ -> Right (PlayTactic pk cardKey (tacticTargetFor mTarget mZone))
  LegendCardDef _ -> Right (PlayLegend pk cardKey)

-- | Map the legacy zone/unit fields on 'GamePlayCard' into the
-- richer 'ActionTarget' carried by 'PlayTactic'. Unit pick wins;
-- otherwise zone pick (against the opponent, since most zone-targeting
-- tactics are offensive); otherwise no target.
tacticTargetFor :: Maybe UnitKey -> Maybe ZoneKind -> CardDef.ActionTarget
tacticTargetFor (Just u) _ = CardDef.TargetUnit u
tacticTargetFor _ _ = CardDef.NoTarget

sendGameError :: GameSlot -> UserInfo -> Text -> IO ()
sendGameError slot user code =
  atomically $ sendToUserConnsSTM slot user GameError {code}

-- | Deliver a 'GameOut' to every connection on this slot whose user
-- matches @user@. Used for per-user replies that shouldn't broadcast to
-- the whole slot.
sendToUserConnsSTM :: GameSlot -> UserInfo -> GameOut -> STM ()
sendToUserConnsSTM slot user msg = do
  conns <- readTVar slot.connections
  for_ (Map.elems conns) \c -> case c.user of
    Just u | u.userId == user.userId -> sendTo c.outbox msg
    _ -> pure ()

countCards :: Aeson.Value -> Int
countCards v = sum [n | (_, n) <- deckCardCounts v]

-- | Parse the @{code: count}@ JSONB shape stored in @decks.cards@. Keys
-- whose values are not non-negative numbers are silently dropped — they
-- can't represent a real card-count anyway.
deckCardCounts :: Aeson.Value -> [(Text, Int)]
deckCardCounts (Aeson.Object o) =
  [ (Aeson.Key.toText k, n)
  | (k, Aeson.Number num) <- KM.toList o
  , let n = truncate (realToFrac num :: Double)
  , n > 0
  ]
deckCardCounts _ = []

-- | Translate a persistent 'Deck' row into the engine's 'Engine.Deck'.
-- Returns a protocol error code on failure so the client can localize it.
engineDeckFromDb :: Deck -> Either Text Engine.Deck
engineDeckFromDb d = do
  capitalSlug <- maybeToEither "missing_capital" $ deckCapital d
  race <- maybeToEither "unsupported_capital" $ Map.lookup capitalSlug capitalRaces
  let cards =
        [ CardCode (T.unpack code)
        | (code, n) <- deckCardCounts (deckCards d)
        , _ <- replicate n ()
        ]
  Right Engine.Deck {cards, race}

-- | Wire-side capital slug → engine race. The slug is what the frontend
-- writes into @decks.capital@.
capitalRaces :: Map.Map Text Race
capitalRaces = Map.fromList
  [ ("dwarf", Dwarf)
  , ("empire", Empire)
  , ("high_elf", HighElf)
  , ("chaos", Chaos)
  , ("orc", Orc)
  , ("dark_elf", DarkElf)
  ]

-- | Inverse of 'capitalRaces': engine race → capital slug. Used when
-- the server synthesizes a 'DeckView' for a starter deck.
raceCapitalSlug :: Race -> Text
raceCapitalSlug = \case
  Dwarf -> "dwarf"
  Empire -> "empire"
  HighElf -> "high_elf"
  Chaos -> "chaos"
  Orc -> "orc"
  DarkElf -> "dark_elf"

-- | Display name embedded in the 'DeckView' for a starter deck. The
-- frontend localizes the seat label separately; this is the fallback
-- string carried on the wire.
raceStarterName :: Race -> Text
raceStarterName = \case
  Dwarf -> "Dwarf Starter"
  Empire -> "Empire Starter"
  HighElf -> "High Elf Starter"
  Chaos -> "Chaos Starter"
  Orc -> "Orc Starter"
  DarkElf -> "Dark Elf Starter"

-- | Resolve both seats' deck selections to engine 'Deck' values for
-- 'GameStart'. Honours the slot's 'useStarterDecks' flag: in starter
-- mode the seat's 'starterRace' is the only thing we read; otherwise
-- we go to the DB. Returns a protocol error code on the first failure.
resolveDecks
  :: WsEnv
  -> GameSlot
  -> DeckView
  -> DeckView
  -> IO (Either Text (Engine.Deck, Engine.Deck))
resolveDecks env slot dv1 dv2
  | slot.useStarterDecks = pure do
      r1 <- maybeToEither "starter_not_selected" dv1.starterRace
      r2 <- maybeToEither "starter_not_selected" dv2.starterRace
      Right (Engine.starterDeckFor r1, Engine.starterDeckFor r2)
  | otherwise = do
      let dbDeck dv = maybeToEither "deck_not_found" dv.deckId
      case (dbDeck dv1, dbDeck dv2) of
        (Left e, _) -> pure (Left e)
        (_, Left e) -> pure (Left e)
        (Right did1, Right did2) -> do
          md1 <- runDB env.dbPool (P.get (DeckKey did1))
          md2 <- runDB env.dbPool (P.get (DeckKey did2))
          case (md1, md2) of
            (Nothing, _) -> pure (Left "deck_not_found")
            (_, Nothing) -> pure (Left "deck_not_found")
            (Just d1, Just d2) ->
              pure $ (,) <$> engineDeckFromDb d1 <*> engineDeckFromDb d2

-- ----------------------------------------------------------------------------
-- Idle sweeper

idleSweeperLoop :: WsEnv -> IO ()
idleSweeperLoop env = forever do
  threadDelay (60 * 1_000_000)
  now <- getCurrentTime
  atomically do
    ds <- sweepIdle env.lobby now
    unless (null ds) do
      sm <- summariesSTM env.lobby
      broadcastLobby env.lobby LobbyGamesUpdate {games = sm}

-- ----------------------------------------------------------------------------
-- Random tokens

randomTokenText :: Int -> IO Text
randomTokenText n = do
  bs <- getRandomBytes n :: IO BS.ByteString
  pure (decodeUtf8 (BAE.convertToBase BAE.Base64URLUnpadded bs))
