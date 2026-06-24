{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Engine (module Invasion.Engine, module Invasion.Message) where

import Control.Monad.Random
import Control.Monad.State.Strict
import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Data.Time (getCurrentTime)
import Data.Traversable
import Invasion.Capital
import Invasion.Card
import Invasion.CardDef
import Invasion.Entity (LegendDetails (..), QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Control.Concurrent.STM
import Data.IORef
import Queue
import System.Random.Shuffle

-- | A single incoming item on a game's mailbox. Either a fresh
-- 'Message' from a client (engine processes it like any queued msg)
-- or a 'PromptResult' answering an outstanding 'askPrompt'.
data EngineMail
  = EngineMsg Message
  | EnginePromptAnswer PromptResult
  deriving stock Show

-- | Per-game runtime context used when the engine runs as a long-lived
-- worker thread. Carries the mailbox the worker drains, the published
-- state TVar clients observe, and a broadcast hook the engine calls
-- after every state publish so the WebSocket layer can push updates.
data EngineCtx = EngineCtx
  { mailbox :: TQueue EngineMail
  , publishedState :: TVar Game
  , broadcastUpdate :: STM ()
  }

data Env = Env
  { queue :: Queue Message
  , game :: Game
  , ctx :: Maybe EngineCtx
    -- ^ 'Nothing' for one-shot 'applyMessage' calls (tests, debug).
    -- 'Just' once a 'GameWorker' is attached; receive bodies that
    -- call 'askPrompt' will then actually publish + block.
  , scriptedAnswers :: Maybe (IORef [PromptResult])
    -- ^ Test-only: a queue of canned 'PromptResult' answers consumed
    -- in order by 'askPrompt' instead of going through the worker
    -- mailbox or the 'autoResolve' fallback. When 'Nothing' or the
    -- list is empty the normal path applies.
  }

newEnv :: Game -> IO Env
newEnv g = do
  q <- newQueue
  pure $ Env q g Nothing Nothing

newEnvWithCtx :: Game -> EngineCtx -> IO Env
newEnvWithCtx g c = do
  q <- newQueue
  pure $ Env q g (Just c) Nothing

-- | Build an 'Env' that satisfies 'askPrompt' from a fixed list of
-- canned answers. Used by tests to drive the prompt-based combat flow
-- deterministically without spinning up a worker.
newEnvWithAnswers :: Game -> [PromptResult] -> IO Env
newEnvWithAnswers g answers = do
  q <- newQueue
  ref <- newIORef answers
  pure $ Env q g Nothing (Just ref)

newtype GameT a = GameT (StateT Env IO a)
  deriving newtype (Functor, Applicative, Monad, MonadIO, MonadRandom, MonadState Env)

instance HasQueue Message GameT where
  getQueue = gets (.queue)

instance HasGame GameT where
  getGame = gets (.game)

-- | Effect-typeclass for receive bodies that need to suspend until a
-- player answers. Tests stub it with an auto-resolver; the worker
-- thread runs the real blocking version.
class Monad m => HasPromptIO m where
  askPrompt :: Prompt -> m PromptResult

instance HasPromptIO m => HasPromptIO (StateT s m) where
  askPrompt = lift . askPrompt

instance HasPromptIO GameT where
  askPrompt p = do
    env <- get
    case env.scriptedAnswers of
      Just ref -> do
        -- Test path: consume the next canned answer; if exhausted,
        -- fall back to the same auto-decline the no-context path
        -- uses so older tests keep working.
        liftIO (atomicModifyIORef' ref popAnswer) >>= \case
          Just a -> pure a
          Nothing -> pure (autoResolve p)
      Nothing -> case env.ctx of
        Nothing ->
          -- Test / debug path: no worker is wired, so just decline.
          -- Receive bodies see PickNone / PickBool False (depending on
          -- prompt kind) and proceed as if the player skipped.
          pure (autoResolve p)
        Just c -> do
          -- Stash the prompt on the in-memory state, sync to the
          -- published TVar (so clients see it), then STM-retry-block
          -- waiting for an answer in the mailbox.
          modify \g -> g {game = g.game {pendingPrompt = Just p}}
          publishCurrent c
          answer <- liftIO (waitForPromptAnswer c.mailbox)
          modify \g -> g {game = g.game {pendingPrompt = Nothing}}
          publishCurrent c
          pure answer
    where
      popAnswer [] = ([], Nothing)
      popAnswer (a : rest) = (rest, Just a)

-- | Default answer when no mailbox is attached. Most prompts in the
-- core set treat skip / decline as a no-op for the source card, which
-- keeps tests deterministic.
autoResolve :: Prompt -> PromptResult
autoResolve p = case p.kind of
  ChooseYesNo {} -> PickBool False
  _ -> PickNone

-- | Mirror the working state into the published TVar, then fire the
-- broadcast hook so clients re-render. Called whenever the engine
-- suspends (pending prompt) so the wire sees the latest snapshot.
publishCurrent :: EngineCtx -> GameT ()
publishCurrent c = do
  g <- gets (.game)
  liftIO $ atomically do
    writeTVar c.publishedState g
    c.broadcastUpdate

-- | STM-retry until a 'PromptResult' arrives in the mailbox. Any
-- 'EngineMsg's drained while waiting get put back at the front so
-- they process in arrival order after the prompt resolves.
waitForPromptAnswer :: TQueue EngineMail -> IO PromptResult
waitForPromptAnswer mb = atomically loop
  where
    loop = do
      drained <- drainAll
      case partitionAnswer [] drained of
        Just (r, rest) -> do
          traverse_ (writeTQueue mb) rest
          pure r
        Nothing -> do
          traverse_ (writeTQueue mb) drained
          retry
    drainAll = do
      ma <- tryReadTQueue mb
      case ma of
        Nothing -> pure []
        Just a -> (a :) <$> drainAll
    partitionAnswer _ [] = Nothing
    partitionAnswer acc (EnginePromptAnswer r : xs) =
      Just (r, reverse acc <> xs)
    partitionAnswer acc (x : xs) = partitionAnswer (x : acc) xs

data Deck = Deck
  { cards :: [CardCode]
  , race :: Race
  }

-- | Size of each pre-built starter deck. The dwarf list below is the
-- existing canonical 40-card starter; the other races match.
starterDeckSize :: Int
starterDeckSize = 40

-- | The pre-built 40-card starter deck for the given race. Used both by
-- 'runSetup' (the legacy debug path that boots a self-vs-self game) and
-- by the WebSocket server when the host enables \"use starter decks\"
-- on a new game. Card pools are drawn from the core set only.
starterDeckFor :: Race -> Deck
starterDeckFor r = Deck {race = r, cards = starterDeckCards r}

-- | Per-race preconstructed starter deck. Quantities mirror the
-- 'quantity' field in @cards.json@, which lines up with the
-- preconstructed deck composition that ships in the published core
-- set (40 cards each for the four main races). High Elf and Dark Elf
-- only have 5 cards in the core box, so their starters are stubbed
-- by combining those 5 cards with all 16 Neutrals — playable as a
-- 21-card deck for setup smoke, not a tournament-legal build.
starterDeckCards :: Race -> [CardCode]
starterDeckCards = \case
  Dwarf ->
    replicate 3 "core-001"
      <> replicate 1 "core-002"
      <> replicate 3 "core-003"
      <> replicate 3 "core-004"
      <> replicate 3 "core-005"
      <> replicate 1 "core-006"
      <> replicate 1 "core-007"
      <> replicate 1 "core-008"
      <> replicate 1 "core-009"
      <> replicate 2 "core-010"
      <> replicate 2 "core-011"
      <> replicate 1 "core-012"
      <> replicate 1 "core-013"
      <> replicate 3 "core-014"
      <> replicate 1 "core-015"
      <> replicate 1 "core-016"
      <> replicate 1 "core-017"
      <> replicate 1 "core-018"
      <> replicate 1 "core-019"
      <> replicate 1 "core-020"
      <> replicate 2 "core-021"
      <> replicate 1 "core-022"
      <> replicate 1 "core-023"
      <> replicate 2 "core-024"
      <> replicate 1 "core-025"
  Empire ->
    replicate 1 "core-026"
      <> replicate 3 "core-027"
      <> replicate 1 "core-028"
      <> replicate 1 "core-029"
      <> replicate 2 "core-030"
      <> replicate 1 "core-031"
      <> replicate 1 "core-032"
      <> replicate 3 "core-033"
      <> replicate 3 "core-034"
      <> replicate 2 "core-035"
      <> replicate 3 "core-036"
      <> replicate 1 "core-037"
      <> replicate 1 "core-038"
      <> replicate 2 "core-039"
      <> replicate 1 "core-040"
      <> replicate 1 "core-041"
      <> replicate 1 "core-042"
      <> replicate 2 "core-043"
      <> replicate 1 "core-044"
      <> replicate 1 "core-045"
      <> replicate 3 "core-046"
      <> replicate 1 "core-047"
      <> replicate 1 "core-048"
      <> replicate 1 "core-049"
      <> replicate 1 "core-050"
  HighElf ->
    -- Only 5 High Elf cards exist in core; pad with neutrals so the
    -- deck has enough draws to play out a setup smoke.
    concatMap (replicate 1) ["core-051", "core-052", "core-053", "core-054", "core-055"]
      <> neutralPadding
  Orc ->
    replicate 3 "core-056"
      <> replicate 3 "core-057"
      <> replicate 1 "core-058"
      <> replicate 2 "core-059"
      <> replicate 3 "core-060"
      <> replicate 2 "core-061"
      <> replicate 1 "core-062"
      <> replicate 1 "core-063"
      <> replicate 1 "core-064"
      <> replicate 1 "core-065"
      <> replicate 3 "core-066"
      <> replicate 1 "core-067"
      <> replicate 1 "core-068"
      <> replicate 2 "core-069"
      <> replicate 1 "core-070"
      <> replicate 1 "core-071"
      <> replicate 2 "core-072"
      <> replicate 1 "core-073"
      <> replicate 1 "core-074"
      <> replicate 1 "core-075"
      <> replicate 2 "core-076"
      <> replicate 3 "core-077"
      <> replicate 1 "core-078"
      <> replicate 1 "core-079"
      <> replicate 1 "core-080"
  Chaos ->
    replicate 3 "core-081"
      <> replicate 3 "core-082"
      <> replicate 3 "core-083"
      <> replicate 1 "core-084"
      <> replicate 2 "core-085"
      <> replicate 1 "core-086"
      <> replicate 1 "core-087"
      <> replicate 1 "core-088"
      <> replicate 1 "core-089"
      <> replicate 3 "core-090"
      <> replicate 2 "core-091"
      <> replicate 1 "core-092"
      <> replicate 1 "core-093"
      <> replicate 1 "core-094"
      <> replicate 1 "core-095"
      <> replicate 1 "core-096"
      <> replicate 2 "core-097"
      <> replicate 3 "core-098"
      <> replicate 2 "core-099"
      <> replicate 1 "core-100"
      <> replicate 1 "core-101"
      <> replicate 1 "core-102"
      <> replicate 2 "core-103"
      <> replicate 1 "core-104"
      <> replicate 1 "core-105"
  DarkElf ->
    concatMap (replicate 1) ["core-106", "core-107", "core-108", "core-109", "core-110"]
      <> neutralPadding
  where
    -- All 16 Neutral core cards, one of each — used to pad the
    -- elven starters since neither race has a full 40-card core.
    neutralPadding =
      [ "core-111", "core-112", "core-113", "core-114", "core-115"
      , "core-116", "core-117", "core-118"
      , "core-120", "core-121"
      , "core-122", "core-123", "core-124"
      , "core-125", "core-126", "core-127"
      ]

type DeckLoadError = String

loadDeck :: Deck -> Either DeckLoadError (Race, [SomeCardDef])
loadDeck Deck {race, cards} = (race,) <$> for cards \c ->
  case Map.lookup c allCards of
    Nothing -> Left $ "Card not found: " <> show c
    Just cardDef -> Right cardDef

runGame :: GameT a -> Env -> IO a
runGame (GameT inner) = evalStateT inner

execGameT :: GameT a -> Env -> IO Env
execGameT (GameT inner) = execStateT inner

-- | Run a long-lived engine pump for one game. Owns the engine
-- state; processes incoming 'EngineMail' items one at a time. Each
-- 'EngineMsg' is fed into the queue and 'gameMain' drains it; any
-- 'askPrompt' inside a receive body publishes intermediate state to
-- 'publishedState' and blocks until the matching
-- 'EnginePromptAnswer' arrives. Loops forever; kill the thread to
-- stop. Stray 'EnginePromptAnswer's (no prompt outstanding) are
-- silently dropped.
runEngineWorker :: Game -> EngineCtx -> IO ()
runEngineWorker initial ctx = do
  env0 <- newEnvWithCtx initial ctx
  atomically do
    writeTVar ctx.publishedState initial
    ctx.broadcastUpdate
  let loop env = do
        item <- atomically (readTQueue ctx.mailbox)
        case item of
          EngineMsg msg -> do
            env' <- execGameT (send msg >> () <$ gameMain) env
            atomically do
              writeTVar ctx.publishedState env'.game
              ctx.broadcastUpdate
            loop env'
          EnginePromptAnswer _ ->
            -- Out-of-context prompt answer. Ignore and keep
            -- listening — askPrompt has its own loop that uses STM
            -- retry, so it won't miss a real answer.
            loop env
  loop env0

overGame :: (Game -> GameT Game) -> GameT ()
overGame f = do
  game <- f =<< getGame
  modify \e -> e {game}

-- | Pump the queue until either empty or the game has ended. Returning
-- here is also how the engine exposes "we're waiting for player input"
-- — an open action window stops emitting messages and the queue drains,
-- and a pending prompt halts pumping entirely until the client posts a
-- 'ResolvePrompt'.
gameMain :: GameT Game
gameMain = do
  game <- getGame
  case game.lifecycle of
    GameFinished _ -> pure game
    _ -> case game.pendingPrompt of
      Just _ -> pure game
      Nothing -> do
        mmsg <- pop
        case mmsg of
          Just msg -> do
            overGame $ distribute msg
            gameMain
          Nothing -> pure game

class Run a where
  distribute :: Message -> a -> GameT a
  distribute msg = execStateT (receive msg)
  receive :: Message -> StateT a GameT ()

send :: HasQueue Message m => Message -> m ()
send = push

class Keyed a where
  type KeyOf a
  toKey :: a -> KeyOf a

onKey :: (Keyed a, Eq (KeyOf a), Monad m) => KeyOf a -> StateT a m () -> StateT a m ()
onKey k f = do
  a <- get
  when (toKey a == k) f

instance Keyed Player where
  type KeyOf Player = PlayerKey
  toKey = (.key)

instance Run Player where
  receive = \case
    Setup -> do
      k <- gets (.key)
      send $ ShuffleDeck k
      send $ Draw $ Drawing StartingHand k
    ShuffleDeck k -> onKey k do
      deck <- shuffleM =<< gets (.deck)
      modify \p -> p {deck}
    Draw drawing -> onKey drawing.player do
      case drawing.kind of
        StartingHand -> do
          (hand, deck) <- splitAt 7 <$> gets (.deck)
          modify \p -> p {hand, deck}
        StandardDraw -> do
          -- Draw restriction (Infiltrate the tactic): once the
          -- player has drawn up to their cap this turn, further
          -- standard draws are swallowed. The pre-message game state
          -- carries the count of draws already taken.
          g <- getGame
          let cap = Map.findWithDefault maxBound drawing.player g.drawCaps
              drawn =
                Map.findWithDefault 0 drawing.player
                  (historyOfScope ThisTurn g).drawnBy
          when (drawn < cap) do
            deck <- gets (.deck)
            case deck of
              [] -> pure ()
              (c : rest) -> do
                hand <- gets (.hand)
                modify \p -> p {hand = hand <> [c], deck = rest}
            -- "Drawing from an empty deck does not auto-fail mid-phase
            -- — but the standing 'running out of cards' rule eliminates
            -- a player who has zero cards in deck." So check AFTER each
            -- draw, including the one that emptied the deck.
            deck' <- gets (.deck)
            elim <- gets (.eliminated)
            when (null deck' && not elim) $
              send $ Eliminate drawing.player DeckedOut
    DrawFromBottom k -> onKey k do
      deck <- gets (.deck)
      case reverse deck of
        [] -> pure ()
        (bottom : restRev) -> do
          hand <- gets (.hand)
          modify \p -> p {hand = hand <> [bottom], deck = reverse restRev}
          deck' <- gets (.deck)
          elim <- gets (.eliminated)
          when (null deck' && not elim) $
            send $ Eliminate k DeckedOut
    ReturnResources k -> onKey k $
      modify \p -> p {resources = Resources 0}
    -- CollectResources and QuestDraw set the right values from
    -- Game.receive once it can see all in-play unit/support power
    -- icons across both sides. Player.receive deliberately doesn't
    -- touch resources / draws for these messages — the engine-level
    -- handler owns them.
    CollectResources _k -> pure ()
    QuestDraw _k -> pure ()
    Eliminate k reason -> onKey k $
      modify \p -> p {state = Eliminated reason}
    _ -> pure ()

instance Run Game where
  distribute msg g = do
    player1 <- distribute msg g.player1
    player2 <- distribute msg g.player2
    let preUnits = g.units
        preSupports = g.supports
        preQuests = g.quests
        preLegends = g.legends
    g' <- execStateT (receive msg) (g {player1, player2})
    -- Recompute cached effective stats on every unit before card
    -- receives run. This way attachments/experiences/burning all show
    -- through immediately and damage destruction uses the right HP.
    let g'' = recomputeUnitStats g'
    dispatchToInPlayUnits msg preUnits g''
    dispatchToInPlaySupports msg preSupports g''
    dispatchToInPlayQuests msg preQuests g''
    dispatchToInPlayLegends msg preLegends g''
    -- Stat-derived destruction sweep: a unit whose accumulated damage
    -- now meets or exceeds its (possibly shrunken) effective max HP
    -- dies, even though no fresh damage landed. Catches HP-lowering
    -- effects and X-HP units whose X collapsed (Cold One Chariot
    -- with no developments). Routed through 'CheckUnitVitals' (which
    -- re-verifies lethality at fire time) rather than a direct
    -- 'DestroyUnit', so a unit that gets saved in the meantime isn't
    -- killed by a stale duplicate.
    for_ g''.units \u -> do
      let Damage d = u.damage
      when (d >= u.effectiveMaxHP) $ push (CheckUnitVitals u.key)
    pure g''
  receive = \case
    Setup -> do
      fp <- sample2 Player1 Player2
      modify \g -> g {firstPlayer = fp, currentPlayer = fp}
      logIt LogSystem "log.setup.begins" [("player", playerParam fp)]
    BeginGame -> do
      fp <- gets (.firstPlayer)
      modify \g -> g {lifecycle = GamePlaying}
      logIt LogSystem "log.game.begins" []
      send $ BeginTurn fp
    BeginTurn k -> do
      modify \g ->
        g
          { currentPlayer = k
          , turn = g.turn + Turn 1
          , history = Map.insert ThisTurn mempty g.history
          , -- Per-turn capital defenses and one-shot play modifiers reset.
            capitalDefenseUsed = mempty
          , pendingUnitDiscount = mempty
          , pendingUnitOnPlayDamage = mempty
          , unitsRedirectedThisTurn = mempty
          , lastResolvedTactic = Nothing
          , pendingActionCancel = mempty
          , developmentPlayedThisTurn = False
          , attackBlockedThisTurn = []
          , lastRevealed = []
          }
      t <- gets (.turn)
      logIt LogTurn
        "log.turn.begins"
        [("turn", turnText t), ("player", playerParam k)]
      -- Phase 0 (FAQ 2.2): all "at the beginning of the turn" triggered
      -- effects fire on BeginTurn itself via card receive hooks; the
      -- action window then lets either player respond before the
      -- Kingdom phase begins. The Kingdom phase is only kicked off
      -- when this window CLOSES — see 'CloseActionWindow'. Queueing
      -- BeginPhase here directly would interleave kingdom upkeep with
      -- the still-open begin-of-turn window.
      send $ OpenActionWindow BeginningOfTurnActionWindow
    EndTurn k -> do
      -- Phase 5: open the end-of-turn window; only AFTER it closes do
      -- the queued end-of-turn effects fire, modifiers expire, and
      -- the turn flips. Handled in 'CloseActionWindow' so the
      -- ordering matches the rulebook (window → effects →
      -- modifiers-expire → next turn).
      logIt LogTurn "log.turn.ends" [("player", playerParam k)]
      send $ OpenActionWindow EndOfTurnActionWindow
    BeginPhase phase -> do
      g <- get
      modify \gx ->
        gx
          { phase = Just phase
          , history = Map.insert ThisPhase mempty gx.history
          }
      if shouldSkipFirstTurnPhase phase g
        then do
          logIt LogPhase "log.phase.skipped" [("phase", phaseParam phase)]
          send $ EndPhase phase
        else do
          logIt LogPhase "log.phase.begins" [("phase", phaseParam phase)]
          let active = g.currentPlayer
          case phase of
            KingdomPhase -> do
              send $ ReturnResources active
              send $ RestoreOneCorruptCard active
              send $ CollectResources active
              send $ OpenActionWindow KingdomActionWindow
            QuestPhase -> do
              send $ QuestDraw active
              send $ OpenActionWindow QuestActionWindow
            CapitalPhase ->
              send $ OpenActionWindow CapitalActionWindow
            BattlefieldPhase ->
              -- With no units yet, the active player has no attack to
              -- declare; the single window suffices. Combat will later
              -- emit the 5-step sub-sequence here.
              send $ OpenActionWindow BattlefieldActionWindow
    EndPhase phase -> do
      -- Fire any scheduled end-of-phase effects for this phase before
      -- handing off.
      (mine, rest) <- gets (partition ((== phase) . fst) . (.pendingEndOfPhase))
      modify \g -> g {pendingEndOfPhase = rest}
      traverse_ (firePendingEffect . snd) mine
      -- "Until the end of the phase" zone-damage draw watchers expire.
      modify \g -> g {phase = Nothing, zoneDamageDrawWatchers = []}
      logIt LogPhase "log.phase.ends" [("phase", phaseParam phase)]
      case nextPhase phase of
        Just np -> send $ BeginPhase np
        Nothing -> do
          current <- gets (.currentPlayer)
          send $ EndTurn current
    OpenActionWindow trigger -> do
      current <- gets (.currentPlayer)
      let aw = ActionWindow {trigger, awaiting = NoPasses current}
      modify \g ->
        let stack' = aw : g.actionWindowStack
         in g
              { actionWindowStack = stack'
              , actionWindow = Just aw
              }
      logIt LogSystem
        "log.window.open"
        [("trigger", triggerParam trigger), ("player", playerParam current)]
      maybeAutoPassPriority trigger current
    PassPriority k -> do
      g <- get
      case g.actionWindowStack of
        (aw : rest) | priorityHolder aw.awaiting == k -> do
          logIt LogPlayerAction "log.priority.pass" [("player", playerParam k)]
          case aw.awaiting of
            NoPasses _ -> do
              let aw' = aw {awaiting = OnePass k.next}
                  stack' = aw' : rest
              modify \gx ->
                gx {actionWindowStack = stack', actionWindow = Just aw'}
              maybeAutoPassPriority aw.trigger k.next
            OnePass _ ->
              -- Both players have now passed consecutively.
              send CloseActionWindow
        -- Invalid pass (no window open, or not the priority holder):
        -- silently ignore. The server should reject these at the
        -- protocol boundary; this is belt-and-braces.
        _ -> pure ()
    CloseActionWindow -> do
      g <- get
      let (closed, rest) = case g.actionWindowStack of
            (w : ws) -> (Just w, ws)
            [] -> (Nothing, [])
          trigger = (.trigger) <$> closed
      modify \gx ->
        gx
          { actionWindowStack = rest
          , actionWindow = case rest of
              (w : _) -> Just w
              [] -> Nothing
          }
      logIt LogSystem "log.window.close" []
      -- Combat sub-step windows advance to the next sub-step;
      -- begin-of-turn opens the Kingdom phase, end-of-turn drains
      -- pending effects and flips the turn; otherwise the
      -- top-of-stack phase window ends its phase.
      case trigger of
        Just AfterDeclareCombatTarget -> send AdvanceCombatToAttackers
        Just AfterDeclareAttackers -> send ResolveAmbushStep
        Just AfterDeclareDefenders -> send AdvanceCombatToAssign
        Just AfterAssignCombatDamage -> send AdvanceCombatToApply
        Just AfterApplyCombatDamage -> send EndCombat
        Just BeginningOfTurnActionWindow -> send (BeginPhase KingdomPhase)
        Just EndOfTurnActionWindow -> do
          -- After the end-of-turn window resolves: fire scheduled
          -- "at end of turn" effects, expire EndOfTurn-scoped
          -- modifiers, clear the phase, and hand off to the next
          -- player. Order matters: per the rulebook the window
          -- comes first, then the triggers, then the modifier
          -- cleanup.
          pending <- gets (.pendingEndOfTurn)
          modify \gx -> gx {pendingEndOfTurn = []}
          traverse_ firePendingEffect pending
          send $ ClearScopedModifiers EndOfTurn
          current <- gets (.currentPlayer)
          modify \gx -> gx
            { phase = Nothing
            , -- Turn-scoped riders expire with the turn.
              drawCaps = mempty
            , capitalShields = mempty
            , defenderCounterstrikeBonus = mempty
            , pendingFreeTactic = mempty
            , loyaltyWaivers = mempty
            , tacticDamageContext = Nothing
            , combatDamageUncancellable = False
            }
          send $ BeginTurn current.next
        _ -> case g.phase of
          Just p -> send $ EndPhase p
          Nothing -> pure ()
    Eliminate k reason -> do
      -- A player whose elimination is being processed loses immediately;
      -- the other player wins. If both somehow become eliminated, the
      -- first to be processed determines the winner.
      g <- get
      case g.lifecycle of
        GameFinished _ -> pure ()
        _ -> do
          let result = GameResult
                { winner = k.next
                , reason = case reason of
                    DeckedOut -> OpponentDeckedOut
                    CapitalBurned -> OpponentCapitalBurned
                }
          modify \gx -> gx {lifecycle = GameFinished result}
          logIt LogResult
            "log.player.eliminated"
            [("player", playerParam k), ("reason", elimReasonParam reason)]
          logIt LogResult
            "log.game.over"
            [ ("winner", playerParam result.winner)
            , ("reason", winReasonParam result.reason)
            ]
    -- Player-upkeep messages: the Player 'Run' instance has already
    -- carried out the actual mutation by the time we get here (Player
    -- runs before Game in 'distribute'). We only narrate them.
    ShuffleDeck k ->
      logIt LogSystem "log.deck.shuffled" [("player", playerParam k)]
    Draw drawing -> case drawing.kind of
      StartingHand ->
        logIt LogSystem
          "log.draw.opening"
          [("player", playerParam drawing.player)]
      StandardDraw -> do
        -- Mirror the cap check the Player handler applied (both run
        -- against the same pre-message history) so a swallowed draw
        -- neither narrates nor counts.
        g <- get
        let cap = Map.findWithDefault maxBound drawing.player g.drawCaps
            drawn =
              Map.findWithDefault 0 drawing.player
                (historyOfScope ThisTurn g).drawnBy
        if drawn < cap
          then do
            recordEvent \h -> h
              {drawnBy = Map.insertWith (+) drawing.player 1 h.drawnBy}
            logIt LogSystem
              "log.draw.card"
              [("player", playerParam drawing.player)]
          else
            logIt LogSystem
              "log.draw.capped"
              [("player", playerParam drawing.player)]
    ReturnResources k ->
      logIt LogSystem "log.resources.returned" [("player", playerParam k)]
    CollectResources k -> do
      g <- get
      let n = zonePower g k KingdomZone
          p = lookupPlayer k g
          p' = p {resources = Resources n}
      modify (setPlayer k p')
      logIt LogSystem
        "log.resources.collected"
        [("player", playerParam k), ("count", tshow n)]
    QuestDraw k -> do
      g <- get
      let n = zonePower g k QuestZone
      logIt LogSystem "log.quest.draw" [("player", playerParam k)]
      replicateM_ n (send (Draw (Drawing StandardDraw k)))
    DrawFromBottom k ->
      -- The hand/deck mutation happens in Player.receive; Game narrates.
      logIt LogSystem "log.draw.card" [("player", playerParam k)]
    PlayUnit pk cardKey zone -> do
      -- Server-side zone-entry gate: "Battlefield only."-style
      -- keywords and the Hero-per-zone limit are enforced here, not
      -- just in the client's zone picker.
      gEntry <- get
      let entryOk = case takeUnitFromHand cardKey (lookupPlayer pk gEntry) of
            Just (cd, _) -> canEnterZone gEntry pk cd zone
            Nothing -> False
      -- Reuse the card's existing key as its in-play UnitKey. This is
      -- what lets the frontend's view-transition map a hand card to its
      -- zone landing spot.
      when entryOk $ withPaidPlay
        pk
        (takeUnitFromHand cardKey)
        (\g cd -> max 0 (effectiveTotalCost g pk cd - pendingDiscountFor pk g))
        \cardDef paidPlayer n -> do
          g <- get
          let damageOnEnter = Map.findWithDefault 0 pk g.pendingUnitOnPlayDamage
              unit0 = freshUnit cardKey pk zone cardDef
              unit = unit0 {damage = Damage damageOnEnter} :: UnitDetails
          modify \gx -> (setPlayer pk paidPlayer gx)
            { units = unit : gx.units
            , pendingUnitDiscount = Map.insert pk 0 gx.pendingUnitDiscount
            , pendingUnitOnPlayDamage = Map.insert pk 0 gx.pendingUnitOnPlayDamage
            }
          recordEvent \h -> h
            {unitsPlayedBy = Map.insertWith (+) pk 1 h.unitsPlayedBy}
          logIt LogPlayerAction "log.unit.played"
            [ ("player", playerParam pk)
            , ("card", T.pack cardDef.title)
            , ("cost", tshow n)
            ]
          send $ UnitEnteredPlay pk cardKey
    PlayUnitOnQuest pk cardKey questKey -> do
      g <- get
      let player = lookupPlayer pk g
      case (takeUnitFromHand cardKey player, findQuest questKey g) of
        (Just (cardDef, playerWithoutCard), Just q)
          | q.controller == pk
          , q.questingUnit == Nothing
          , canEnterZone g pk cardDef QuestZone
          , canPlayCard pk cardDef g ->
              case cardDef.cost of
                Variable -> pure ()
                Fixed printed -> do
                  let n = effectiveUnitCost g pk cardDef printed
                  when (player.resources >= Resources n) $ do
                    markPlayedLimited cardDef
                    recordEvent \h -> h
                      { playedBy =
                          Map.insertWith (<>) pk [cardCodeFilter cardDef] h.playedBy
                      }
                    let paidPlayer =
                          playerWithoutCard
                            {resources = player.resources - Resources n}
                        unitDetails = freshUnit cardKey pk QuestZone cardDef
                        q' = (q {questingUnit = Just cardKey}) :: QuestDetails
                    modify \gx ->
                      (setPlayer pk paidPlayer gx)
                        { units = unitDetails : gx.units
                        , quests = replaceQuest q' gx.quests
                        }
                    logIt LogPlayerAction
                      "log.unit.played_on_quest"
                      [ ("player", playerParam pk)
                      , ("card", T.pack cardDef.title)
                      , ("quest", T.pack q.cardDef.title)
                      , ("cost", tshow n)
                      ]
                    send $ UnitEnteredPlay pk cardKey
        _ -> pure ()
    UnitEnteredPlay pk _key ->
      -- The card's own 'receive' fires via 'dispatchToInPlayUnits'.
      -- Game just narrates.
      logIt LogSystem "log.unit.entered_play" [("player", playerParam pk)]
    UnitAmbushed pk _key ->
      -- "When this unit ambushes" abilities fire via the card's own
      -- 'receive' ('onAmbush'); Game just narrates.
      logIt LogSystem "log.unit.ambushed" [("player", playerParam pk)]
    AssignUnitToQuest pk uKey qKey -> do
      g <- get
      case (findUnit uKey g, findQuest qKey g) of
        (Just u, Just q)
          | u.controller == pk
          , u.zone == QuestZone
          , q.controller == pk
          , q.questingUnit == Nothing -> do
              let q' = (q {questingUnit = Just uKey}) :: QuestDetails
              modify \gx -> gx {quests = replaceQuest q' gx.quests}
              logIt LogSystem
                "log.unit.assigned_to_quest"
                [ ("player", playerParam pk)
                , ("card", T.pack u.cardDef.title)
                , ("quest", T.pack q.cardDef.title)
                ]
        _ -> pure ()
    DealDamageToUnit ukey amount -> do
      g <- get
      whenJust (findUnit ukey g) \u -> do
          -- Tactic-sourced damage may be amplified by in-play
          -- supports (Hellcannon Reserves: +1 to each target while
          -- the controller's tactic resolves).
          let amplified = amplifyTacticDamage g (max 0 amount)
          -- Passive multiplier (Bloodletter doubles all damage) applies
          -- to the raw assignment; Toughness then cancels off the top,
          -- then per-turn caps (Daemonettes of Slaanesh) clip the
          -- remainder.
          let inflated = applyDamageMultipliers g amplified
              toughness = totalToughness g u
              afterToughness = max 0 (inflated - toughness)
              -- "Whenever this unit is assigned damage, cancel all
              -- but N of that damage." (Dragonmage.)
              afterPerHit = case (unitExtrasOf u).perHitDamageCap of
                Just cap -> min cap afterToughness
                Nothing -> afterToughness
              already =
                Map.findWithDefault 0 ukey
                  (historyOfScope ThisTurn g).damageTaken
              -- Full conditional immunity (Gustav the Bear): cancel
              -- everything that survived the keyword math.
              immune =
                (unitExtrasOf u).cancelAllDamageWhen g u
                  || any (\s -> s.cardDef.extras.grantsHostDamageImmunity g s u) u.attachments
              capped =
                if immune then 0 else applyPerTurnCap u already afterPerHit
          -- Damage shields and redirects installed as modifiers
          -- (Steel's Bane, Blessing of Valaya) consume next.
          afterShields <- consumeDamageShields ukey capped
          -- Consult the per-card pre-damage redirect slice. Returns
          -- the amount the card claims; the card's 'run' is expected
          -- to enqueue the redirected damage and mark whatever
          -- per-turn state stops it from re-firing.
          (landing, cancelled) <- case (unitExtrasOf u).preDamageRedirect g u afterShields of
            Just plan | plan.amount > 0 -> do
              let redirected = min plan.amount afterShields
                  remaining = afterShields - redirected
              -- Run the card's redirect plan with a synthetic ActionUsage.
              case plan.run of
                ActionEffect fire ->
                  fire ActionUsage
                    { user = u.controller
                    , self = u
                    , target = NoTarget
                    , payments = []
                    }
              -- Redirected points are re-dealt elsewhere, not
              -- cancelled — keep them out of the cancel log.
              pure (remaining, inflated - remaining - redirected)
            _ -> pure (afterShields, inflated - afterShields)
          when (cancelled > 0) $
            logIt LogSystem
              "log.damage.cancelled"
              [ ("card", T.pack u.cardDef.title)
              , ("amount", tshow cancelled)
              ]
          when (landing > 0) $ do
            let Damage existing = u.damage
                newDmg = Damage (existing + landing)
                u' = u {damage = newDmg} :: UnitDetails
            modify \gx -> gx {units = replaceUnit u' gx.units}
            recordEvent \h -> h
              { damageTaken = Map.insertWith (+) ukey landing h.damageTaken
              , damagedUnits =
                  if ukey `elem` h.damagedUnits
                    then h.damagedUnits
                    else ukey : h.damagedUnits
              }
            logIt LogSystem
              "log.unit.damaged"
              [ ("card", T.pack u.cardDef.title)
              , ("amount", tshow landing)
              ]
            let Damage total = newDmg
            when (total >= u.effectiveMaxHP) $
              send $ DestroyUnit ukey
    DealDamageToUnitUncancellable ukey amount -> do
      g <- get
      whenJust (findUnit ukey g) \u -> do
          -- Uncancellable damage still respects per-turn caps
          -- (Daemonettes) — the cap is independent of cancellation.
          let inflated =
                applyDamageMultipliers g (amplifyTacticDamage g (max 0 amount))
              already =
                Map.findWithDefault 0 ukey
                  (historyOfScope ThisTurn g).damageTaken
              landing = applyPerTurnCap u already inflated
          when (landing > 0) $ do
            let Damage existing = u.damage
                newDmg = Damage (existing + landing)
                u' = u {damage = newDmg} :: UnitDetails
            modify \gx -> gx {units = replaceUnit u' gx.units}
            recordEvent \h -> h
              { damageTaken = Map.insertWith (+) ukey landing h.damageTaken
              , damagedUnits =
                  if ukey `elem` h.damagedUnits
                    then h.damagedUnits
                    else ukey : h.damagedUnits
              }
            logIt LogSystem
              "log.unit.damaged"
              [ ("card", T.pack u.cardDef.title)
              , ("amount", tshow landing)
              ]
            let Damage total = newDmg
            when (total >= u.effectiveMaxHP) $
              send $ DestroyUnit ukey
    HealUnit ukey amount -> do
      g <- get
      whenJust (findUnit ukey g) \u -> do
          let Damage existing = u.damage
              healed = max 0 (existing - max 0 amount)
              u' = (u {damage = Damage healed}) :: UnitDetails
          modify \gx -> gx {units = replaceUnit u' gx.units}
          logIt LogSystem
            "log.unit.healed"
            [ ("card", T.pack u.cardDef.title)
            , ("amount", tshow amount)
            ]
    DestroyUnit ukey -> do
      munit <- gets (findUnit ukey)
      whenJust munit \u -> do
          g <- get
          -- Hydra Blade: an attachment may ransom the host out of
          -- destruction entirely (pay N, heal all, stay in play).
          ransomed <- ransomHostIfPossible u
          unless ransomed do
            let departed = DepartedUnit
                  { key = ukey
                  , controller = u.controller
                  , zone = u.zone
                  , cardDef = u.cardDef
                  }
            case (unitExtrasOf u).destroyedToZone g u of
              -- Vigilant Pistoliers: destruction replacement — the
              -- card re-enters play in the named zone instead of
              -- hitting the discard pile. Attachments still fall off
              -- and leave-play hooks still fire.
              Just newZone -> do
                for_ u.attachments discardAttachment
                modify \gx -> gx
                  { units =
                      freshUnit ukey u.controller newZone u.cardDef
                        : removeById ukey gx.units
                  }
                logIt LogSystem
                  "log.unit.relocated_on_destroy"
                  [ ("card", T.pack u.cardDef.title)
                  , ("zone", zoneParam newZone)
                  ]
                send $ UnitLeftPlay departed
                send $ UnitEnteredPlay u.controller ukey
              Nothing
                -- Bolt of Change: an animated development reverts to
                -- being a development; destroying the unit destroys
                -- one development from its zone instead of routing a
                -- (nonexistent) card to the discard pile.
                | u.cardDef.code == animatedDevelopmentCode -> do
                    modify \gx -> gx {units = removeById ukey gx.units}
                    logIt LogSystem
                      "log.unit.destroyed"
                      [ ("player", playerParam u.controller)
                      , ("card", T.pack u.cardDef.title)
                      ]
                    send $ DestroyDevelopment u.controller u.zone
                    send $ UnitLeftPlay departed
                | otherwise -> do
                    -- Remove the unit and all its attachments. Each
                    -- card lands in its OWN controller's discard pile
                    -- carrying the same key it had in play, so the
                    -- frontend's view-transition continues to track
                    -- the same card visually from board to pile.
                    --
                    -- Attachments may be controlled by either side —
                    -- Branded by Khorne is the canonical hostile
                    -- attachment.
                    discardToController u.controller $ mkCard u.key (UnitCardDef u.cardDef)
                    for_ u.attachments discardAttachment
                    modify \gx -> gx {units = removeById ukey gx.units}
                    recordEvent \h -> h
                      { unitsDiscarded = h.unitsDiscarded + 1
                      , discardedUnitsBy =
                          Map.insertWith (<>) u.controller [ukey] h.discardedUnitsBy
                      }
                    logIt LogSystem
                      "log.unit.destroyed"
                      [ ("player", playerParam u.controller)
                      , ("card", T.pack u.cardDef.title)
                      ]
                    send $ UnitLeftPlay departed
    UnitLeftPlay du -> do
      let ukey = du.key
      -- If the departed unit was questing on a quest, clear the slot
      -- and dump accumulated resource tokens.
      g <- get
      let touched =
            [ (q {questingUnit = Nothing, tokens = 0}) :: QuestDetails
            | q <- g.quests
            , q.questingUnit == Just ukey
            ]
      unless (null touched) $ do
        let updated = foldr replaceQuest g.quests touched
        modify \gx -> gx {quests = updated}
        traverse_
          ( \q ->
              logIt LogSystem
                "log.quest.unit_left"
                [("quest", T.pack q.cardDef.title)]
          )
          touched
      -- Pure hook point beyond the questing-slot bookkeeping:
      -- 'dispatchToInPlayUnits' runs cards' bespoke reactions; Game
      -- itself has nothing more to do.
      pure ()
    CorruptUnit ukey -> do
      g <- get
      unless (hasModifier g.modifiers ukey CannotBeCorrupted) $
        setCorrupted True "log.unit.corrupted" ukey
    CleanseUnit ukey -> setCorrupted False "log.unit.cleansed" ukey
    RestoreOneCorruptCard pk -> do
      -- Kingdom phase, step 2: the active player MAY restore one of
      -- their corrupt cards. Prompt them to pick (or skip) instead
      -- of auto-restoring the first one we find.
      g <- get
      let corrupt =
            [ u.key
            | u <- g.units
            , u.controller == pk
            , u.corrupted
            ]
              <> [ s.key
                 | s <- allInPlaySupports g
                 , s.controller == pk
                 , s.corrupted
                 ]
      case corrupt of
        [] -> pure ()
        candidates -> do
          ans <- askPrompt Prompt
            { player = pk
            , kind = ChooseUnits
                { filterSpec = UnitsFromList candidates
                , minPick = 0
                , maxPick = 1
                , description = "Restore one corrupt card (or skip)."
                }
            , callback = CallbackInlinePrompt
            }
          case ans of
            PickUnits (chosen : _) | chosen `elem` candidates ->
              send $ CleanseUnit chosen
            _ -> pure ()
    PlayAttachment pk cardKey targetKey -> do
      g <- get
      mhost <- gets (findUnit targetKey)
      whenJust mhost \host -> do
        let player = lookupPlayer pk g
        whenJust (takeSupportFromHand cardKey player) \(cardDef, playerWithoutCard) -> do
          let windowOk =
                canPlayNonTactic pk g
                  || (PlayAnytime `elem` cardDef.keywords && canPlayTactic pk g)
          when (windowOk && canPlayCard pk cardDef g) case cardDef.cost of
            Variable -> pure ()
            Fixed _ -> do
              let n =
                    effectiveTotalCost g pk cardDef
                      + extraTargetTax pk (TargetUnit targetKey) g
                  Resources have = player.resources
                  -- Dat's Mine!: resource tokens on the controller's
                  -- pays-for-attachments quests stretch the budget.
                  -- They drain first so the pool stays liquid.
                  tokenQuests =
                    [ q
                    | q <- g.quests
                    , q.controller == pk
                    , q.cardDef.extras.paysForAttachments
                    ]
                  tokensAvail = sum (map (.tokens) tokenQuests)
              when (have + tokensAvail >= n) do
                markPlayedLimited cardDef
                recordEvent \h -> h
                  { playedBy =
                      Map.insertWith (<>) pk [cardCodeFilter cardDef] h.playedBy
                  }
                let fromTokens = min n tokensAvail
                    fromPool = n - fromTokens
                    paidPlayer =
                      playerWithoutCard {resources = Resources (have - fromPool)}
                    drain _ 0 = pure ()
                    drain [] _ = pure ()
                    drain (q : qs) k = do
                      let t = min q.tokens k
                      when (t > 0) $ send (AdjustQuestTokens q.key (negate t))
                      drain qs (k - t)
                drain tokenQuests fromTokens
                let attachment = freshSupport cardKey pk host.zone (Just targetKey) cardDef
                    host' = (host {attachments = attachment : host.attachments}) :: UnitDetails
                modify \gx -> (setPlayer pk paidPlayer gx) {units = replaceUnit host' gx.units}
                logIt LogPlayerAction "log.attachment.played"
                  [ ("player", playerParam pk)
                  , ("card", T.pack cardDef.title)
                  , ("target", T.pack host.cardDef.title)
                  , ("cost", tshow n)
                  ]
                send $ SupportEnteredPlay pk cardKey
    SupportEnteredPlay _pk _key ->
      -- 'dispatchToInPlayUnits' walks attachments via their host; any
      -- bespoke reaction lives in the support card's 'receive'.
      pure ()
    PlaySupport pk cardKey zone -> do
      gEntry <- get
      let entryOk = case takeSupportFromHand cardKey (lookupPlayer pk gEntry) of
            Just (cd, _) -> canEnterZone gEntry pk cd zone
            Nothing -> False
      when entryOk $ withPaidPlay pk (takeSupportFromHand cardKey) (\g cd -> effectiveTotalCost g pk cd)
        \cardDef paidPlayer n -> do
          let support = freshSupport cardKey pk zone Nothing cardDef
          modify \gx -> (setPlayer pk paidPlayer gx) {supports = support : gx.supports}
          recordEvent \h -> h
            {supportsPlayedBy = Map.insertWith (+) pk 1 h.supportsPlayedBy}
          logIt LogPlayerAction "log.support.played"
            [ ("player", playerParam pk)
            , ("card", T.pack cardDef.title)
            , ("cost", tshow n)
            ]
          send $ SupportEnteredPlay pk cardKey
    PlaySupportFromDeck pk cardKey zone -> do
      g <- get
      player <- getPlayerS pk
      whenJust (takeSupportFromDeck cardKey player) \(cardDef, playerWithoutCard) ->
        when (canEnterZone g pk cardDef zone) do
          let support = freshSupport cardKey pk zone Nothing cardDef
          modify \gx -> (setPlayer pk playerWithoutCard gx) {supports = support : gx.supports}
          logIt LogSystem "log.support.played_from_deck"
            [("player", playerParam pk), ("card", T.pack cardDef.title)]
          send $ SupportEnteredPlay pk cardKey
    PlayQuest pk cardKey ->
      withPaidPlay pk (takeQuestFromHand cardKey) (\g cd -> effectiveTotalCost g pk cd)
        \cardDef paidPlayer n -> do
          let hostPlayer
                | PlayInOpponentArea `elem` cardDef.keywords = pk.next
                | otherwise = pk
              quest = QuestDetails
                { key = cardKey
                , controller = pk
                , zoneOwner = hostPlayer
                , cardDef
                , tokens = 0
                , questingUnit = Nothing
                }
          modify \gx -> (setPlayer pk paidPlayer gx) {quests = quest : gx.quests}
          logIt LogPlayerAction "log.quest.played"
            [ ("player", playerParam pk)
            , ("card", T.pack cardDef.title)
            , ("cost", tshow n)
            ]
          send $ QuestEnteredPlay pk cardKey
    QuestEnteredPlay _pk _key ->
      -- Per-card reactions fire via dispatch (see
      -- 'dispatchToInPlayUnits' which now also walks 'Game.supports'
      -- and 'Game.quests').
      pure ()
    AdjustSupportTokens skey delta -> do
      g <- get
      case findSupport skey g of
        Just s -> do
          let n = max 0 (s.tokens + delta)
              s' = (s {tokens = n}) :: SupportDetails
          modify \gx -> gx {supports = replaceSupport s' gx.supports}
          logIt LogSystem
            "log.support.tokens"
            [ ("card", T.pack s.cardDef.title)
            , ("count", tshow n)
            ]
        Nothing -> do
          -- Attached supports carry tokens too (War Crown of Saphery,
          -- Fellblade); they live inside their host's 'attachments'.
          let hosts =
                [ (u, a)
                | u <- g.units
                , a <- u.attachments
                , a.key == skey
                ]
          whenJust (listToMaybe hosts) \(host, a) -> do
            let n = max 0 (a.tokens + delta)
                a' = (a {tokens = n}) :: SupportDetails
                host' =
                  (host {attachments = replaceSupport a' host.attachments})
                    :: UnitDetails
            modify \gx -> gx {units = replaceUnit host' gx.units}
            logIt LogSystem
              "log.support.tokens"
              [ ("card", T.pack a.cardDef.title)
              , ("count", tshow n)
              ]
    AdjustQuestTokens qkey delta -> do
      g <- get
      whenJust (findQuest qkey g) \q -> do
          let n = max 0 (q.tokens + delta)
              q' = (q {tokens = n}) :: QuestDetails
          modify \gx -> gx {quests = replaceQuest q' gx.quests}
          logIt LogSystem
            "log.quest.tokens"
            [ ("card", T.pack q.cardDef.title)
            , ("count", tshow n)
            ]
    DestroySupport skey -> do
      msupport <- gets (findSupport skey)
      case msupport of
        Just s -> do
          discardToController s.controller $ mkCard s.key (SupportCardDef s.cardDef)
          modify \gx -> gx {supports = removeById skey gx.supports}
          logIt LogSystem "log.support.destroyed"
            [("player", playerParam s.controller), ("card", T.pack s.cardDef.title)]
          send $ SupportLeftPlay s.controller skey s.cardDef.code
        Nothing -> do
          -- Attached supports live inside their host unit's
          -- 'attachments'; "destroy one target support card" can hit
          -- them too.
          g <- get
          let hosts =
                [ (u, a)
                | u <- g.units
                , a <- u.attachments
                , a.key == skey
                ]
          whenJust (listToMaybe hosts) \(host, a) -> do
            discardAttachment a
            let host' =
                  (host {attachments = filter ((/= skey) . (.key)) host.attachments})
                    :: UnitDetails
            modify \gx -> gx {units = replaceUnit host' gx.units}
            logIt LogSystem "log.support.destroyed"
              [("player", playerParam a.controller), ("card", T.pack a.cardDef.title)]
            send $ SupportLeftPlay a.controller skey a.cardDef.code
    SupportLeftPlay _pk _skey _code -> pure ()
    DestroyQuest qkey -> do
      mquest <- gets (findQuest qkey)
      whenJust mquest \q -> do
        discardToController q.controller $ mkCard q.key (QuestCardDef q.cardDef)
        modify \gx -> gx {quests = removeById qkey gx.quests}
        logIt LogSystem "log.quest.destroyed"
          [("player", playerParam q.controller), ("card", T.pack q.cardDef.title)]
        send $ QuestLeftPlay q.controller qkey q.cardDef.code
    QuestLeftPlay _pk _qkey _code -> pure ()
    AttachExperience hostKey expCode -> do
      g <- get
      whenJust (findUnit hostKey g) \u -> do
          let u' = (u {experiences = expCode : u.experiences}) :: UnitDetails
          modify \gx -> gx {units = replaceUnit u' gx.units}
          logIt LogSystem
            "log.unit.experience_attached"
            [ ("card", T.pack u.cardDef.title)
            , ("count", tshow (length u'.experiences))
            ]
    PlayTactic pk cardKey target -> do
      g <- get
      let player = lookupPlayer pk g
      case takeTacticFromHand cardKey player of
        Nothing -> pure ()
        Just (cardDef, playerWithoutCard)
          | not (canPlayTactic pk g) ->
              -- Tactics only fire when the player has priority in an
              -- open action window.
              pure ()
          | not (canPlayCard pk cardDef g) ->
              -- Limited already played this turn (tactics never trip
              -- the uniqueness check because they don't persist).
              pure ()
          | not (validateTarget pk (tacticTargetSchema cardDef) target g) ->
              pure ()
          | otherwise -> do
              let loyaltyCost = loyaltySurcharge g pk cardDef
                  Resources rNow = player.resources
                  targetTax = extraTargetTax pk target g
              x <- case cardDef.cost of
                Fixed n -> pure (max 0 (n + printedCostAdjustment g pk cardDef))
                Variable -> do
                  -- Prompt the controller for an X in [0, available
                  -- resources minus loyalty surcharge minus target tax].
                  let cap = max 0 (rNow - loyaltyCost - targetTax)
                  answer <- askPrompt Prompt
                    { player = pk
                    , kind = ChooseAmount
                        { minAmount = 0
                        , maxAmount = cap
                        , description =
                            "Pick X for " <> T.pack cardDef.title <> "."
                        }
                    , callback = CallbackInlinePrompt
                    }
                  pure $ case answer of
                    PickAmount k -> max 0 (min cap k)
                    _ -> 0
              -- Runefang of Solland: the next non-Epic fixed-cost
              -- tactic this turn has its printed cost lowered to 0
              -- (loyalty surcharge and target taxes still apply).
              let useFree =
                    Map.findWithDefault 0 pk g.pendingFreeTactic > 0
                      && Epic `notElem` cardDef.traits
                      && case cardDef.cost of
                        Fixed _ -> True
                        Variable -> False
                  xPaid = if useFree then 0 else x
                  n = xPaid + loyaltyCost + targetTax
              when (rNow >= n) $ do
                markPlayedLimited cardDef
                when useFree do
                  modify \gx -> gx
                    { pendingFreeTactic =
                        Map.adjust (subtract 1) pk gx.pendingFreeTactic
                    }
                  logIt LogSystem
                    "log.tactic.free"
                    [("player", playerParam pk), ("card", T.pack cardDef.title)]
                recordEvent \h -> h
                  { playedBy =
                      Map.insertWith (<>) pk [cardCodeFilter cardDef] h.playedBy
                  }
                let paidPlayer =
                      playerWithoutCard
                        { resources = Resources (rNow - n)
                        , discard =
                            mkCard cardKey (TacticCardDef cardDef)
                              : playerWithoutCard.discard
                        }
                modify (setPlayer pk paidPlayer)
                logIt LogPlayerAction
                  "log.tactic.played"
                  [ ("player", playerParam pk)
                  , ("card", T.pack cardDef.title)
                  , ("cost", tshow n)
                  ]
                send $ TacticResolved pk cardDef.code target x
    TacticResolved pk code target xVal -> do
      g <- get
      case Map.lookup code allCards of
        Just (TacticCardDef cardDef) -> do
          let ctx = TacticContext
                { controller = pk
                , cardDef
                , xValue = xVal
                }
              owner = lookupPlayer pk g
          -- Open the tactic-damage context: damage messages the body
          -- queues drain while it's set, so Hellcannon Reserves can
          -- amplify them. The sentinel pushed after the body lands
          -- behind those messages (FIFO) and closes the window.
          modify \gx -> gx {tacticDamageContext = Just pk}
          case cardDef.receive of
            Receive f -> f (TacticResolved pk code target xVal) owner ctx
          send ClearTacticDamageContext
          -- Remember the resolved tactic so Twin-Tailed Comet can
          -- target it. Done after the body fires so the body sees
          -- its own pre-state if it queries.
          modify \gx -> gx
            {lastResolvedTactic = Just (code, target, xVal)}
        _ -> pure ()
    ClearTacticDamageContext ->
      modify \gx -> gx {tacticDamageContext = Nothing}
    RequestPrompt p -> do
      modify \g -> g {pendingPrompt = Just p}
      logIt LogSystem
        "log.prompt.opened"
        [("player", playerParam p.player)]
    ResolvePrompt result -> do
      g <- get
      case g.pendingPrompt of
        Nothing -> pure ()
        Just p -> do
          modify \gx -> gx {pendingPrompt = Nothing}
          logIt LogSystem
            "log.prompt.resolved"
            [("player", playerParam p.player)]
          dispatchPromptCallback p.callback result
    TriggerCardAction pk srcKey idx target -> do
      g <- get
      case findActionSource srcKey g of
        Nothing -> pure ()
        Just src -> case actionAt src idx of
          Nothing -> pure ()
          Just (name, baseCost, schema) -> do
            let player = lookupPlayer pk g
                tax = extraTargetTax pk target g
                totalCost = baseCost + tax
                extras = actionExtraCostsAt src idx
                -- Witch Hag's Curse: a blanked unit's printed actions
                -- don't exist.
                sourceBlanked = case src of
                  UnitSource u -> u.blanked
                  _ -> False
            when (not sourceBlanked) $
              when (validateActionTriggerer pk src idx) $
              when (actionAvailableHere src idx) $
                when (player.resources >= Resources totalCost) $
                  when (validateTarget pk schema target g) $
                    when (canPayExtras pk srcKey extras g) $ do
                      let paid = player {resources = player.resources - Resources totalCost}
                      modify (setPlayer pk paid)
                      logIt LogPlayerAction
                        "log.action.triggered"
                        [ ("player", playerParam pk)
                        , ("card", T.pack (actionSourceTitle src))
                        , ("action", name)
                        , ("cost", tshow totalCost)
                        ]
                      mpayments <- payExtras pk srcKey extras
                      -- If a pending action-cancel token exists for
                      -- this player (Bright Wizard Apprentice), pop
                      -- one and skip the effect. Cost is still paid;
                      -- only the body is suppressed.
                      g' <- get
                      let cancelled = Map.findWithDefault 0 pk g'.pendingActionCancel > 0
                      when cancelled do
                        modify \gx -> gx
                          { pendingActionCancel =
                              Map.adjust (subtract 1) pk gx.pendingActionCancel
                          }
                        logIt LogSystem
                          "log.action.cancelled"
                          [("player", playerParam pk)]
                      whenJust mpayments \payments ->
                        unless cancelled $
                          fireAction src idx pk target payments
    DeferDamageToUnitUntilEoT ukey n -> do
      modify \g ->
        g {pendingEndOfTurn = PEDealDamageToUnit ukey n : g.pendingEndOfTurn}
      logIt LogSystem
        "log.effect.deferred_damage"
        [ ("amount", tshow n)
        , ("trigger", "EndTurn")
        ]
    DeferSacrificeUntilEoT ukey -> do
      modify \g ->
        g {pendingEndOfTurn = PEDestroyUnit ukey : g.pendingEndOfTurn}
    DealDamageToZone targetPlayer zoneKind raw -> do
      g0 <- get
      let -- Tactic amplification (Hellcannon Reserves), then capital
          -- doublers (Basha's Bloodaxe while its host attacks this
          -- player).
          amplified = amplifyTacticDamage g0 (max 0 raw)
          doubling =
            any
              (\s -> s.cardDef.extras.capitalDamageDoubler g0 s targetPlayer)
              (allInPlaySupports g0)
          raised = if doubling then amplified * 2 else amplified
          targetZone0 = getZone zoneKind (lookupPlayer targetPlayer g0)
      if targetZone0.burned
        then
          -- FAQ: "A burning zone still functions normally except that
          -- it cannot be assigned damage." Damage aimed at a burned
          -- section is simply wasted.
          when (raised > 0) $
            logIt LogSystem
              "log.zone.damage_wasted"
              [ ("player", playerParam targetPlayer)
              , ("zone", zoneParam zoneKind)
              , ("amount", tshow raised)
              ]
        else do
          -- Armed shield grants (Flagellants, Gifts of Aenarion)
          -- cancel first — possibly refunding resources — then the
          -- once-per-turn capital defenses, evaluated live at damage
          -- time so they protect on the opponent's turn too:
          --   1. redirect-first-damage quests (Defend the Border),
          --   2. cancel-1 supports (Contested Fortress).
          afterGrants <- consumeCapitalShieldGrants targetPlayer raised
          afterRedirect <- applyCapitalRedirects targetPlayer zoneKind afterGrants
          amount <- applyCapitalShields targetPlayer zoneKind afterRedirect
          when (amount > 0) $ do
            g <- get
            -- Add damage; burn if it now meets or exceeds HP, and if
            -- that's the second burn on this capital eliminate the
            -- player.
            let target = lookupPlayer targetPlayer g
                zoneL = getZone zoneKind target
                Damage existing = zoneL.damage
                HitPoints zoneHp = zoneL.hitPoints
                total = existing + amount
                (newDmg, justBurned) =
                  if total >= zoneHp && not zoneL.burned
                    then (Damage 0, True)
                    else (Damage total, False)
                zoneL' =
                  zoneL
                    { damage = newDmg
                    , burned = zoneL.burned || justBurned
                    }
                target' = setZone zoneKind zoneL' target
            modify (setPlayer targetPlayer target')
            logIt LogSystem
              "log.zone.damaged"
              [ ("player", playerParam targetPlayer)
              , ("zone", zoneParam zoneKind)
              , ("amount", tshow amount)
              ]
            -- Get 'Em Ladz!: each player watching this zone draws a card
            -- per point of damage that just landed.
            watchers <- gets (.zoneDamageDrawWatchers)
            for_ watchers \(watcher, owner, z) ->
              when (owner == targetPlayer && z == zoneKind) $
                replicateM_ amount (send (Draw (Drawing StandardDraw watcher)))
            when justBurned $ do
              logIt LogResult
                "log.zone.burned"
                [ ("player", playerParam targetPlayer)
                , ("zone", zoneParam zoneKind)
                ]
              -- Check for elimination (two burned zones = lose).
              let burnedNow = burnedZoneCount target'.capital
              when (burnedNow >= 2) $
                send $ Eliminate targetPlayer CapitalBurned
    HealCapital pk raw -> do
      -- Heal up to N total damage tokens off the capital, spending the
      -- budget greedily on the most-damaged unburned zone, then the
      -- next, and so on. Burned zones are not healed.
      let budget = max 0 raw
      when (budget > 0) $ do
        g <- get
        let player0 = lookupPlayer pk g
            step p b
              | b <= 0 = p
              | otherwise =
                  let candidates =
                        [ (d, z)
                        | z <- [KingdomZone, QuestZone, BattlefieldZone]
                        , let zL = getZone z p
                        , not zL.burned
                        , let Damage d = zL.damage
                        , d > 0
                        ]
                   in case candidates of
                        [] -> p
                        _ ->
                          let (dMost, zMost) = maximum candidates
                              taken = min b dMost
                              zL = getZone zMost p
                              zL' = (zL {damage = Damage (dMost - taken)}) :: Zone
                           in step (setZone zMost zL' p) (b - taken)
            player' = step player0 budget
        modify (setPlayer pk player')
        logIt LogSystem
          "log.capital.healed"
          [("player", playerParam pk), ("amount", tshow budget)]
    HealZone pk zone raw -> do
      let amount = max 0 raw
      when (amount > 0) $ do
        g <- get
        let player = lookupPlayer pk g
            zoneL = getZone zone player
            Damage d = zoneL.damage
            taken = min amount d
        when (taken > 0) $ do
          let zoneL' = (zoneL {damage = Damage (d - taken)}) :: Zone
              player' = setZone zone zoneL' player
          modify (setPlayer pk player')
          logIt LogSystem
            "log.zone.healed"
            [ ("player", playerParam pk)
            , ("zone", zoneParam zone)
            , ("amount", tshow taken)
            ]
    AddDevelopment pk zone -> do
      g <- get
      let player = lookupPlayer pk g
      case player.deck of
        [] -> pure ()
        (topCard : restDeck) -> do
          let zoneL = getZone zone player
              Developments d = zoneL.developments
              zoneL' = (zoneL {developments = Developments (d + 1)}) :: Zone
              existing = Map.findWithDefault [] zone player.developmentCards
              player' =
                (setZone zone zoneL' player)
                  { deck = restDeck
                  , developmentCards =
                      Map.insert zone (topCard : existing) player.developmentCards
                  }
          modify (setPlayer pk player')
          logIt LogSystem
            "log.zone.development_added"
            [("player", playerParam pk), ("zone", zoneParam zone)]
          -- Decking-out check: AddDevelopment can empty the deck too.
          when (null restDeck) $
            send $ Eliminate pk DeckedOut
    PlayDevelopment pk cardKey zone -> do
      g <- get
      let player = lookupPlayer pk g
          alreadyPlayed = g.developmentPlayedThisTurn
      -- Gate: Capital phase + active player + own priority + not
      -- already played a development this turn. Find the card in
      -- hand (any kind is legal — the card goes face-down).
      when (canPlayNonTactic pk g && not alreadyPlayed) $
        case find (\c -> c.key == cardKey) player.hand of
          Nothing -> pure ()
          Just card -> do
            let newHand = filter (\c -> c.key /= cardKey) player.hand
                zoneL = getZone zone player
                Developments d = zoneL.developments
                zoneL' = (zoneL {developments = Developments (d + 1)}) :: Zone
                existing = Map.findWithDefault [] zone player.developmentCards
                player' =
                  (setZone zone zoneL' player)
                    { hand = newHand
                    , developmentCards =
                        Map.insert zone (card : existing) player.developmentCards
                    }
            modify \gx ->
              (setPlayer pk player' gx) {developmentPlayedThisTurn = True}
            logIt LogPlayerAction
              "log.zone.development_played"
              [("player", playerParam pk), ("zone", zoneParam zone)]
    WatchZoneForDamageDraw watcher owner zone ->
      modify \gx ->
        gx
          { zoneDamageDrawWatchers =
              (watcher, owner, zone) : gx.zoneDamageDrawWatchers
          }
    RevealCards pk cards -> do
      modify \gx -> gx {lastRevealed = cards}
      logIt LogSystem
        "log.reveal.cards"
        [("player", playerParam pk), ("count", tshow (length cards))]
    MoveTopToBottomOfDeck pk n -> do
      g <- get
      let player = lookupPlayer pk g
          (taken, rest) = splitAt (max 0 n) player.deck
      unless (null taken) $
        modify (setPlayer pk player {deck = rest <> taken})
    TurnUnitIntoDevelopment ukey -> do
      g <- get
      whenJust (findUnit ukey g) \u -> do
        let pk = u.controller
            zone = u.zone
            player = lookupPlayer pk g
            zoneL = getZone zone player
            Developments d = zoneL.developments
            zoneL' = (zoneL {developments = Developments (d + 1)}) :: Zone
            existing = Map.findWithDefault [] zone player.developmentCards
            player' =
              (setZone zone zoneL' player)
                { developmentCards =
                    Map.insert
                      zone
                      (mkCard ukey (UnitCardDef u.cardDef) : existing)
                      player.developmentCards
                }
        modify \gx -> (setPlayer pk player' gx) {units = removeById ukey gx.units}
        logIt LogSystem
          "log.unit.became_development"
          [("player", playerParam pk), ("zone", zoneParam zone)]
    PlaceTopAsDevelopments pk zone n -> do
      g <- get
      let player = lookupPlayer pk g
          (taken, restDeck) = splitAt n player.deck
          zoneL = getZone zone player
          Developments d = zoneL.developments
          zoneL' = (zoneL {developments = Developments (d + length taken)}) :: Zone
          existing = Map.findWithDefault [] zone player.developmentCards
          player' =
            (setZone zone zoneL' player)
              { deck = restDeck
              , developmentCards = Map.insert zone (taken ++ existing) player.developmentCards
              }
      unless (null taken) $ do
        modify (setPlayer pk player')
        logIt LogSystem
          "log.zone.development_played"
          [("player", playerParam pk), ("zone", zoneParam zone)]
    DealDamageToEachEnemyUnitInZone pk zone raw -> do
      let amount = max 0 raw
      when (amount > 0) $ do
        g <- get
        let targets =
              [ u.key
              | u <- g.units
              , u.controller /= pk
              , u.zone == zone
              ]
        traverse_ (\k -> send (DealDamageToUnit k amount)) targets
    DealDamageToEachUnitInCombat amount -> do
      g <- get
      case g.combat of
        Nothing -> pure ()
        Just cs ->
          traverse_ (\k -> send (DealDamageToUnit k (max 0 amount))) (cs.attackers <> cs.defenders)
    CancelAssignedDamageOnUnit ukey raw -> do
      let cap = max 0 raw
      modify \gx -> case gx.combat of
        Nothing -> gx
        Just cs ->
          let pa' =
                map
                  ( \pd -> case pd.target of
                      PDUnit k
                        | k == ukey ->
                            pd {cancellable = max 0 (pd.cancellable - cap)}
                      _ -> pd
                  )
                  cs.pendingAssignments
              cs' = (cs {pendingAssignments = pa'}) :: CombatState
           in gx {combat = Just cs'}
      logIt LogSystem
        "log.damage.cancelled_pending"
        [("amount", tshow cap)]
    CancelAllAssignedDamage -> do
      modify \gx -> case gx.combat of
        Nothing -> gx
        Just cs ->
          let cs' = (cs {pendingAssignments = []}) :: CombatState
           in gx {combat = Just cs'}
      logIt LogSystem "log.damage.cancelled_all" []
    DiscardRandomFromHand pk -> do
      g <- get
      let player = lookupPlayer pk g
      case player.hand of
        [] -> pure ()
        cards -> do
          idx <- getRandomR (0, length cards - 1)
          let (before, after) = splitAt idx cards
          case after of
            (picked : rest) -> do
              let player' =
                    player
                      { hand = before <> rest
                      , discard = picked : player.discard
                      }
              modify (setPlayer pk player')
              logIt LogSystem
                "log.hand.discarded"
                [("player", playerParam pk)]
            [] -> pure ()
    DiscardRandomForResources pk -> do
      g <- get
      let player = lookupPlayer pk g
      case player.hand of
        [] -> pure ()
        cards -> do
          idx <- getRandomR (0, length cards - 1)
          let (before, after) = splitAt idx cards
          case after of
            (picked : rest) -> do
              let player' =
                    player
                      { hand = before <> rest
                      , discard = picked : player.discard
                      }
              modify (setPlayer pk player')
              logIt LogSystem
                "log.hand.discarded"
                [("player", playerParam pk)]
              send $ GainResources pk (someCardCost picked.def)
            [] -> pure ()
    GainResources pk raw -> do
      let amount = max 0 raw
      when (amount > 0) $ do
        g <- get
        let player = lookupPlayer pk g
            Resources r = player.resources
            player' = player {resources = Resources (r + amount)}
        modify (setPlayer pk player')
        logIt LogSystem
          "log.resources.gained"
          [("player", playerParam pk), ("amount", tshow amount)]
    GrantLoyaltyWaiver pk race -> do
      g <- get
      let snapshot = racePlaysThisTurn g pk race
      modify \gx -> gx
        { loyaltyWaivers =
            Map.insertWith (<>) pk [(race, snapshot)] gx.loyaltyWaivers
        }
      logIt LogSystem
        "log.loyalty.waived"
        [("player", playerParam pk), ("race", tshow race)]
    SpendResources pk raw -> do
      let amount = max 0 raw
      when (amount > 0) $ do
        g <- get
        let player = lookupPlayer pk g
            Resources r = player.resources
            spent = min amount r
            player' = player {resources = Resources (r - spent)}
        modify (setPlayer pk player')
        logIt LogSystem
          "log.resources.spent"
          [("player", playerParam pk), ("amount", tshow spent)]
    BeginCombat attacker zone attackerKeys -> do
      g <- get
      let defender = attacker.next
          -- Filter attackers by per-card eligibility (Sworn of Khorne,
          -- corruption gating, etc.) before committing the combat.
          eligible = filter (eligibleAttacker g defender zone) attackerKeys
          attackBlocked = attacker `elem` g.attackBlockedThisTurn
      if null eligible || attackBlocked
        then do
          logIt LogSystem
            "log.combat.aborted"
            [("attacker", playerParam attacker)]
        else do
          -- Rune-of-Fortitude family: each support whose
          -- 'runeOfFortitudeTax' slice is set, sitting in the
          -- defender's same zone, imposes the 1-per-attacker tax.
          -- All-or-nothing approximation: if the attacker can
          -- afford the full tax, pay it and the penalty stays at
          -- 0; otherwise leave resources intact and impose -1
          -- per attacker for this combat.
          let runeHere =
                any
                  ( \s ->
                      s.controller == defender
                        && s.zone == zone
                        && s.cardDef.extras.runeOfFortitudeTax
                  )
                  g.supports
              attackerPlayer = lookupPlayer attacker g
              Resources attackerRes = attackerPlayer.resources
              runeCost = length eligible
              (paidRune, penalty) =
                if runeHere
                  then if attackerRes >= runeCost
                    then (True, 0)
                    else (False, 1)
                  else (False, 0)
              attackerAfterRune =
                if paidRune
                  then attackerPlayer {resources = Resources (attackerRes - runeCost)}
                  else attackerPlayer
              -- Defenders are filled in during step 3
              -- (AdvanceCombatToDefenders) after the defender resolves
              -- the choose-defenders prompt; leave the list empty here.
              combatState =
                CombatState
                  { attackingPlayer = attacker
                  , defendingPlayer = defender
                  , targetZone = zone
                  , targetLegend = Nothing
                  , attackers = eligible
                  , defenders = []
                  , attackerPowerPenalty = penalty
                  , pendingAssignments = []
                  }
          modify \gx ->
            (setPlayer attacker attackerAfterRune gx)
              { combat = Just combatState
              , history = Map.insert ThisCombat mempty gx.history
              }
          recordEvent \h -> h
            { attackersDeclared = eligible <> h.attackersDeclared
            , combats =
                CombatRecord
                  { attacker
                  , defender
                  , zone
                  , attackerKeys = eligible
                  , defenderKeys = []
                  }
                  : h.combats
            }
          logIt LogPlayerAction
            "log.combat.begins"
            [ ("attacker", playerParam attacker)
            , ("zone", zoneParam zone)
            ]
          when paidRune $
            logIt LogSystem
              "log.combat.rune_paid"
              [("amount", tshow runeCost)]
          when (penalty > 0) $
            logIt LogSystem
              "log.combat.rune_penalty"
              [("amount", tshow penalty)]
          -- Defending-legend sub-decision: if the opponent controls a
          -- legend, the attacker may target the legend through this
          -- zone instead of the capital section. Overflow damage then
          -- lands on the legend and is capped at its HP (no zone
          -- spillover, no defender spillover).
          gAfter <- get
          case legendOf defender gAfter of
            Just leg -> do
              ans <- askPrompt Prompt
                { player = attacker
                , kind = ChooseYesNo
                    { description =
                        "Target opponent's legend ("
                          <> T.pack leg.cardDef.title
                          <> ") through the "
                          <> zoneParam zone
                          <> " instead of the capital section?"
                    }
                , callback = CallbackInlinePrompt
                }
              case ans of
                PickBool True -> do
                  modify \gx -> case gx.combat of
                    Just cs ->
                      gx {combat = Just (cs {targetLegend = Just leg.key} :: CombatState)}
                    Nothing -> gx
                  logIt LogSystem
                    "log.combat.targets_legend"
                    [("attacker", playerParam attacker)]
                _ -> pure ()
            Nothing -> pure ()
          openAutoCombatWindow AfterDeclareCombatTarget
    AdvanceCombatToAttackers ->
      openAutoCombatWindow AfterDeclareAttackers
    ResolveAmbushStep -> do
      -- Step 2.5 (Ambush): offer the defender each affordable facedown
      -- development in the defending zone that carries Ambush X. One
      -- ambush per firing — flipping re-enters this step — so the
      -- iterative budget check stays consistent. Unit and tactic
      -- developments ambush (a unit becomes a defender; a tactic
      -- resolves and is discarded); support/quest/legend ambush is left
      -- for follow-up.
      g <- get
      case g.combat of
        Nothing -> send AdvanceCombatToDefenders
        Just cs -> do
          let pk = cs.defendingPlayer
              zk = cs.targetZone
              player = lookupPlayer pk g
              Resources budget = player.resources
              ambushable c = case c.def of
                UnitCardDef _ -> True
                TacticCardDef _ -> True
                _ -> False
              eligible =
                [ c
                | c <- Map.findWithDefault [] zk player.developmentCards
                , ambushable c
                , Just x <- [someCardAmbushCost c.def]
                , x <= budget
                ]
          if null eligible
            then send AdvanceCombatToDefenders
            else do
              ans <- askPrompt Prompt
                { player = pk
                , kind = ChooseFromCards
                    { cards = eligible
                    , minPick = 0
                    , maxPick = 1
                    , description =
                        "Ambush: flip a development in the defending zone "
                          <> "(pay its Ambush cost)?"
                    }
                , callback = CallbackInlinePrompt
                }
              case ans of
                PickUnits (ck : _)
                  | any ((== ck) . (.key)) eligible ->
                      send (AmbushDevelopment pk zk ck)
                _ -> send AdvanceCombatToDefenders
    AmbushDevelopment pk zk cardKey -> do
      -- Flip one specific facedown development faceup as an ambush:
      -- pay Ambush X, pop it, put the unit into play (no end-of-turn
      -- sacrifice), force it to defend, fire its enter-play text, then
      -- re-enter the Ambush step to offer the next one.
      g <- get
      let player = lookupPlayer pk g
          zoneCards = Map.findWithDefault [] zk player.developmentCards
      case find ((== cardKey) . (.key)) zoneCards of
        Just c
          | UnitCardDef cardDef <- c.def -> do
              let cost = fromMaybe 0 (someCardAmbushCost c.def)
                  rest = filter ((/= cardKey) . (.key)) zoneCards
                  cap = player.capital
                  cap' = Capital
                    { kingdom = decrementDev zk cap.kingdom
                    , quest = decrementDev zk cap.quest
                    , battlefield = decrementDev zk cap.battlefield
                    }
                  player' = player
                    { developmentCards = Map.insert zk rest player.developmentCards
                    , capital = cap'
                    }
                  unit = freshUnit cardKey pk zk cardDef
              modify \gx -> (setPlayer pk player' gx) {units = unit : gx.units}
              send (SpendResources pk cost)
              send (InstallModifier (UnitRef cardKey) (Modifier MustDefend EndOfTurn))
              logIt LogSystem
                "log.development.ambushed"
                [ ("player", playerParam pk)
                , ("zone", zoneParam zk)
                , ("card", T.pack cardDef.title)
                ]
              send (UnitEnteredPlay pk cardKey)
              send (UnitAmbushed pk cardKey)
              send ResolveAmbushStep
          | TacticCardDef cardDef <- c.def -> do
              -- A tactic ambush: pop the development, pay the cost, put
              -- the card in discard, and resolve its effect (it doesn't
              -- enter play). Then re-enter the step.
              let cost = fromMaybe 0 (someCardAmbushCost c.def)
                  rest = filter ((/= cardKey) . (.key)) zoneCards
                  cap = player.capital
                  cap' = Capital
                    { kingdom = decrementDev zk cap.kingdom
                    , quest = decrementDev zk cap.quest
                    , battlefield = decrementDev zk cap.battlefield
                    }
                  player' = player
                    { developmentCards = Map.insert zk rest player.developmentCards
                    , capital = cap'
                    , discard = mkCard cardKey (TacticCardDef cardDef) : player.discard
                    }
              modify (setPlayer pk player')
              send (SpendResources pk cost)
              logIt LogSystem
                "log.development.ambushed"
                [ ("player", playerParam pk)
                , ("zone", zoneParam zk)
                , ("card", T.pack cardDef.title)
                ]
              send (TacticResolved pk cardDef.code NoTarget 0)
              send ResolveAmbushStep
        _ -> send AdvanceCombatToDefenders
    AdvanceCombatToDefenders -> do
      -- Step 3: defender chooses which of their units defend the
      -- attacked zone. Prompt the defending player to pick a subset of
      -- the legal candidates (own units in the target zone that aren't
      -- corrupt or blocked by CannotDefend).
      g <- get
      case g.combat of
        Nothing -> pure ()
        Just cs -> do
          let candidates = eligibleDefenderCandidates g cs.defendingPlayer cs.targetZone
              -- "Target unit must defend this turn, if able."
              -- (Animosity, Alluring Daemonettes.) Eligible units
              -- carrying the marker are force-included regardless of
              -- the player's pick.
              compelled =
                [ k
                | k <- candidates
                , hasModifier g.modifiers k MustDefend
                ]
          defs <-
            if null candidates
              then pure []
              else do
                ans <- askPrompt Prompt
                  { player = cs.defendingPlayer
                  , kind = ChooseUnits
                      { filterSpec = UnitsFromList candidates
                      , minPick = 0
                      , maxPick = length candidates
                      , description = "Choose defenders."
                      }
                  , callback = CallbackInlinePrompt
                  }
                let allowed = filter (`elem` candidates)
                pure $ case ans of
                  PickUnits picks -> allowed picks
                  _ -> []
          let withCompelled = defs <> filter (`notElem` defs) compelled
          send $ DeclareDefenders withCompelled
    DeclareDefenders defs -> do
      modify \gx -> case gx.combat of
        Just cs -> gx {combat = Just (cs {defenders = defs} :: CombatState)}
        Nothing -> gx
      -- Complete the in-flight combat record with the declared
      -- defenders (Malus Darkblade reads these at phase end).
      recordEvent \h -> h
        { combats = case h.combats of
            (rec : rest) -> (rec {defenderKeys = defs} :: CombatRecord) : rest
            [] -> []
        }
      -- Fire Counterstrike: each defending unit with Counterstrike N
      -- immediately deals N uncancellable damage to one attacker of
      -- the defender's choice. Triggers in step 3, before regular
      -- damage assigns. With multiple attackers still in play we
      -- prompt the defender to pick the target. Ulric's Fury adds a
      -- turn-scoped bonus to every defending unit.
      g <- get
      case g.combat of
        Just cs -> do
          let defenderUnits =
                [ u
                | k <- defs
                , Just u <- [findUnit k g]
                ]
              csBonus =
                Map.findWithDefault 0 cs.defendingPlayer
                  g.defenderCounterstrikeBonus
          traverse_
            ( \def -> do
                let cs_total = totalCounterstrike g def + csBonus
                when (cs_total > 0) $ do
                  g' <- get
                  let alive = filter (\k -> isJust (findUnit k g')) cs.attackers
                  case alive of
                    [] -> pure ()
                    [single] -> send $ DealDamageToUnitUncancellable single cs_total
                    many -> do
                      ans <- askPrompt Prompt
                        { player = cs.defendingPlayer
                        , kind = ChooseUnits
                            { filterSpec = UnitsFromList many
                            , minPick = 1
                            , maxPick = 1
                            , description =
                                "Counterstrike: choose an attacker to take "
                                  <> tshow cs_total
                                  <> " damage."
                            }
                        , callback = CallbackInlinePrompt
                        }
                      case ans of
                        PickUnits (target : _)
                          | target `elem` many ->
                              send $ DealDamageToUnitUncancellable target cs_total
                        _ ->
                          -- Player declined / illegal pick: default to
                          -- the first surviving attacker so the
                          -- Counterstrike still resolves.
                          send $ DealDamageToUnitUncancellable (head many) cs_total
            )
            defenderUnits
        Nothing -> pure ()
      openAutoCombatWindow AfterDeclareDefenders
    AdvanceCombatToAssign -> do
      -- Step 4: assignment. Prompt each side for the order in which
      -- their damage gets allocated, then queue the assignments. The
      -- damage-cancel window (Defenders of the Faith, Master Rune of
      -- Valaya) opens afterwards.
      g <- get
      case g.combat of
        Nothing -> pure ()
        Just cs -> do
          defenderAssignmentOrder <-
            promptAssignmentOrder
              cs.attackingPlayer
              "Order defenders to receive your damage (first to last)."
              cs.defenders
          attackerAssignmentOrder <-
            promptAssignmentOrder
              cs.defendingPlayer
              "Order attackers to receive defender damage (first to last)."
              cs.attackers
          g' <- get
          case g'.combat of
            Just cs' ->
              assignCombatDamage g' cs' defenderAssignmentOrder attackerAssignmentOrder
            Nothing -> pure ()
      openAutoCombatWindow AfterAssignCombatDamage
    AdvanceCombatToApply -> do
      -- Step 5: commit the staged damage and open the post-apply
      -- response window. Cancellation effects had their chance to
      -- mutate the pending list during the AfterAssign window.
      commitPendingCombatDamage
      openAutoCombatWindow AfterApplyCombatDamage
    ResolveCombat -> do
      -- Legacy entry-point: the staged 5-step flow does this work
      -- via AdvanceCombatToAssign + CloseActionWindow advances.
      -- Calling ResolveCombat directly runs assign + commit + ends
      -- without opening the response windows. The assignment order
      -- follows whatever was stored on the combat state.
      g <- get
      case g.combat of
        Nothing -> pure ()
        Just cs -> do
          assignCombatDamage g cs cs.defenders cs.attackers
          commitPendingCombatDamage
          send EndCombat
    EndCombat -> do
      g <- get
      -- Fire post-damage "when this unit damages an enemy" effects
      -- now that every queued combat-damage message has flushed.
      case g.combat of
        Just cs -> firePerSourceCombatEffects g cs
        Nothing -> pure ()
      modify \gx -> gx {combat = Nothing}
      logIt LogSystem "log.combat.ends" []
    CancelAttack -> do
      -- End the combat with no damage and no post-combat effects. The
      -- downstream AdvanceCombatTo*/ResolveAmbushStep handlers all guard
      -- on 'g.combat == Nothing', so they no-op once this clears it and
      -- priority returns to the battlefield action window.
      g <- get
      case g.combat of
        Nothing -> pure ()
        Just _ -> do
          modify \gx -> gx {combat = Nothing}
          logIt LogSystem "log.combat.cancelled" []
    BlockAttacksThisTurn pk ->
      modify \gx ->
        gx
          { attackBlockedThisTurn =
              if pk `elem` gx.attackBlockedThisTurn
                then gx.attackBlockedThisTurn
                else pk : gx.attackBlockedThisTurn
          }
    FireScoutDiscards attacker defender attackerKeys defenderKeys -> do
      -- Post-damage: count surviving Scouts on each side and queue a
      -- single 'DiscardRandomFromHand' against the opposing player
      -- for each. Reading 'g.units' at receive-time (rather than at
      -- enqueue-time in 'commitPendingCombatDamage') is what makes
      -- "surviving" correct — dead Scouts have already been removed
      -- from 'g.units' by the time this fires.
      g <- get
      let scoutOf u = Scout `elem` unitKeywords u
          attackerScouts = filter scoutOf $ mapMaybe (`findUnit` g) attackerKeys
          defenderScouts = filter scoutOf $ mapMaybe (`findUnit` g) defenderKeys
      replicateM_ (length attackerScouts) $
        send $ DiscardRandomFromHand defender
      replicateM_ (length defenderScouts) $
        send $ DiscardRandomFromHand attacker
    FireRaiderResources attacker attackerKeys -> do
      -- Sum Raider X across every attacker still in play (survivors).
      -- Reading 'g.units' here — not at enqueue time — is what makes
      -- "survived combat" correct, as with the Scout sweep above.
      g <- get
      let raiderOf u = sum [n | Raider n <- unitKeywords u]
          total = sum $ map raiderOf $ mapMaybe (`findUnit` g) attackerKeys
      when (total > 0) $ send $ GainResources attacker total
    PutUnitIntoPlay pk cardKey zone -> do
      -- Skip cost; same wiring as 'PlayUnit' but no resource debit and
      -- no Variable-cost gate.
      player <- getPlayerS pk
      whenJust (takeUnitFromHand cardKey player) \(cardDef, playerWithoutCard) -> do
        let unit = freshUnit cardKey pk zone cardDef
        modify \gx -> (setPlayer pk playerWithoutCard gx) {units = unit : gx.units}
        logIt LogSystem "log.unit.summoned_free"
          [("player", playerParam pk), ("card", T.pack cardDef.title)]
        send $ UnitEnteredPlay pk cardKey
    PutUnitIntoPlayFromDiscard pk cardKey zone -> do
      player <- getPlayerS pk
      whenJust (takeUnitFromDiscard cardKey player) \(cardDef, playerWithoutCard) -> do
        let unit = freshUnit cardKey pk zone cardDef
            -- If a combat is in progress with this player as attacker,
            -- also add the fresh unit to its attackers list (Reckless
            -- Attack relies on this).
            joinAsAttacker cs = cs {attackers = cardKey : cs.attackers} :: CombatState
        modify \gx -> (setPlayer pk playerWithoutCard gx)
          { units = unit : gx.units
          , combat = case gx.combat of
              Just cs | cs.attackingPlayer == pk -> Just $ joinAsAttacker cs
              other -> other
          }
        g' <- get
        whenJust g'.combat \cs ->
          when (cs.attackingPlayer == pk) $
            recordEvent \h -> h {attackersDeclared = cardKey : h.attackersDeclared}
        logIt LogSystem "log.unit.summoned_from_discard"
          [("player", playerParam pk), ("card", T.pack cardDef.title)]
        send $ UnitEnteredPlay pk cardKey
    InstallModifier target modifier -> do
      modify \g ->
        g
          { modifiers =
              Map.insertWith (++) target [modifier] g.modifiers
          }
      logIt LogSystem "log.modifier.installed" []
    ClearScopedModifiers scope -> do
      modify \g ->
        g
          { modifiers =
              Map.map (filter (\m -> m.scope /= scope)) g.modifiers
          }
      logIt LogSystem "log.modifier.cleared" []
    ScheduleAttackerSacrifice -> do
      modify \g ->
        g
          { pendingEndOfPhase =
              (BattlefieldPhase, PESacrificeAttackersThisPhase)
                : g.pendingEndOfPhase
          }
      logIt LogSystem
        "log.effect.scheduled"
        [("trigger", "EndOfBattlefieldPhase"), ("what", "sacrifice attackers")]
    MoveAllDamage fromKey toKey -> do
      g <- get
      case (findUnit fromKey g, findUnit toKey g) of
        (Just src, Just dst) -> do
          let Damage srcDmg = src.damage
          when (srcDmg > 0) $ do
            let src' = (src {damage = Damage 0}) :: UnitDetails
                Damage dstDmg = dst.damage
                dst' = (dst {damage = Damage (dstDmg + srcDmg)}) :: UnitDetails
            modify \gx ->
              gx {units = replaceUnit src' (replaceUnit dst' gx.units)}
            logIt LogSystem
              "log.unit.damage_moved"
              [ ("source", T.pack src.cardDef.title)
              , ("target", T.pack dst.cardDef.title)
              , ("amount", tshow srcDmg)
              ]
            -- Destination might now exceed its HP.
            when (dstDmg + srcDmg >= dst.effectiveMaxHP) $
              send $ DestroyUnit toKey
        _ -> pure ()
    MoveDamage fromKey toKey n -> do
      g <- get
      case (findUnit fromKey g, findUnit toKey g) of
        (Just src, Just dst) -> do
          let Damage srcDmg = src.damage
              moved = min n srcDmg
          when (moved > 0) $ do
            let src' = (src {damage = Damage (srcDmg - moved)}) :: UnitDetails
                Damage dstDmg = dst.damage
                dst' = (dst {damage = Damage (dstDmg + moved)}) :: UnitDetails
            modify \gx ->
              gx {units = replaceUnit src' (replaceUnit dst' gx.units)}
            logIt LogSystem
              "log.unit.damage_moved"
              [ ("source", T.pack src.cardDef.title)
              , ("target", T.pack dst.cardDef.title)
              , ("amount", tshow moved)
              ]
            when (dstDmg + moved >= dst.effectiveMaxHP) $
              send $ DestroyUnit toKey
        _ -> pure ()
    MoveUnit ukey newZone -> do
      g <- get
      whenJust (findUnit ukey g) \u ->
        when (u.zone /= newZone) do
          -- "Limit one Hero per zone" blocks moves as well as plays
          -- (FAQ: cannot "play, take control of, move, or put into
          -- play" another Hero into that zone).
          if heroLimitBlocks g u.controller u.cardDef newZone (Just ukey)
            then
              logIt LogSystem
                "log.unit.move_blocked_hero"
                [ ("card", T.pack u.cardDef.title)
                , ("zone", zoneParam newZone)
                ]
            else do
              let u' = (u {zone = newZone}) :: UnitDetails
              modify \gx -> gx {units = replaceUnit u' gx.units}
              -- FAQ Quests v1.7: a questing unit that moves to another
              -- zone is no longer questing; tokens on the quest are
              -- discarded immediately.
              let touched =
                    [ (q {questingUnit = Nothing, tokens = 0}) :: QuestDetails
                    | q <- g.quests
                    , q.questingUnit == Just ukey
                    ]
              unless (null touched) do
                modify \gx ->
                  gx {quests = foldr replaceQuest gx.quests touched}
                traverse_
                  ( \q ->
                      logIt LogSystem
                        "log.quest.unit_left"
                        [("quest", T.pack q.cardDef.title)]
                  )
                  touched
              logIt LogSystem
                "log.unit.moved"
                [ ("player", playerParam u.controller)
                , ("card", T.pack u.cardDef.title)
                , ("zone", zoneParam newZone)
                ]
    ReturnUnitToHand ukey -> do
      g <- get
      whenJust (findUnit ukey g) \u -> do
        -- Card goes back to its controller's hand; attachments go to
        -- their respective controllers' discard piles (the host left
        -- play, so any attachments do too — the rulebook treats
        -- bounce-to-hand as a "leaves play" event).
        let player = lookupPlayer u.controller g
            handCard = mkCard u.key (UnitCardDef u.cardDef)
            player' = player {hand = handCard : player.hand}
        modify (setPlayer u.controller player')
        for_ u.attachments discardAttachment
        modify \gx -> gx {units = removeById ukey gx.units}
        recordEvent \h -> h {unitsDiscarded = h.unitsDiscarded + 1}
        logIt LogSystem
          "log.unit.returned"
          [ ("player", playerParam u.controller)
          , ("card", T.pack u.cardDef.title)
          ]
        send $ UnitLeftPlay DepartedUnit
          { key = ukey
          , controller = u.controller
          , zone = u.zone
          , cardDef = u.cardDef
          }
    MillFromDeck pk n -> do
      g <- get
      let player = lookupPlayer pk g
          (top, rest) = splitAt (max 0 n) player.deck
      when (not (null top)) do
        let player' = player {deck = rest, discard = reverse top <> player.discard}
        modify (setPlayer pk player')
        logIt LogSystem
          "log.deck.milled"
          [("player", playerParam pk), ("count", tshow (length top))]
    DiscardHand pk -> do
      g <- get
      let player = lookupPlayer pk g
          n = length player.hand
      when (n > 0) do
        let player' = player {hand = [], discard = player.hand <> player.discard}
        modify (setPlayer pk player')
        logIt LogSystem
          "log.hand.discarded_all"
          [("player", playerParam pk), ("count", tshow n)]
    RecycleDiscard pk n -> do
      g <- get
      let player = lookupPlayer pk g
          (back, rest) = splitAt (max 0 n) player.discard
      when (not (null back)) do
        let player' = player {discard = rest, deck = player.deck <> back}
        modify (setPlayer pk player')
        logIt LogSystem
          "log.discard.recycled"
          [("player", playerParam pk), ("count", tshow (length back))]
        send (ShuffleDeck pk)
    MoveDevelopment pk fromZ toZ -> do
      g <- get
      let player = lookupPlayer pk g
          fromCards = Map.findWithDefault [] fromZ player.developmentCards
      case fromCards of
        [] -> pure ()
        (c : rest) ->
          when (fromZ /= toZ) do
            let toCards = Map.findWithDefault [] toZ player.developmentCards
                developmentCards' =
                  Map.insert fromZ rest $
                    Map.insert toZ (c : toCards) player.developmentCards
                cap = player.capital
                cap' = Capital
                  { kingdom = bumpDev fromZ toZ cap.kingdom
                  , quest = bumpDev fromZ toZ cap.quest
                  , battlefield = bumpDev fromZ toZ cap.battlefield
                  }
                player' = player
                  { developmentCards = developmentCards'
                  , capital = cap'
                  }
            modify (setPlayer pk player')
            logIt LogSystem
              "log.development.moved"
              [ ("player", playerParam pk)
              , ("from", zoneParam fromZ)
              , ("to", zoneParam toZ)
              ]
    IndirectDamage pk amount -> do
      -- Per rules: the targeted player chooses how to distribute
      -- each point. They cannot put more on a zone than its
      -- remaining HP and cannot target a burned zone. Excess that
      -- can't be assigned (all non-burned zones full) is lost. We
      -- prompt one point at a time, tracking already-placed
      -- allocations so the eligibility filter respects the
      -- slack-vs-HP cap across the whole effect; finally we queue a
      -- single 'DealDamageToZone' per chosen zone so the normal
      -- shield / burn / elimination machinery fires.
      when (amount > 0) $ do
        allocation <- collectIndirect pk amount mempty
        let totalAllocated = sum (Map.elems allocation)
        for_ (Map.toList allocation) $ \(zk, n) ->
          when (n > 0) $ send (DealDamageToZone pk zk n)
        when (totalAllocated > 0) $
          logIt LogSystem
            "log.capital.indirect"
            [ ("player", playerParam pk)
            , ("amount", tshow totalAllocated)
            ]
    RedirectAttackZone newZone -> do
      g <- get
      whenJust g.combat \cs ->
        unless (cs.targetZone == newZone) do
          -- Refuse to redirect into a burned zone.
          let target = lookupPlayer cs.defendingPlayer g
              destZone = getZone newZone target
          unless destZone.burned do
            let cs' = (cs {targetZone = newZone}) :: CombatState
            modify \gx -> gx {combat = Just cs'}
            logIt LogSystem
              "log.combat.redirected"
              [ ("attacker", playerParam cs.attackingPlayer)
              , ("zone", zoneParam newZone)
              ]
    ArmActionCancel pk -> do
      modify \gx -> gx
        { pendingActionCancel =
            Map.insertWith (+) pk 1 gx.pendingActionCancel
        }
      logIt LogSystem
        "log.action.cancel_scheduled"
        [("player", playerParam pk)]
    SlaaneshDominate caster opp count -> do
      g <- get
      shuffled <- shuffleM (lookupPlayer opp g).hand
      let revealed = take (max 0 count) shuffled
          tactics =
            [ (cd.code, cd.title)
            | c <- revealed
            , Just cd <- [asTactic c.def]
            ]
      for_ tactics \(code, title) -> do
        play <- askYesNo caster
          ("Play '" <> T.pack title <> "' for free?")
        when play (send (TacticResolved caster code NoTarget 0))
    ScheduleNextUnitDiscount pk n -> do
      modify \gx -> gx
        { pendingUnitDiscount =
            Map.insertWith (+) pk n gx.pendingUnitDiscount
        }
    ScheduleNextUnitDamage pk n -> do
      modify \gx -> gx
        { pendingUnitOnPlayDamage =
            Map.insertWith (+) pk n gx.pendingUnitOnPlayDamage
        }
    DestroyDevelopment pk zk -> do
      g <- get
      let player = lookupPlayer pk g
          zoneCards = Map.findWithDefault [] zk player.developmentCards
      case zoneCards of
        [] -> pure ()
        (c : rest) -> do
          let cap = player.capital
              cap' = Capital
                { kingdom = decrementDev zk cap.kingdom
                , quest = decrementDev zk cap.quest
                , battlefield = decrementDev zk cap.battlefield
                }
              player' = player
                { developmentCards = Map.insert zk rest player.developmentCards
                , capital = cap'
                , discard = c : player.discard
                }
          modify (setPlayer pk player')
          logIt LogSystem
            "log.development.destroyed"
            [("player", playerParam pk), ("zone", zoneParam zk)]
    FlipDevelopment pk zk -> do
      g <- get
      let player = lookupPlayer pk g
          zoneCards = Map.findWithDefault [] zk player.developmentCards
      case zoneCards of
        [] -> pure ()
        (c : rest) -> case c.def of
          UnitCardDef cardDef -> do
            -- Pop the dev (decrement count, remove from facedown
            -- list), put the unit into play, schedule its EoT
            -- sacrifice. Card key carries through so the frontend
            -- can morph the dev → unit.
            let cap = player.capital
                cap' = Capital
                  { kingdom = decrementDev zk cap.kingdom
                  , quest = decrementDev zk cap.quest
                  , battlefield = decrementDev zk cap.battlefield
                  }
                player' = player
                  { developmentCards = Map.insert zk rest player.developmentCards
                  , capital = cap'
                  }
                unit = freshUnit c.key pk zk cardDef
            modify \gx -> (setPlayer pk player' gx) {units = unit : gx.units}
            -- Schedule the printed end-of-turn sacrifice.
            modify \gx -> gx
              { pendingEndOfTurn = PEDestroyUnit c.key : gx.pendingEndOfTurn
              }
            logIt LogSystem
              "log.development.flipped_unit"
              [ ("player", playerParam pk)
              , ("zone", zoneParam zk)
              , ("card", T.pack cardDef.title)
              ]
            send (UnitEnteredPlay pk c.key)
          _ ->
            -- Non-unit flips immediately sacrifice the development.
            send (DestroyDevelopment pk zk)
    PlayLegend pk cardKey -> do
      -- One legend per player at a time. If one is already in play for
      -- this player, silently refuse.
      hasLegend <- gets (isJust . legendOf pk)
      unless hasLegend $
        withPaidPlay pk (takeLegendFromHand cardKey) (\g cd -> effectiveTotalCost g pk cd)
          \cardDef paidPlayer n -> do
            let legendDetails = LegendDetails
                  { key = cardKey
                  , controller = pk
                  , zone = BattlefieldZone
                  , cardDef
                  , damage = Damage 0
                  }
            modify \gx -> (setPlayer pk paidPlayer gx) {legends = legendDetails : gx.legends}
            logIt LogPlayerAction
              "log.legend.played"
              [ ("player", playerParam pk)
              , ("card", T.pack cardDef.title)
              , ("cost", tshow n)
              ]
            send $ LegendEnteredPlay pk cardKey
    LegendEnteredPlay pk _key ->
      logIt LogSystem "log.legend.entered_play" [("player", playerParam pk)]
    DealDamageToLegend lkey amount -> do
      g <- get
      whenJust (findLegend lkey g) \l -> do
          let inflated = max 0 amount
              Damage existing = l.damage
              newDmg = Damage (existing + inflated)
              l' = l {damage = newDmg} :: LegendDetails
          modify \gx -> gx {legends = replaceLegend l' gx.legends}
          logIt LogSystem
            "log.legend.damaged"
            [ ("card", T.pack l.cardDef.title)
            , ("amount", tshow inflated)
            ]
          let Damage total = newDmg
              hp = legendPrintedHPFromDef l.cardDef
          when (total >= hp) $
            send $ DestroyLegend lkey
    DestroyLegend lkey -> do
      mlegend <- gets (findLegend lkey)
      whenJust mlegend \l -> do
        discardToController l.controller $ mkCard l.key (LegendCardDef l.cardDef)
        modify \gx -> gx {legends = removeById lkey gx.legends}
        logIt LogSystem "log.legend.destroyed"
          [("player", playerParam l.controller), ("card", T.pack l.cardDef.title)]
        send $ LegendLeftPlay l.controller lkey l.cardDef.code
    LegendLeftPlay _pk _lkey _code -> pure ()
    AdjustUnitTokens ukey delta -> do
      g <- get
      whenJust (findUnit ukey g) \u -> do
        let n = max 0 (u.tokens + delta)
            u' = (u {tokens = n}) :: UnitDetails
        modify \gx -> gx {units = replaceUnit u' gx.units}
        logIt LogSystem
          "log.unit.tokens"
          [("card", T.pack u.cardDef.title), ("count", tshow n)]
    SetDrawCap pk n -> do
      modify \gx -> gx {drawCaps = Map.insert pk (max 0 n) gx.drawCaps}
      logIt LogSystem
        "log.draw.capped_set"
        [("player", playerParam pk), ("count", tshow (max 0 n))]
    ArmCapitalShield pk points refundPer -> do
      modify \gx -> gx
        { capitalShields =
            Map.insertWith
              (flip (<>))
              pk
              [CapitalShieldGrant {points, refundPer}]
              gx.capitalShields
        }
      logIt LogSystem "log.capital.ward_armed" [("player", playerParam pk)]
    ArmDefenderCounterstrike pk n ->
      modify \gx -> gx
        { defenderCounterstrikeBonus =
            Map.insertWith (+) pk n gx.defenderCounterstrikeBonus
        }
    SetCombatDamageUncancellable -> do
      modify \gx -> gx {combatDamageUncancellable = True}
      logIt LogSystem "log.combat.uncancellable" []
    ArmFreeTactic pk -> do
      modify \gx -> gx
        {pendingFreeTactic = Map.insertWith (+) pk 1 gx.pendingFreeTactic}
      logIt LogSystem "log.tactic.free_armed" [("player", playerParam pk)]
    PlaySupportFromDiscard pk cardKey zone -> do
      g <- get
      player <- getPlayerS pk
      whenJust (takeSupportFromDiscard cardKey player) \(cardDef, playerWithoutCard) ->
        when (canEnterZone g pk cardDef zone) do
          let support = freshSupport cardKey pk zone Nothing cardDef
          modify \gx -> (setPlayer pk playerWithoutCard gx) {supports = support : gx.supports}
          logIt LogSystem "log.support.played_from_discard"
            [("player", playerParam pk), ("card", T.pack cardDef.title)]
          send $ SupportEnteredPlay pk cardKey
    PlayUnitFromDiscard pk cardKey zone -> do
      g <- get
      let player = lookupPlayer pk g
      whenJust (takeUnitFromDiscard cardKey player) \(cardDef, playerWithoutCard) ->
        when (Necromancy `elem` cardDef.keywords && canPlayNonTactic pk g && canPlayCard pk cardDef g) $
          case cardDef.cost of
            Variable -> pure ()
            Fixed _ -> do
              let n = effectiveTotalCost g pk cardDef
              when (player.resources >= Resources n) do
                markPlayedLimited cardDef
                recordEvent \h -> h
                  { playedBy =
                      Map.insertWith (<>) pk [cardCodeFilter cardDef] h.playedBy
                  }
                let paid = playerWithoutCard {resources = player.resources - Resources n}
                    unit = freshUnit cardKey pk zone cardDef
                modify \gx ->
                  let gx' = (setPlayer pk paid gx) {units = unit : gx.units}
                   in gx'
                        { pendingEndOfTurn =
                            PEReturnUnitToDeckBottom cardKey : gx'.pendingEndOfTurn
                        }
                logIt LogPlayerAction "log.unit.necromancy"
                  [ ("player", playerParam pk)
                  , ("card", T.pack cardDef.title)
                  , ("cost", tshow n)
                  ]
                send $ UnitEnteredPlay pk cardKey
    PutUnitIntoPlayFromDeck pk cardKey zone -> do
      g <- get
      player <- getPlayerS pk
      whenJust (takeUnitFromDeck cardKey player) \(cardDef, playerWithoutCard) ->
        when (canEnterZone g pk cardDef zone) do
          let unit = freshUnit cardKey pk zone cardDef
          modify \gx -> (setPlayer pk playerWithoutCard gx) {units = unit : gx.units}
          logIt LogSystem "log.unit.summoned_from_deck"
            [("player", playerParam pk), ("card", T.pack cardDef.title)]
          send $ UnitEnteredPlay pk cardKey
          -- Pulling cards out of the deck can empty it; the standing
          -- decked-out rule still applies.
          g' <- get
          when (null (lookupPlayer pk g').deck) $
            send $ Eliminate pk DeckedOut
    PutRandomUnitIntoPlayFromDeckTop pk n -> do
      -- Blessings of Tzeentch: reveal the top N, put one unit found
      -- there into play at random (controller picks the zone), then
      -- shuffle.
      g <- get
      let player = lookupPlayer pk g
          top = take (max 0 n) player.deck
          unitCards = [c | c <- top, isJust (asUnit c.def)]
      case unitCards of
        [] -> send (ShuffleDeck pk)
        _ -> do
          idx <- getRandomR (0, length unitCards - 1)
          let chosen = unitCards !! idx
              zones = case asUnit chosen.def of
                Just cd ->
                  [ zk
                  | zk <- [KingdomZone, QuestZone, BattlefieldZone]
                  , canEnterZone g pk cd zk
                  ]
                Nothing -> []
          case zones of
            [] -> send (ShuffleDeck pk)
            _ -> do
              ans <- askPrompt Prompt
                { player = pk
                , kind = ChooseTargetOption
                    { options = [TargetZoneOption pk zk | zk <- zones]
                    , description = "Choose the zone for the summoned unit."
                    }
                , callback = CallbackInlinePrompt
                }
              let zk = case ans of
                    PickTargetOption (TargetZoneOption owner z)
                      | owner == pk, z `elem` zones -> z
                    _ -> head zones
              send (PutUnitIntoPlayFromDeck pk chosen.key zk)
              send (ShuffleDeck pk)
    StealUnitFromDiscard newController srcPlayer cardKey zone corruptIt -> do
      g <- get
      let src = lookupPlayer srcPlayer g
      whenJust (takeUnitFromDiscard cardKey src) \(cardDef, srcWithoutCard) ->
        when (canEnterZone g newController cardDef zone) do
          let unit = freshUnit cardKey newController zone cardDef
          modify \gx ->
            (setPlayer srcPlayer srcWithoutCard gx) {units = unit : gx.units}
          logIt LogSystem "log.unit.stolen_from_discard"
            [ ("player", playerParam newController)
            , ("card", T.pack cardDef.title)
            ]
          send $ UnitEnteredPlay newController cardKey
          when corruptIt $ send $ CorruptUnit cardKey
    ReturnDevelopmentToHand pk zk -> do
      g <- get
      let player = lookupPlayer pk g
          zoneCards = Map.findWithDefault [] zk player.developmentCards
      case zoneCards of
        [] -> pure ()
        (c : rest) -> do
          let cap = player.capital
              cap' = Capital
                { kingdom = decrementDev zk cap.kingdom
                , quest = decrementDev zk cap.quest
                , battlefield = decrementDev zk cap.battlefield
                }
              player' = player
                { developmentCards = Map.insert zk rest player.developmentCards
                , capital = cap'
                , hand = c : player.hand
                }
          modify (setPlayer pk player')
          logIt LogSystem
            "log.development.returned"
            [("player", playerParam pk), ("zone", zoneParam zk)]
    ConvertDepartedToDevelopment pk cardKey zk -> do
      g <- get
      let player = lookupPlayer pk g
          (matches, rest) = partition (\c -> c.key == cardKey) player.discard
      case matches of
        [] -> pure ()
        (c : _) -> do
          let zoneL = getZone zk player
              Developments d = zoneL.developments
              zoneL' = (zoneL {developments = Developments (d + 1)}) :: Zone
              existing = Map.findWithDefault [] zk player.developmentCards
              player' =
                (setZone zk zoneL' player)
                  { discard = rest
                  , developmentCards =
                      Map.insert zk (c : existing) player.developmentCards
                  }
          modify (setPlayer pk player')
          logIt LogSystem
            "log.development.reclaimed"
            [("player", playerParam pk), ("zone", zoneParam zk)]
    AnimateDevelopment owner zk pwr hp -> do
      g <- get
      let zoneL = getZone zk (lookupPlayer owner g)
          Developments d = zoneL.developments
      -- Needs an actual development to animate; the count is NOT
      -- decremented ("it also counts as a development").
      when (d > 0) do
        let UnitKey nextN = g.nextUnitKey
            key = UnitKey nextN
            unit = freshUnit key owner zk (animatedDevelopmentDef pwr hp)
        modify \gx -> gx
          { units = unit : gx.units
          , nextUnitKey = UnitKey (nextN + 1)
          , pendingEndOfTurn = PERemoveAnimatedUnit key : gx.pendingEndOfTurn
          }
        logIt LogSystem
          "log.unit.animated"
          [("player", playerParam owner), ("zone", zoneParam zk)]
        send $ UnitEnteredPlay owner key
    TakeCardsFromDeckToHand pk keys -> do
      g <- get
      let player = lookupPlayer pk g
          (taken, rest) = partition (\c -> c.key `elem` keys) player.deck
      unless (null taken) do
        let player' = player {deck = rest, hand = player.hand <> taken}
        modify (setPlayer pk player')
        logIt LogSystem
          "log.deck.taken_to_hand"
          [("player", playerParam pk), ("count", tshow (length taken))]
        when (null rest) $
          send $ Eliminate pk DeckedOut
    ReturnCardsFromDiscardToHand pk keys -> do
      g <- get
      let player = lookupPlayer pk g
          (taken, rest) = partition (\c -> c.key `elem` keys) player.discard
      unless (null taken) do
        let player' = player {discard = rest, hand = player.hand <> taken}
        modify (setPlayer pk player')
        logIt LogSystem
          "log.discard.returned_to_hand"
          [("player", playerParam pk), ("count", tshow (length taken))]
    DiscardCardsFromHand pk keys -> do
      g <- get
      let player = lookupPlayer pk g
          (dropped, rest) = partition (\c -> c.key `elem` keys) player.hand
      unless (null dropped) do
        let player' = player {hand = rest, discard = dropped <> player.discard}
        modify (setPlayer pk player')
        logIt LogSystem
          "log.hand.discarded_chosen"
          [("player", playerParam pk), ("count", tshow (length dropped))]
    DiscardCardsFromDeck pk keys -> do
      g <- get
      let player = lookupPlayer pk g
          (dropped, rest) = partition (\c -> c.key `elem` keys) player.deck
      unless (null dropped) do
        let player' = player {deck = rest, discard = dropped <> player.discard}
        modify (setPlayer pk player')
        logIt LogSystem
          "log.deck.discarded_from"
          [("player", playerParam pk), ("count", tshow (length dropped))]
        when (null rest) $
          send $ Eliminate pk DeckedOut
    ArrangeDeckCards pk topKeys botKeys -> do
      g <- get
      let player = lookupPlayer pk g
          byKey k = find ((== k) . (.key)) player.deck
          topCards = mapMaybe byKey topKeys
          botCards = mapMaybe byKey botKeys
          moved = topKeys <> botKeys
          middle = [c | c <- player.deck, c.key `notElem` moved]
      unless (null topCards && null botCards) do
        modify (setPlayer pk player {deck = topCards <> middle <> botCards})
        logIt LogSystem
          "log.deck.rearranged"
          [("player", playerParam pk), ("count", tshow (length moved))]
    TransformUnitToAttachment ukey hostKey -> do
      g <- get
      case (findUnit ukey g, findUnit hostKey g) of
        (Just u, Just host) | ukey /= hostKey -> do
          -- The unit's own attachments fall off (it stops being a
          -- unit); the card itself re-enters as an attachment whose
          -- synthetic def destroys the host at the host controller's
          -- end of turn (Vigilant Elector).
          for_ u.attachments discardAttachment
          let attachment =
                (freshSupport ukey u.controller host.zone (Just hostKey)
                  (transformedUnitAttachmentDef u.cardDef))
          modify \gx ->
            let withoutUnit = removeById ukey gx.units
                host' =
                  (host {attachments = attachment : host.attachments})
                    :: UnitDetails
             in gx {units = replaceUnit host' withoutUnit}
          logIt LogSystem
            "log.unit.transformed_attachment"
            [ ("card", T.pack u.cardDef.title)
            , ("target", T.pack host.cardDef.title)
            ]
        _ -> pure ()
    MoveAttachment skey newHostKey -> do
      g <- get
      let hosts =
            [ (u, a)
            | u <- g.units
            , a <- u.attachments
            , a.key == skey
            ]
      case (listToMaybe hosts, findUnit newHostKey g) of
        (Just (oldHost, a), Just newHost)
          | oldHost.key /= newHost.key -> do
              let a' =
                    (a {attachedTo = Just newHostKey, zone = newHost.zone})
                      :: SupportDetails
                  oldHost' =
                    (oldHost {attachments = filter ((/= skey) . (.key)) oldHost.attachments})
                      :: UnitDetails
              modify \gx ->
                let units' = replaceUnit oldHost' gx.units
                    -- Re-read the new host AFTER the old host update in
                    -- case both reference the same record list.
                    units'' = case find ((== newHostKey) . (.key)) units' of
                      Just nh ->
                        replaceUnit
                          ((nh {attachments = a' : nh.attachments}) :: UnitDetails)
                          units'
                      Nothing -> units'
                 in gx {units = units''}
              logIt LogSystem
                "log.attachment.moved"
                [ ("card", T.pack a.cardDef.title)
                , ("target", T.pack newHost.cardDef.title)
                ]
        _ -> pure ()
    MoveSupport skey newZone -> do
      g <- get
      whenJust (findSupport skey g) \s ->
        when (s.zone /= newZone) do
          let s' = (s {zone = newZone}) :: SupportDetails
          modify \gx -> gx {supports = replaceSupport s' gx.supports}
          logIt LogSystem
            "log.support.moved"
            [ ("player", playerParam s.controller)
            , ("card", T.pack s.cardDef.title)
            , ("zone", zoneParam newZone)
            ]
    TakeControlOfUnit pk ukey -> do
      g <- get
      whenJust (findUnit ukey g) \u ->
        unless (u.controller == pk) do
          -- The hero-per-zone limit blocks taking control into an
          -- occupied zone, same as plays and moves.
          if heroLimitBlocks g pk u.cardDef u.zone (Just ukey)
            then
              logIt LogSystem
                "log.unit.move_blocked_hero"
                [ ("card", T.pack u.cardDef.title)
                , ("zone", zoneParam u.zone)
                ]
            else do
              let u' = (u {controller = pk}) :: UnitDetails
              modify \gx -> gx {units = replaceUnit u' gx.units}
              -- A questing unit that changes sides stops questing;
              -- accumulated tokens are lost (mirrors MoveUnit).
              let touched =
                    [ (q {questingUnit = Nothing, tokens = 0}) :: QuestDetails
                    | q <- g.quests
                    , q.questingUnit == Just ukey
                    ]
              unless (null touched) $
                modify \gx ->
                  gx {quests = foldr replaceQuest gx.quests touched}
              logIt LogSystem
                "log.unit.control_taken"
                [ ("player", playerParam pk)
                , ("card", T.pack u.cardDef.title)
                ]
    ScheduleControlReturn ukey pk ->
      modify \gx ->
        gx {pendingEndOfTurn = PEGiveControl ukey pk : gx.pendingEndOfTurn}
    CheckUnitVitals ukey -> do
      g <- get
      whenJust (findUnit ukey g) \u -> do
        let Damage d = u.damage
        when (d >= u.effectiveMaxHP) $ send (DestroyUnit ukey)
    RedirectAssignedUnitDamage fromKey toKey raw -> do
      let want = max 0 raw
      modify \gx -> case gx.combat of
        Nothing -> gx
        Just cs ->
          let available =
                sum
                  [ pd.cancellable
                  | pd <- cs.pendingAssignments
                  , pd.target == PDUnit fromKey
                  ]
              moved = min want available
              strip k pds = case pds of
                [] -> []
                (pd : rest)
                  | k <= 0 -> pd : rest
                  | pd.target == PDUnit fromKey ->
                      let take' = min k pd.cancellable
                       in pd {cancellable = pd.cancellable - take'}
                            : strip (k - take') rest
                  | otherwise -> pd : strip k rest
              pa' =
                strip moved cs.pendingAssignments
                  <> [ PendingDamage
                        { target = PDUnit toKey
                        , cancellable = moved
                        , uncancellable = 0
                        }
                     | moved > 0
                     ]
              cs' = (cs {pendingAssignments = pa'}) :: CombatState
           in gx {combat = Just cs'}
      logIt LogSystem
        "log.damage.redirected"
        [("amount", tshow want)]

-- | First-turn penalty: the starting player skips Quest and Battlefield
-- on the very first turn of the game.
shouldSkipFirstTurnPhase :: Phase -> Game -> Bool
shouldSkipFirstTurnPhase phase g =
  g.turn == Turn 1
    && g.currentPlayer == g.firstPlayer
    && (phase == QuestPhase || phase == BattlefieldPhase)

-- | Wrap a list of bare card definitions into 'Card's with sequential
-- keys minted starting from the given counter. Returns the next-free
-- key plus the wrapped cards (in input order). Used at game-init to
-- stamp every starting-deck card with a stable identity that survives
-- through hand, play, and discard.
mintCards :: UnitKey -> [SomeCardDef] -> (UnitKey, [Card])
mintCards (UnitKey n0) = go n0 []
  where
    go n acc [] = (UnitKey n, reverse acc)
    go n acc (d : rest) = go (n + 1) (mkCard (UnitKey n) d : acc) rest

newPlayer :: PlayerKey -> Race -> [Card] -> Player
newPlayer k race cards =
  Player
    { key = k
    , state = IdlePlayer
    , capital = newCapital
    , resources = Resources 0
    , hand = []
    , deck = cards
    , discard = []
    , developmentCards = emptyDevelopmentCards
    , race
    , handPlayability = mempty
    }

newGame :: Deck -> Deck -> GameOptions -> Either DeckLoadError Game
newGame deck1 deck2 opts = do
  (race1, defs1) <- loadDeck deck1
  (race2, defs2) <- loadDeck deck2
  let (afterP1, cards1) = mintCards (UnitKey 0) defs1
      (nextKey, cards2) = mintCards afterP1 defs2
      player1 = newPlayer Player1 race1 cards1
      player2 = newPlayer Player2 race2 cards2
  pure $
    Game
      { player1
      , player2
      , firstPlayer = Player1
      , currentPlayer = Player1
      , turn = Turn 0
      , phase = Nothing
      , actionWindow = Nothing
      , actionWindowStack = []
      , pendingPrompt = Nothing
      , modifiers = mempty
      , lifecycle = GameSetup
      , log = []
      , units = []
      , supports = []
      , quests = []
      , legends = []
      , nextUnitKey = nextKey
      , pendingEndOfTurn = []
      , combat = Nothing
      , pendingEndOfPhase = []
      , zoneDamageDrawWatchers = []
      , history = emptyHistory
      , autoSkipActionWindows = opts.autoSkipActionWindows
      , capitalDefenseUsed = mempty
      , pendingUnitDiscount = mempty
      , pendingUnitOnPlayDamage = mempty
      , unitsRedirectedThisTurn = mempty
      , lastResolvedTactic = Nothing
      , pendingActionCancel = mempty
      , developmentPlayedThisTurn = False
      , attackBlockedThisTurn = []
      , lastRevealed = []
      , drawCaps = mempty
      , capitalShields = mempty
      , defenderCounterstrikeBonus = mempty
      , pendingFreeTactic = mempty
      , loyaltyWaivers = mempty
      , tacticDamageContext = Nothing
      , combatDamageUncancellable = False
      }

-- | Host-chosen options that shape engine behavior without altering
-- rules. Currently a single toggle; new options join this record so
-- existing callers stay source-compatible via 'defaultGameOptions'.
data GameOptions = GameOptions
  { autoSkipActionWindows :: Bool
  }

defaultGameOptions :: GameOptions
defaultGameOptions = GameOptions {autoSkipActionWindows = False}

-- | Look up a player record by key.
lookupPlayer :: PlayerKey -> Game -> Player
lookupPlayer Player1 g = g.player1
lookupPlayer Player2 g = g.player2

-- | Replace a player record by key.
setPlayer :: PlayerKey -> Player -> Game -> Game
setPlayer Player1 p g = g {player1 = p}
setPlayer Player2 p g = g {player2 = p}

-- | 'lookupPlayer' lifted into the engine's 'StateT Game' carrier.
getPlayerS :: Monad m => PlayerKey -> StateT Game m Player
getPlayerS pk = gets (lookupPlayer pk)

-- | Apply @f@ to the named player and write the result back. The new
-- player value is observed via 'lookupPlayer' on the current state, so
-- this composes correctly with other in-flight mutations to the same
-- 'Game'.
modifyPlayer :: Monad m => PlayerKey -> (Player -> Player) -> StateT Game m ()
modifyPlayer pk f = modify \g -> setPlayer pk (f (lookupPlayer pk g)) g

-- | Drop the element whose 'key' matches. The dual of 'replaceById'.
-- Works on any keyed in-play record (units, supports, quests, legends).
removeById :: HasField "key" a UnitKey => UnitKey -> [a] -> [a]
removeById k = filter ((/= k) . (.key))

-- | Construct the @InPlay Unit@ wrapper for a freshly-entering unit.
-- Used by the various play paths ('PlayUnit', 'PutUnitIntoPlay', …)
-- so they don't each have to spell out the same 11-field record.
freshUnit :: UnitKey -> PlayerKey -> ZoneKind -> CardDef Unit -> UnitDetails
freshUnit key controller zone cardDef = UnitDetails
  { key
  , controller
  , zone
  , cardDef
  , damage = Damage 0
  , corrupted = False
  , attachments = []
  , experiences = []
  , effectivePower = cardDef.power
  , effectiveMaxHP = unitPrintedHPFromDef cardDef
  , attacking = False
  , defending = False
  , tokens = 0
  , blanked = False
  }

-- | The unit's extras with Witch Hag's Curse blanking applied: a
-- blanked unit's printed text box is treated as empty, so every
-- engine read of its per-card slices goes through the defaults
-- instead. Attachment-granted properties are unaffected (they're
-- other cards' text).
unitExtrasOf :: UnitDetails -> UnitExtras
unitExtrasOf u
  | u.blanked = defaultExtras @'Unit
  | otherwise = u.cardDef.extras

-- | The unit's printed keywords, blank while text-boxed-out.
unitKeywords :: UnitDetails -> [Keyword]
unitKeywords u = if u.blanked then [] else u.cardDef.keywords

-- | The unit's printed action abilities, blank while text-boxed-out.
unitActions :: UnitDetails -> [ActionDef 'Unit]
unitActions u = if u.blanked then [] else u.cardDef.actions

-- | Construct the @InPlay Support@ wrapper for a fresh support. Pass
-- @Just hostKey@ for an attachment, @Nothing@ for a free-standing
-- support.
freshSupport
  :: UnitKey
  -> PlayerKey
  -> ZoneKind
  -> Maybe UnitKey
  -> CardDef Support
  -> SupportDetails
freshSupport key controller zone attachedTo cardDef = SupportDetails
  { key
  , controller
  , zone
  , cardDef
  , attachedTo
  , tokens = 0
  , corrupted = False
  }

-- | Push a card onto the named player's discard pile.
discardToController
  :: Monad m => PlayerKey -> Card -> StateT Game m ()
discardToController pk c = modifyPlayer pk \p -> p {discard = c : p.discard}

-- | Send a departing attachment to its controller's discard pile.
-- Synthetic attachments that are physically unit cards (Vigilant
-- Elector) revert to their unit definition on the way out.
discardAttachment :: Monad m => SupportDetails -> StateT Game m ()
discardAttachment a =
  discardToController a.controller $ mkCard a.key case a.cardDef.extras.revertToUnit of
    Just ucd -> UnitCardDef ucd
    Nothing -> SupportCardDef a.cardDef

-- | Code of the synthetic 'CardDef' minted by 'AnimateDevelopment'
-- (Bolt of Change). Never appears in 'allCards' or a real deck.
animatedDevelopmentCode :: CardCode
animatedDevelopmentCode = "animated-development"

-- | The synthetic unit a Bolt of Change development becomes. Plain
-- statline, no abilities; destroying it destroys a development from
-- its zone (see the 'DestroyUnit' special case).
animatedDevelopmentDef :: Int -> Int -> CardDef Unit
animatedDevelopmentDef pwr hp =
  unitCard animatedDevelopmentCode "Animated Development" do
    cost 0
    power pwr
    hitPoints hp
    body "A development animated into a unit until the end of the turn."

-- | The synthetic Attachment a unit becomes via Vigilant Elector's
-- quest action: same code/title/races as the original card (so
-- uniqueness checks and discard routing stay coherent), carrying the
-- granted text "Attached unit is destroyed at the end of its
-- controller's turn."
transformedUnitAttachmentDef :: CardDef Unit -> CardDef Support
transformedUnitAttachmentDef original =
  supportCard original.code original.title do
    traverse_ race original.races
    cost costN
    loyalty original.loyalty
    trait Attachment
    body "Attached unit is destroyed at the end of its controller's turn."
    revertsToUnit original
    onReceive $ Receive \msg _owner self -> case msg of
      EndTurn pk -> whenJust self.attachedTo \hostKey -> do
        g <- getGame
        case findUnit hostKey g of
          Just host | host.controller == pk -> push (DestroyUnit hostKey)
          _ -> pure ()
      _ -> pure ()
  where
    costN = case original.cost of
      Fixed n -> n
      Variable -> 0

-- | Hydra Blade's destruction ransom: scan the unit's attachments
-- for a 'hostDestroyRansom'; the first one whose controller can pay
-- and agrees saves the host (heal all damage, stay in play). Returns
-- 'True' if the destruction was averted.
ransomHostIfPossible :: UnitDetails -> StateT Game GameT Bool
ransomHostIfPossible u = go u.attachments
  where
    go [] = pure False
    go (a : rest) = case a.cardDef.extras.hostDestroyRansom of
      Nothing -> go rest
      Just n -> do
        g <- get
        let owner = lookupPlayer a.controller g
        if owner.resources >= Resources n
          then do
            yes <-
              askYesNo a.controller $
                "Pay " <> tshow n <> " resources to save "
                  <> T.pack u.cardDef.title
                  <> " from destruction? ("
                  <> T.pack a.cardDef.title
                  <> ")"
            if yes
              then do
                -- Pay and heal synchronously: the post-message
                -- destruction sweep re-checks damage-vs-HP, so the
                -- save has to be visible before this message
                -- finishes processing.
                modifyPlayer a.controller \p ->
                  p {resources = p.resources - Resources n}
                modify \gx ->
                  gx {units = replaceUnit (u {damage = Damage 0} :: UnitDetails) gx.units}
                logIt LogSystem
                  "log.unit.saved"
                  [("card", T.pack u.cardDef.title)]
                pure True
              else go rest
          else go rest

-- | One-shot discount accumulated for the next unit this player plays.
pendingDiscountFor :: PlayerKey -> Game -> Int
pendingDiscountFor pk g = Map.findWithDefault 0 pk g.pendingUnitDiscount

-- | Drop one development from the named zone if 'z' matches.
-- No-op for unrelated zones. Count is clamped non-negative.
decrementDev :: ZoneKind -> Zone -> Zone
decrementDev zk z
  | z.kind == zk =
      let Developments d = z.developments
       in z {developments = Developments (max 0 (d - 1))}
  | otherwise = z

-- | Increment / decrement a 'Zone's development count when its kind
-- matches the @from@ / @to@ pair of a 'MoveDevelopment' message.
-- Untouched zones pass through. Counts are clamped non-negative.
bumpDev :: ZoneKind -> ZoneKind -> Zone -> Zone
bumpDev fromZ toZ z =
  let Developments d = z.developments
      delta
        | z.kind == fromZ && z.kind == toZ = 0
        | z.kind == fromZ = -1
        | z.kind == toZ = 1
        | otherwise = 0
   in z {developments = Developments (max 0 (d + delta))}

-- | Flip a unit's 'corrupted' flag and emit the matching log entry.
-- No-op when the unit is missing or already in the requested state.
setCorrupted :: Bool -> Text -> UnitKey -> StateT Game GameT ()
setCorrupted newVal logKey ukey = do
  munit <- gets (findUnit ukey)
  case munit of
    Just u ->
      when (u.corrupted /= newVal) do
        let u' = (u {corrupted = newVal}) :: UnitDetails
        modify \gx -> gx {units = replaceUnit u' gx.units}
        logIt LogSystem logKey [("card", T.pack u.cardDef.title)]
    Nothing -> do
      -- Supports (free-standing or attached) carry the same flag; the
      -- artefact "Corrupt this card" cost flips it here.
      g <- get
      whenJust (find ((== ukey) . (.key)) (allInPlaySupports g)) \s ->
        when (s.corrupted /= newVal) do
          let s' = (s {corrupted = newVal}) :: SupportDetails
          modify (replaceSupportAnywhere s')
          logIt LogSystem logKey [("card", T.pack s.cardDef.title)]

-- | The "I can pay for it" preamble shared by the vanilla play handlers
-- (PlayUnit, PlaySupport, PlayQuest, PlayLegend, and PlayTactic / …
-- with their own pre-flight checks). Runs the @install@ body with the
-- card def, the player record after the cost has been debited, and the
-- final cost. Silently no-ops on missing card, failed 'canPlayCard',
-- 'Variable' cost, or insufficient resources.
withPaidPlay
  :: PlayerKey
  -> (Player -> Maybe (CardDef k, Player))
  -> (Game -> CardDef k -> Int)
  -> (CardDef k -> Player -> Int -> StateT Game GameT ())
  -> StateT Game GameT ()
withPaidPlay pk extract costFn install = do
  g <- get
  -- Hard rule: unit / support / quest / attachment / legend plays
  -- are gated to the active player's CapitalActionWindow. Without
  -- this check the engine would accept the message any time, even
  -- though the frontend's playability hint correctly hides it.
  -- Cards carrying 'PlayAnytime' (Nordland Halberdiers) instead use
  -- tactic timing: any window where the player holds priority.
  let player = lookupPlayer pk g
  whenJust (extract player) \(cardDef, playerWithoutCard) -> do
    let windowOk =
          canPlayNonTactic pk g
            || (PlayAnytime `elem` cardDef.keywords && canPlayTactic pk g)
    when (windowOk && canPlayCard pk cardDef g) case cardDef.cost of
      Variable -> pure ()
      Fixed _ -> do
        let n = costFn g cardDef
        when (player.resources >= Resources n) do
          markPlayedLimited cardDef
          recordEvent \h -> h
            { playedBy =
                Map.insertWith (<>) pk [cardCodeFilter cardDef] h.playedBy
            }
          let paidPlayer = playerWithoutCard
                { resources = player.resources - Resources n
                }
          install cardDef paidPlayer n

-- | The active player may play units / supports / quests / legends
-- only during their own 'CapitalActionWindow', and only while they
-- hold priority. Tactics use 'canPlayTactic' below.
canPlayNonTactic :: PlayerKey -> Game -> Bool
canPlayNonTactic pk g
  | g.currentPlayer /= pk = False
  | otherwise = case g.actionWindow of
      Nothing -> False
      Just aw ->
        priorityHolder aw.awaiting == pk
          && aw.trigger == CapitalActionWindow

-- | Tactics are playable in any action window where the player holds
-- priority — no phase restriction.
canPlayTactic :: PlayerKey -> Game -> Bool
canPlayTactic pk g = case g.actionWindow of
  Nothing -> False
  Just aw -> priorityHolder aw.awaiting == pk

-- | A 'Pile' is a getter/setter pair for one of a 'Player'\'s card
-- collections. Lets 'takeFromPile' work uniformly across hand, deck
-- and discard.
data Pile = Pile
  { read :: Player -> [Card]
  , write :: [Card] -> Player -> Player
  }

handPile, deckPile, discardPile :: Pile
handPile = Pile (.hand) \cs p -> p {hand = cs}
deckPile = Pile (.deck) \cs p -> p {deck = cs}
discardPile = Pile (.discard) \cs p -> p {discard = cs}

-- | Pull a card matching the given key from a 'Pile', if its 'def' is
-- of the requested kind. Returns the unwrapped definition and the
-- player with the card removed (pile order preserved otherwise).
takeFromPile
  :: Pile
  -> (SomeCardDef -> Maybe (CardDef k))
  -> UnitKey
  -> Player
  -> Maybe (CardDef k, Player)
takeFromPile pile asKind key p = go [] (pile.read p)
  where
    go _ [] = Nothing
    go acc (c : rest)
      | c.key == key, Just cd <- asKind c.def =
          Just (cd, pile.write (reverse acc ++ rest) p)
      | otherwise = go (c : acc) rest

takeUnitFromHand :: UnitKey -> Player -> Maybe (CardDef Unit, Player)
takeUnitFromHand = takeFromPile handPile asUnit

takeSupportFromHand :: UnitKey -> Player -> Maybe (CardDef Support, Player)
takeSupportFromHand = takeFromPile handPile asSupport

takeSupportFromDeck :: UnitKey -> Player -> Maybe (CardDef Support, Player)
takeSupportFromDeck = takeFromPile deckPile asSupport

takeQuestFromHand :: UnitKey -> Player -> Maybe (CardDef Quest, Player)
takeQuestFromHand = takeFromPile handPile asQuest

-- | Fire one scheduled effect by translating it into messages.
firePendingEffect :: PendingEffect -> StateT Game GameT ()
firePendingEffect = \case
  PEDealDamageToUnit ukey n -> send $ DealDamageToUnit ukey n
  PESacrificeAttackersThisPhase -> do
    g <- get
    traverse_ (send . DestroyUnit) (historyOfScope ThisPhase g).attackersDeclared
  PEDestroyUnit ukey -> send (DestroyUnit ukey)
  PEGiveControl ukey pk -> send (TakeControlOfUnit pk ukey)
  PEReturnUnitToDeckBottom ukey -> do
    g <- get
    whenJust (findUnit ukey g) \u -> do
      -- Necromancy: move the unit (and its attachments fall off) from
      -- play to the bottom of its controller's deck. Not a destruction.
      for_ u.attachments discardAttachment
      let player = lookupPlayer u.controller g
          card = Card {key = ukey, def = UnitCardDef u.cardDef}
          player' = player {deck = player.deck <> [card]}
      modify \gx ->
        (setPlayer u.controller player' gx) {units = removeById ukey gx.units}
      logIt LogSystem
        "log.unit.necromancy_returned"
        [("player", playerParam u.controller), ("card", T.pack u.cardDef.title)]
  PERemoveAnimatedUnit ukey -> do
    g <- get
    whenJust (findUnit ukey g) \u ->
      when (u.cardDef.code == animatedDevelopmentCode) do
        modify \gx -> gx {units = removeById ukey gx.units}
        logIt LogSystem
          "log.unit.deanimated"
          [ ("player", playerParam u.controller)
          , ("zone", zoneParam u.zone)
          ]

-- | The damage a single unit contributes in combat. Adds card-specific
-- bonuses (Lord of Khorne self-burning, Rift of Battle, …) on top of
-- the cached effective power. Per-card slices live on
-- 'UnitExtras.combatPowerBonus' (self) and 'SupportExtras.supportCombatBonus'
-- (every in-play support — free-standing or attached — gets to
-- contribute).
combatDamageOf :: Game -> PlayerKey -> UnitDetails -> Int
combatDamageOf g side u =
  max 0
    ( u.effectivePower
        + (unitExtrasOf u).combatPowerBonus g u
        + sum
            [ s.cardDef.extras.supportCombatBonus g s u
            | s <- allInPlaySupports g
            ]
        + modifierCombatBonus
        - runeOfFortitudePenalty
    )
  where
    isAttacker = case g.combat of
      Just cs -> side == cs.attackingPlayer && u.key `elem` cs.attackers
      Nothing -> False

    -- "+N damage in combat" modifiers (Naggaroth Spearmen).
    modifierCombatBonus =
      let mods = fromMaybe [] (Map.lookup (UnitRef u.key) g.modifiers)
       in sum [n | Modifier (GainCombatDamage n) _ <- mods]

    -- Rune of Fortitude (core-013): if BeginCombat couldn't charge
    -- the per-attacker tax, every attacker eats -1 power for this
    -- combat. The penalty lives on the in-flight CombatState.
    runeOfFortitudePenalty
      | isAttacker = case g.combat of
          Just cs -> cs.attackerPowerPenalty
          Nothing -> 0
      | otherwise = 0

-- | Card-aware attacker eligibility check at 'BeginCombat'. Returns
-- 'True' if the unit can attack the named defender zone right now.
-- Hard rules:
--   * Only units in one of their eligible attack zones can attack
--     (battlefield by default; Greyseer Thanquol and Dragonslayer
--     widen theirs).
--   * Corrupt units cannot attack.
--   * Units modifier-tagged 'CannotAttack' cannot attack.
-- Then the per-card 'canAttackZone' slice ('Sworn of Khorne', etc.)
-- gets the last word.
eligibleAttacker :: Game -> PlayerKey -> ZoneKind -> UnitKey -> Bool
eligibleAttacker g defender zone ukey = case findUnit ukey g of
  Nothing -> False
  Just u ->
    (u.zone `elem` (unitExtrasOf u).attackEligibleZones || rovingAttacker u ukey)
      && not u.corrupted
      && not (hasModifier g.modifiers ukey CannotAttack)
      && (unitExtrasOf u).canAttackZone g defender zone u
  where
    -- A unit questing on a Sack Tor Aendris-style quest may attack as
    -- though it were in its controller's battlefield.
    rovingAttacker u k =
      any
        ( \q ->
            q.controller == u.controller
              && q.cardDef.extras.questerAttacksAnyZone
              && q.questingUnit == Just k
        )
        g.quests

-- | Prompt the named player to place 'remaining' indirect-damage
-- points one at a time, respecting the slack-vs-HP cap and skipping
-- burned zones. Returns the per-zone tally; the caller turns that
-- into actual 'DealDamageToZone' messages. Falls back gracefully on
-- a declined / illegal prompt response by stopping the loop (any
-- unallocated remainder is simply not assigned).
collectIndirect
  :: PlayerKey
  -> Int
  -> Map.Map ZoneKind Int
  -> StateT Game GameT (Map.Map ZoneKind Int)
collectIndirect pk remaining acc
  | remaining <= 0 = pure acc
  | otherwise = do
      g <- get
      let p = lookupPlayer pk g
          cap = p.capital
          baseZones =
            [ (KingdomZone, cap.kingdom)
            , (QuestZone, cap.quest)
            , (BattlefieldZone, cap.battlefield)
            ]
          accFor zk = Map.findWithDefault 0 zk acc
          slackOf z =
            let HitPoints hp = z.hitPoints
                Damage d = z.damage
             in hp - d
          eligible =
            [ zk
            | (zk, z) <- baseZones
            , not z.burned
            , slackOf z - accFor zk > 0
            ]
          bump zk = collectIndirect pk (remaining - 1) (Map.insertWith (+) zk 1 acc)
      case eligible of
        [] -> pure acc
        [only] -> bump only
        many -> do
          ans <- askPrompt Prompt
            { player = pk
            , kind = ChooseTargetOption
                { options = [TargetZoneOption pk zk | zk <- many]
                , description =
                    "Place 1 indirect damage ("
                      <> tshow remaining
                      <> " left)."
                }
            , callback = CallbackInlinePrompt
            }
          case ans of
            PickTargetOption (TargetZoneOption owner zk)
              | owner == pk, zk `elem` many ->
                  bump zk
            _ -> pure acc

-- | Once-per-turn "redirect the first point of damage done to your
-- capital each turn" defenses (Defend the Border with 3+ tokens).
-- For each eligible quest the damaged player controls that hasn't
-- fired this turn, 1 point of the incoming damage is redirected to a
-- unit or capital section of the defender's choice. Eligibility is
-- evaluated live (not pre-armed at turn start) so the defense works
-- on the opponent's turn — when attacks actually happen. Returns the
-- damage remaining after redirects.
applyCapitalRedirects
  :: PlayerKey -> ZoneKind -> Int -> StateT Game GameT Int
applyCapitalRedirects pk zoneKind = go
  where
    go remaining
      | remaining <= 0 = pure remaining
      | otherwise = do
          g <- get
          let used k = Map.findWithDefault 0 k g.capitalDefenseUsed > 0
              eligible =
                [ q
                | q <- g.quests
                , q.controller == pk
                , q.cardDef.extras.capitalRedirectFirstDamage g q
                , not (used q.key)
                ]
              unitOpts = [TargetUnitOption u.key | u <- g.units]
              zoneOpts =
                [ TargetZoneOption p.key z.kind
                | p <- [g.player1, g.player2]
                , z <- p.capital.zones
                , not z.burned
                , not (p.key == pk && z.kind == zoneKind)
                ]
              opts = unitOpts <> zoneOpts
          case (eligible, opts) of
            (q : _, _ : _) -> do
              modify \gx -> gx
                { capitalDefenseUsed =
                    Map.insertWith (+) q.key 1 gx.capitalDefenseUsed
                }
              ans <- askPrompt Prompt
                { player = pk
                , kind = ChooseTargetOption
                    { options = opts
                    , description =
                        "Redirect 1 damage from your capital to a unit or capital section."
                    }
                , callback = CallbackInlinePrompt
                }
              let chosen = case ans of
                    PickTargetOption c | c `elem` opts -> c
                    _ -> head opts
              case chosen of
                TargetUnitOption uk -> send (DealDamageToUnit uk 1)
                TargetZoneOption owner zk -> send (DealDamageToZone owner zk 1)
                TargetSupportOption _ -> pure ()
              logIt LogSystem
                "log.capital.redirected"
                [ ("player", playerParam pk)
                , ("card", T.pack q.cardDef.title)
                ]
              go (remaining - 1)
            _ -> pure remaining

-- | Consume armed capital-shield grants (Flagellants, Gifts of
-- Aenarion) against inbound capital damage, oldest grant first.
-- Refund-bearing grants credit their owner per point cancelled.
-- Returns the damage remaining.
consumeCapitalShieldGrants :: PlayerKey -> Int -> StateT Game GameT Int
consumeCapitalShieldGrants pk = go
  where
    go inbound
      | inbound <= 0 = pure inbound
      | otherwise = do
          g <- get
          case Map.findWithDefault [] pk g.capitalShields of
            [] -> pure inbound
            (grant : rest) -> do
              let absorbed = case grant.points of
                    Nothing -> inbound
                    Just n -> min n inbound
                  remainder = case grant.points of
                    Nothing -> [grant]
                    Just n
                      | n - absorbed > 0 ->
                          [CapitalShieldGrant (Just (n - absorbed)) grant.refundPer]
                      | otherwise -> []
              modify \gx ->
                gx
                  { capitalShields =
                      Map.insert pk (remainder <> rest) gx.capitalShields
                  }
              when (absorbed > 0) do
                logIt LogSystem
                  "log.capital.warded"
                  [("player", playerParam pk), ("amount", tshow absorbed)]
                when (grant.refundPer > 0) $
                  send $ GainResources pk (absorbed * grant.refundPer)
              if absorbed > 0
                then go (inbound - absorbed)
                else pure inbound

-- | Once-per-turn "cancel 1 damage to your capital each turn"
-- defenses (Contested Fortress). Each eligible support the damaged
-- player controls absorbs 1 point per turn. Returns the damage
-- remaining after cancellation.
applyCapitalShields
  :: PlayerKey -> ZoneKind -> Int -> StateT Game GameT Int
applyCapitalShields pk zoneKind = go
  where
    go remaining
      | remaining <= 0 = pure remaining
      | otherwise = do
          g <- get
          let used k = Map.findWithDefault 0 k g.capitalDefenseUsed > 0
              eligible =
                [ s
                | s <- allInPlaySupports g
                , s.controller == pk
                , s.cardDef.extras.capitalShieldPerTurn
                , not (used s.key)
                ]
          case eligible of
            [] -> pure remaining
            (s : _) -> do
              modify \gx -> gx
                { capitalDefenseUsed =
                    Map.insertWith (+) s.key 1 gx.capitalDefenseUsed
                }
              logIt LogSystem
                "log.capital.shielded"
                [ ("player", playerParam pk)
                , ("zone", zoneParam zoneKind)
                , ("amount", tshow (1 :: Int))
                ]
              go (remaining - 1)

-- | Units the named player may legally declare as defenders of the
-- given zone right now. Excludes corrupt units, anything carrying a
-- 'CannotDefend' modifier, and printed can't-defend units (Clan
-- Moulder's Elite). Units questing on a Protect the Empire-style
-- quest may defend any of their controller's zones, so they join the
-- pool regardless of the attacked zone.
eligibleDefenderCandidates :: Game -> PlayerKey -> ZoneKind -> [UnitKey]
eligibleDefenderCandidates g defender zone =
  [ u.key
  | u <- g.units
  , u.controller == defender
  , u.zone == zone || rovingDefender u.key
  , not u.corrupted
  , not (hasModifier g.modifiers u.key CannotDefend)
  , not (unitExtrasOf u).cannotDefend
  ]
  where
    rovingDefender k =
      any
        ( \q ->
            q.controller == defender
              && q.cardDef.extras.questerDefendsAnyZone
              && q.questingUnit == Just k
        )
        g.quests

-- | Prompt the named player for a damage-assignment order over the
-- supplied keys. Returns the chosen permutation, falling back to the
-- input order on a degenerate / declined / illegal answer. Skips the
-- prompt when there's nothing to choose (0 or 1 recipients).
promptAssignmentOrder
  :: PlayerKey
  -> Text
  -> [UnitKey]
  -> StateT Game GameT [UnitKey]
promptAssignmentOrder pk desc = \case
  [] -> pure []
  [single] -> pure [single]
  many -> do
    let n = length many
    ans <- askPrompt Prompt
      { player = pk
      , kind = ChooseUnits
          { filterSpec = UnitsFromList many
          , minPick = n
          , maxPick = n
          , description = desc
          }
      , callback = CallbackInlinePrompt
      }
    pure $ case ans of
      PickUnits picks
        | length picks == n && all (`elem` many) picks && allUnique picks ->
            picks
      _ -> many
  where
    allUnique xs = length xs == length (nubKeys xs)
    nubKeys = foldr (\x acc -> if x `elem` acc then acc else x : acc) []

-- | Reorder 'reference' keys by a player-supplied order, dropping any
-- key in the order that doesn't appear in the reference list (defensive
-- against stale client input) and appending any reference keys the
-- player didn't include (so the allocator still sees every recipient).
orderedRecipients :: Game -> [UnitKey] -> [UnitKey] -> [UnitDetails]
orderedRecipients g order reference =
  let refSet = filter (`elem` reference) order
      tail' = filter (`notElem` refSet) reference
   in mapMaybe (`findUnit` g) (refSet <> tail')

-- | True iff the named unit currently carries the given atomic
-- modifier in 'Game.modifiers'.
hasModifier :: Map.Map (Ref 'Target) [Modifier] -> UnitKey -> ModifierDetails -> Bool
hasModifier mods ukey d =
  any (\m -> m.details == d) (fromMaybe [] (Map.lookup (UnitRef ukey) mods))

-- | Split the combat damage contributed by a list of units into a
-- (cancellable, uncancellable) pair. Damage from units with the
-- 'DamageCannotBeCancelled' keyword — or whose attached supports
-- grant that property (e.g. Hammer of Sigmar) — goes into the
-- uncancellable bucket. While Mob Up's rider is active, all combat
-- damage is uncancellable.
splitDamage :: Game -> PlayerKey -> [UnitDetails] -> (Int, Int)
splitDamage g side units =
  foldr step (0, 0) units
  where
    step u (c, n) =
      let d = combatDamageOf g side u
       in if g.combatDamageUncancellable || hasUncancellableDamage u
            then (c, n + d)
            else (c + d, n)

-- | True if this unit's damage is uncancellable, accounting for both
-- its printed keywords and any attached supports that grant that
-- property (via 'SupportExtras.grantsUncancellableDamage').
hasUncancellableDamage :: UnitDetails -> Bool
hasUncancellableDamage u =
  DamageCannotBeCancelled `elem` unitKeywords u
    || any (.cardDef.extras.grantsUncancellableDamage) u.attachments

-- | Apply a cancellable + uncancellable damage budget to a list of
-- units, in order. Cancellable damage is offered first so Toughness
-- absorbs as much of it as possible before any lands; uncancellable
-- damage then fills in the remaining slack.
--
-- Returns the budget left over (cancellable, uncancellable) after
-- every defender has been processed — that's what the caller sends
-- on to the zone as spillover.
applyDamageToUnitsSplit
  :: Game
  -> Int
  -- ^ cancellable budget
  -> Int
  -- ^ uncancellable budget
  -> [UnitDetails]
  -> StateT Game GameT (Int, Int)
applyDamageToUnitsSplit g = go
  where
    go cAvail uAvail [] = pure (cAvail, uAvail)
    go 0 0 _ = pure (0, 0)
    go cAvail uAvail (u : rest) = do
      let Damage existing = u.damage
          slack = max 0 (u.effectiveMaxHP - existing)
          tough = totalToughness g u
          -- Cancellable budget absorbed: enough to fill (slack +
          -- toughness) tokens of assignment, capped by what's
          -- available. The first 'tough' of that is cancelled; the
          -- rest lands.
          cancellableUsed = min cAvail (slack + tough)
          landingFromCancellable = max 0 (cancellableUsed - tough)
          slackAfterCancellable = slack - landingFromCancellable
          uncancellableUsed = min uAvail slackAfterCancellable
      when (cancellableUsed > 0) $
        send $ DealDamageToUnit u.key cancellableUsed
      when (uncancellableUsed > 0) $
        send $ DealDamageToUnitUncancellable u.key uncancellableUsed
      go (cAvail - cancellableUsed) (uAvail - uncancellableUsed) rest

-- | Read a zone from a 'Capital' by 'ZoneKind'.
getZone :: ZoneKind -> Player -> Zone
getZone kind p = case kind of
  KingdomZone -> p.capital.kingdom
  QuestZone -> p.capital.quest
  BattlefieldZone -> p.capital.battlefield

-- | Replace a zone within a player's capital, preserving the other two.
setZone :: ZoneKind -> Zone -> Player -> Player
setZone kind z p =
  let c = p.capital
      c' = case kind of
        KingdomZone -> c {kingdom = z}
        QuestZone -> c {quest = z}
        BattlefieldZone -> c {battlefield = z}
   in p {capital = c'}

-- | Wire-side enum encoding for 'ZoneKind' (mirrors 'playerParam').
zoneParam :: ZoneKind -> Text
zoneParam = tshow

-- | Apply any in-play passive damage multipliers to a raw damage
-- amount. Driven by the per-card 'damageMultiplierWhileInPlay' slice
-- on 'UnitExtras'; the strongest in-play multiplier wins (Bloodletter
-- gives 2; default is 1; duplicate copies don't stack).
applyDamageMultipliers :: Game -> Int -> Int
applyDamageMultipliers g amount =
  amount
    * maximum (1 : map (\u -> (unitExtrasOf u).damageMultiplierWhileInPlay) g.units)

-- | While a tactic's queued damage messages drain
-- ('Game.tacticDamageContext' names the player who played it),
-- supports like Hellcannon Reserves add to each damage event.
amplifyTacticDamage :: Game -> Int -> Int
amplifyTacticDamage g amount
  | amount <= 0 = amount
  | otherwise = case g.tacticDamageContext of
      Nothing -> amount
      Just pk ->
        amount
          + sum
              [ s.cardDef.extras.tacticDamageBonus g s pk
              | s <- g.supports
              ]

-- | Consume 'DamageShield' / 'RedirectShield' modifiers sitting on
-- the unit, oldest first, against an inbound cancellable damage
-- amount. Shields cancel; redirect shields re-deal their claimed
-- points to the carried target (a shield whose target has left play
-- is dropped without absorbing). Remaining shield budgets are
-- rewritten in place. Returns the damage left to land.
consumeDamageShields :: UnitKey -> Int -> StateT Game GameT Int
consumeDamageShields ukey = go
  where
    go inbound
      | inbound <= 0 = pure inbound
      | otherwise = do
          g <- get
          let mods = fromMaybe [] (Map.lookup (UnitRef ukey) g.modifiers)
              isShield m = case m.details of
                DamageShield n -> n > 0
                RedirectShield n _ -> n > 0
                _ -> False
          case break isShield mods of
            (_, []) -> pure inbound
            (before, shield : after) -> do
              (absorbed, replacement, redirectTo) <- case shield.details of
                DamageShield n ->
                  let used = min n inbound
                      rest = n - used
                   in pure
                        ( used
                        , [Modifier (DamageShield rest) shield.scope | rest > 0]
                        , Nothing
                        )
                RedirectShield n dst
                  | isJust (findUnit dst g) ->
                      let used = min n inbound
                          rest = n - used
                       in pure
                            ( used
                            , [Modifier (RedirectShield rest dst) shield.scope | rest > 0]
                            , Just dst
                            )
                  | otherwise ->
                      -- Redirect target has left play: the shield
                      -- expires without absorbing anything.
                      pure (0, [], Nothing)
                _ -> pure (0, [shield], Nothing)
              modify \gx ->
                gx
                  { modifiers =
                      Map.insert (UnitRef ukey) (before <> replacement <> after) gx.modifiers
                  }
              whenJust redirectTo \dst ->
                when (absorbed > 0) do
                  logIt LogSystem
                    "log.damage.redirected"
                    [("amount", tshow absorbed)]
                  send (DealDamageToUnit dst absorbed)
              go (inbound - absorbed)

-- | A wrapper over the four card-kinds that can host an action
-- ability. Used to dispatch a 'TriggerCardAction' message uniformly
-- regardless of the source kind.
data ActionSource
  = UnitSource UnitDetails
  | SupportSource SupportDetails
  | QuestSource QuestDetails
  | LegendSource LegendDetails

-- | Look up an in-play card by 'UnitKey' across units, supports,
-- quests and legends. First hit wins; the card kinds use disjoint
-- key spaces in practice so the order only matters for malformed input.
findActionSource :: UnitKey -> Game -> Maybe ActionSource
findActionSource k g =
      (UnitSource <$> findUnit k g)
  -- Includes attached supports — an Attachment artefact (Windcatcher
  -- Prism, Star Crown Fragments, Eye of Sheerian) carries an Action
  -- ability that fires while it's attached to its host.
  <|> (SupportSource <$> find ((== k) . (.key)) (allInPlaySupports g))
  <|> (QuestSource <$> findQuest k g)
  <|> (LegendSource <$> findLegend k g)

-- | Run a polymorphic action over the wrapped in-play record, no matter
-- which kind it is. Lets us write @actionAt@, @actionSourceTitle@, etc.
-- once instead of casing on every 'ActionSource' constructor.
withActionSource
  :: ActionSource
  -> ( forall a k
       . ( HasField "controller" a PlayerKey
         , HasField "cardDef" a (CardDef k)
         , InPlay k ~ a
         )
      => a -> r
     )
  -> r
withActionSource src f = case src of
  UnitSource u -> f u
  SupportSource s -> f s
  QuestSource q -> f q
  LegendSource l -> f l

-- | Metadata for the action at the given index on a source. Looks up
-- the static action list printed on the card; runtime-evaluated
-- availability ('availableInZone') is enforced by the caller before
-- firing.
actionAt :: ActionSource -> Int -> Maybe (Text, Int, TargetSchema)
actionAt src i = withActionSource src \a ->
  meta <$> safeIndex a.cardDef.actions i
  where
    meta a = (a.actionName, a.actionCost, a.actionTarget)

-- | Resolve the 'availableInZone' gate on the action at the given
-- index. Returns 'True' iff the action either has no zone gate or its
-- host card is currently in the gated zone. Non-Unit sources have no
-- zone (yet) and pass through.
actionAvailableHere :: ActionSource -> Int -> Bool
actionAvailableHere src i = withActionSource src \a ->
  case safeIndex a.cardDef.actions i of
    Nothing -> False
    Just def -> case def.availableInZone of
      Nothing -> True
      Just z -> case src of
        UnitSource u -> u.zone == z
        _ -> True

-- | Non-resource costs the action at the given index imposes. Empty
-- list when the action has no extra costs or the index is invalid.
actionExtraCostsAt :: ActionSource -> Int -> [ExtraCost]
actionExtraCostsAt src i = withActionSource src \a ->
  maybe [] (.actionExtraCosts) (safeIndex a.cardDef.actions i)

-- | Validate that every non-resource cost can be paid by 'pk' given
-- the current game state. 'SacrificeUnit' means the player controls
-- at least one unit; the self-referencing costs check the hosting
-- card directly.
canPayExtras :: PlayerKey -> UnitKey -> [ExtraCost] -> Game -> Bool
canPayExtras pk srcKey extras g = all (canPayExtra pk srcKey g) extras

canPayExtra :: PlayerKey -> UnitKey -> Game -> ExtraCost -> Bool
canPayExtra pk _srcKey g SacrificeUnit =
  any (\u -> u.controller == pk) g.units
canPayExtra _pk srcKey g SacrificeSelf =
  isJust (findUnit srcKey g)
    || isJust (findSupport srcKey g)
    || any (any ((== srcKey) . (.key)) . (.attachments)) g.units
canPayExtra _pk srcKey g CorruptSelf =
  maybe
    (maybe False (not . (.corrupted)) (find ((== srcKey) . (.key)) (allInPlaySupports g)))
    (not . (.corrupted))
    (findUnit srcKey g)
canPayExtra pk _srcKey g SacrificeDevelopment =
  any (\z -> case z.developments of Developments n -> n > 0)
    (playerOf pk g).capital.zones

-- | Pay every non-resource cost, prompting the player for choices
-- where needed. Returns the list of 'Payment' receipts on success,
-- or 'Nothing' if any payment fails (e.g. the player declined a
-- sacrifice prompt). The caller skips firing the effect on failure.
payExtras
  :: PlayerKey -> UnitKey -> [ExtraCost] -> StateT Game GameT (Maybe [Payment])
payExtras pk srcKey = go []
  where
    go acc [] = pure (Just (reverse acc))
    go acc (SacrificeUnit : rest) = do
      answer <- askPrompt Prompt
        { player = pk
        , kind = ChooseUnits
            { filterSpec = AnyOwnUnit
            , minPick = 1
            , maxPick = 1
            , description = "Sacrifice a unit (cost)."
            }
        , callback = CallbackInlinePrompt
        }
      case answer of
        PickUnits (chosen : _) -> do
          g <- get
          case findUnit chosen g of
            Just u | u.controller == pk -> do
              send (DestroyUnit u.key)
              go (SacrificedUnit u.key : acc) rest
            _ -> pure Nothing
        _ -> pure Nothing
    go acc (SacrificeSelf : rest) = do
      g <- get
      if isJust (findUnit srcKey g)
        then do
          send (DestroyUnit srcKey)
          go (SacrificedUnit srcKey : acc) rest
        else do
          -- Supports (free-standing or attached) sacrifice via the
          -- support-destruction path.
          send (DestroySupport srcKey)
          go (SacrificedUnit srcKey : acc) rest
    go acc (CorruptSelf : rest) = do
      send (CorruptUnit srcKey)
      go (CorruptedSelf : acc) rest
    go acc (SacrificeDevelopment : rest) = do
      g <- get
      let devZones =
            [ z.kind
            | z <- (playerOf pk g).capital.zones
            , case z.developments of Developments n -> n > 0
            ]
      case devZones of
        [] -> pure Nothing
        [zk] -> do
          send (DestroyDevelopment pk zk)
          go (SacrificedDevelopment zk : acc) rest
        _ -> do
          ans <- askPrompt Prompt
            { player = pk
            , kind = ChooseTargetOption
                { options = [TargetZoneOption pk zk | zk <- devZones]
                , description = "Sacrifice a development (cost)."
                }
            , callback = CallbackInlinePrompt
            }
          case ans of
            PickTargetOption (TargetZoneOption owner zk)
              | owner == pk, zk `elem` devZones -> do
                  send (DestroyDevelopment pk zk)
                  go (SacrificedDevelopment zk : acc) rest
            _ -> pure Nothing

-- | The card title for log lines.
actionSourceTitle :: ActionSource -> String
actionSourceTitle src = withActionSource src (.cardDef.title)

-- | Each card's actions are "controlled by" its controller — only that
-- player can trigger them.
validateActionSource :: PlayerKey -> ActionSource -> Bool
validateActionSource pk src = withActionSource src \a -> a.controller == pk

-- | Who may trigger the action at the given index: the controller by
-- default, the controller's opponent for actions marked
-- 'actionOpponentOnly' (Morathi's Pegasus).
validateActionTriggerer :: PlayerKey -> ActionSource -> Int -> Bool
validateActionTriggerer pk src i = withActionSource src \a ->
  case safeIndex a.cardDef.actions i of
    Nothing -> False
    Just def
      | def.actionOpponentOnly -> a.controller.next == pk
      | otherwise -> a.controller == pk

-- | Fire the chosen action's effect closure. No-op if the index is out
-- of bounds. The zone-availability check ('availableInZone') is the
-- caller's responsibility — 'TriggerCardAction' enforces it via
-- 'actionAvailableHere'.
fireAction
  :: ActionSource
  -> Int
  -> PlayerKey
  -> ActionTarget
  -> [Payment]
  -> StateT Game GameT ()
fireAction src i pk tgt payments = withActionSource src \self ->
  whenJust (safeIndex self.cardDef.actions i) \a ->
    case a.actionEffect of
      ActionEffect f ->
        lift $ f ActionUsage {user = pk, self, target = tgt, payments}

safeIndex :: [a] -> Int -> Maybe a
safeIndex xs i
  | i < 0 = Nothing
  | otherwise = case drop i xs of
      (x : _) -> Just x
      [] -> Nothing

-- | Type-indexed accessors for the per-kind 'cardDef.actions' field on
-- the metadata side. Used so the schema for tactic targets lives in
-- the same place as actions on in-play cards.
tacticTargetSchema :: CardDef Tactic -> TargetSchema
tacticTargetSchema cd = case cd.actions of
  (a : _) -> a.actionTarget
  [] -> NoTargetSchema

-- | Check that an 'ActionTarget' satisfies a 'TargetSchema' against
-- the current game state, from the perspective of the player firing.
validateTarget :: PlayerKey -> TargetSchema -> ActionTarget -> Game -> Bool
validateTarget pk schema tgt g = case (schema, tgt) of
  (NoTargetSchema, NoTarget) -> True
  (AnyUnitTargetSchema, TargetUnit k) -> isJust $ findUnit k g
  (EnemyUnitTargetSchema, TargetUnit k) -> maybe False ((/= pk) . (.controller)) $ findUnit k g
  (FriendlyUnitTargetSchema, TargetUnit k) -> maybe False ((== pk) . (.controller)) $ findUnit k g
  (AnyZoneTargetSchema, TargetZone _ _) -> True
  (EnemyZoneTargetSchema, TargetZone owner _) -> owner /= pk
  (SupportTargetSchema, TargetSupport k) -> isJust $ findSupport k g
  _ -> False

-- | Total power available to the named player in the named zone:
-- base power printed on the capital board, plus the power icons on
-- every unit/support/legend currently in the zone, plus any
-- zone-targeting aura bonuses (e.g. Lighthouse of Lothern).
zonePower :: Game -> PlayerKey -> ZoneKind -> Int
zonePower g pk zone =
  let Power base = basePower zone
      mine
        :: ( HasField "controller" a PlayerKey
           , HasField "zone" a ZoneKind
           )
        => [a] -> [a]
      mine = filter \x -> x.controller == pk && x.zone == zone
      -- Corruption does NOT suppress power: per the rulebook, corrupt
      -- cards "cannot be declared as attackers or defenders" and
      -- nothing more — they still produce resources and draw.
      unitPow = sum $ map (.effectivePower) $ mine g.units
      supportPow = sum $ map (.cardDef.power) $ mine g.supports
      legendPow = sum $ map (.cardDef.power) $ mine g.legends
   in base + unitPow + supportPow + legendPow + zoneAuraBonus g pk zone

-- | Extra power a player's zone gets from in-play cards that grant a
-- zone-wide bonus. Driven by the per-support 'zonePowerBonus' slice
-- on 'SupportExtras' (Lighthouse of Lothern, Rift of Chaos, …); each
-- support reports its contribution for the queried (controller, zone)
-- pair.
zoneAuraBonus :: Game -> PlayerKey -> ZoneKind -> Int
zoneAuraBonus g pk zone =
  sum
    [ s.cardDef.extras.zonePowerBonus g s zone
    | s <- g.supports
    , s.controller == pk
    ]

-- | Compute and STAGE damage assignments for the in-flight combat
-- without yet committing them. The pending list lives on
-- 'CombatState.pendingAssignments'; cancellation effects (Defenders
-- of the Faith, Master Rune of Valaya) can mutate it during the
-- AfterAssignCombatDamage window. 'commitPendingCombatDamage' is the
-- counterpart that turns each entry into a real DealDamage message.
--
-- The two ordering arguments come from per-side player prompts:
--   * @defenderOrder@ — attacker's chosen order of defenders
--   * @attackerOrder@ — defender's chosen order of attackers
-- An empty / partial order falls back to the side's own combat list.
assignCombatDamage
  :: Game
  -> CombatState
  -> [UnitKey]
  -- ^ defender-receiving order (attacker-chosen)
  -> [UnitKey]
  -- ^ attacker-receiving order (defender-chosen)
  -> StateT Game GameT ()
assignCombatDamage g cs defenderOrder attackerOrder = do
  let attackerUnits = mapMaybe (`findUnit` g) cs.attackers
      defenderUnits = mapMaybe (`findUnit` g) cs.defenders
      -- Juvenile Wyvern: a defender that deals its combat damage to
      -- ALL attackers leaves the pooled allocation and instead lands
      -- a full hit on every attacking unit.
      (broadsiders, pooledDefenders) =
        partition
          (\u -> (unitExtrasOf u).defenderDamageToAllAttackers)
          defenderUnits
      defenderRecipients =
        orderedRecipients g defenderOrder cs.defenders
      attackerRecipients =
        orderedRecipients g attackerOrder cs.attackers
      defendingLegend = case cs.targetLegend of
        Just _ -> legendOf cs.defendingPlayer g
        Nothing -> Nothing
      defendingLegendPow = maybe 0 (.cardDef.power) defendingLegend
      (attackerCanc, attackerUncanc) =
        splitDamage g cs.attackingPlayer attackerUnits
      (defenderUnitCanc, defenderUncanc) =
        splitDamage g cs.defendingPlayer pooledDefenders
      broadsideEntries =
        [ PendingDamage
            { target = PDUnit a.key
            , cancellable = c
            , uncancellable = nc
            }
        | w <- broadsiders
        , let d = combatDamageOf g cs.defendingPlayer w
              (c, nc) =
                if g.combatDamageUncancellable || hasUncancellableDamage w
                  then (0, d)
                  else (d, 0)
        , d > 0
        , a <- attackerUnits
        ]
      -- When attacking the legend, the legend itself contributes its
      -- power to the defender's damage budget (the rulebook is
      -- explicit about this). Treat it as cancellable defender
      -- damage; no Toughness applies to it (legends don't have
      -- Toughness).
      defenderCanc = defenderUnitCanc + defendingLegendPow
      defenderAssignments =
        allocateDamage g attackerCanc attackerUncanc defenderRecipients
      attackerAssignments =
        allocateDamage g defenderCanc defenderUncanc attackerRecipients
      (defenderUnitAssignments, defenderSpillover) = defenderAssignments
      (attackerUnitAssignments, _attackerSpillover) = attackerAssignments
      -- Spillover routing: legend-targeted attacks land overflow on
      -- the legend (capped at remaining HP, any excess discarded);
      -- zone-targeted attacks spill into the capital section.
      spilloverEntry = case (cs.targetLegend, defendingLegend) of
        (Just _, Just leg) ->
          let Damage existing = leg.damage
              hp = legendPrintedHPFromDef leg.cardDef
              slack = max 0 (hp - existing)
              landing = min defenderSpillover slack
           in if landing > 0
                then
                  [ PendingDamage
                      { target = PDLegend leg.key
                      , cancellable = landing
                      , uncancellable = 0
                      }
                  ]
                else []
        _ ->
          -- A burning section cannot be assigned damage (FAQ); the
          -- attack itself is still legal — units there can be fought
          -- — but any overflow past the defenders is wasted.
          let sectionBurned =
                (getZone cs.targetZone (lookupPlayer cs.defendingPlayer g)).burned
           in if defenderSpillover > 0 && not sectionBurned
                then
                  [ PendingDamage
                      { target = PDZone cs.defendingPlayer cs.targetZone
                      , cancellable = defenderSpillover
                      , uncancellable = 0
                      }
                  ]
                else []
      pendings =
        spilloverEntry
          <> map toPending defenderUnitAssignments
          <> map toPending attackerUnitAssignments
          <> broadsideEntries
      toPending (ukey, canc, uncanc) =
        PendingDamage {target = PDUnit ukey, cancellable = canc, uncancellable = uncanc}
      cs' = (cs {pendingAssignments = pendings}) :: CombatState
  modify \gx -> gx {combat = Just cs'}
  logIt LogSystem
    "log.combat.assigned"
    [ ("attacker_damage", tshow (attackerCanc + attackerUncanc))
    , ("defender_damage", tshow (defenderCanc + defenderUncanc))
    ]

-- | Convert the in-flight 'pendingAssignments' into actual damage
-- messages. Clears the pending list. Also queues Scout post-combat
-- discards.
commitPendingCombatDamage :: StateT Game GameT ()
commitPendingCombatDamage = do
  g <- get
  case g.combat of
    Nothing -> pure ()
    Just cs -> do
      traverse_ commitOne cs.pendingAssignments
      modify \gx -> case gx.combat of
        Just c -> gx {combat = Just (c {pendingAssignments = []} :: CombatState)}
        Nothing -> gx
      -- Scout fires "after combat damage" — but only for SURVIVING
      -- Scouts. Queue a deferred sweep so the survivor check runs
      -- against post-apply state rather than the pre-damage list.
      send $
        FireScoutDiscards
          cs.attackingPlayer
          cs.defendingPlayer
          cs.attackers
          cs.defenders
      -- Raider fires on the same boundary: surviving attackers grant
      -- their controller resources equal to their combined Raider X.
      send $ FireRaiderResources cs.attackingPlayer cs.attackers
  where
    commitOne pd = case pd.target of
      PDUnit k -> do
        when (pd.cancellable > 0) $ send $ DealDamageToUnit k pd.cancellable
        when (pd.uncancellable > 0) $ send $ DealDamageToUnitUncancellable k pd.uncancellable
      PDZone owner z ->
        send $ DealDamageToZone owner z (pd.cancellable + pd.uncancellable)
      PDLegend lkey ->
        send $ DealDamageToLegend lkey (pd.cancellable + pd.uncancellable)

-- | Allocate a (cancellable, uncancellable) damage budget across a
-- list of recipients (in order), respecting Toughness on each one.
-- Returns the per-recipient (cancellable, uncancellable) tuples plus
-- the leftover cancellable+uncancellable that would have spilled
-- past the last recipient.
allocateDamage
  :: Game
  -> Int
  -- ^ cancellable budget
  -> Int
  -- ^ uncancellable budget
  -> [UnitDetails]
  -> ([(UnitKey, Int, Int)], Int)
allocateDamage g = go []
  where
    go acc 0 0 _ = (reverse acc, 0)
    go acc cAvail uAvail [] = (reverse acc, cAvail + uAvail)
    go acc cAvail uAvail (u : rest) =
      let Damage existing = u.damage
          slack = max 0 (u.effectiveMaxHP - existing)
          tough = totalToughness g u
          cancellableUsed = min cAvail (slack + tough)
          landingFromCancellable = max 0 (cancellableUsed - tough)
          slackAfterCancellable = slack - landingFromCancellable
          uncancellableUsed = min uAvail slackAfterCancellable
          entry = (u.key, cancellableUsed, uncancellableUsed)
       in go
            (entry : acc)
            (cAvail - cancellableUsed)
            (uAvail - uncancellableUsed)
            rest

-- | Open a combat sub-step window. The 'OpenActionWindow' handler
-- runs 'maybeAutoPassPriority' for the active player; same goes for
-- the inactive player after the first 'PassPriority' fires. So a
-- window where neither player has anything playable closes itself
-- with no explicit input; one where either player can react (e.g.
-- Defenders of the Faith in hand during 'AfterAssignCombatDamage')
-- pauses for that player.
openAutoCombatWindow :: ActionWindowTrigger -> StateT Game GameT ()
openAutoCombatWindow trigger = send (OpenActionWindow trigger)

-- | Phase-level action windows opt into auto-pass via the host's
-- 'autoSkipActionWindows' setting. Combat sub-step windows have their
-- own always-on auto-pass path via 'isCombatSubStepWindow', so they
-- can stay listed here for completeness without depending on the
-- host's choice.
isAutoSkippableTrigger :: ActionWindowTrigger -> Bool
isAutoSkippableTrigger = \case
  BeginningOfTurnActionWindow -> True
  KingdomActionWindow -> True
  QuestActionWindow -> True
  CapitalActionWindow -> True
  BattlefieldActionWindow -> True
  AfterDeclareCombatTarget -> True
  AfterDeclareAttackers -> True
  AfterDeclareDefenders -> True
  AfterAssignCombatDamage -> True
  AfterApplyCombatDamage -> True
  EndOfTurnActionWindow -> True

-- | Combat sub-step windows where the engine auto-passes a player
-- with no available move regardless of the host's
-- 'autoSkipActionWindows' setting. Without this, combat would require
-- 10 manual passes per phase in non-auto-skip games even when neither
-- player has a relevant tactic / action.
isCombatSubStepWindow :: ActionWindowTrigger -> Bool
isCombatSubStepWindow = \case
  AfterDeclareCombatTarget -> True
  AfterDeclareAttackers -> True
  AfterDeclareDefenders -> True
  AfterAssignCombatDamage -> True
  AfterApplyCombatDamage -> True
  _ -> False

-- | A player has "something to do" in an action window if any hand
-- card is currently playable here, or they control an in-play card
-- with an action ability whose zone gate matches its location and
-- whose base cost they can afford. Both checks are window-aware, so
-- begin-of-turn / end-of-turn / phase windows auto-pass whenever the
-- player can't actually act — a Battlefield-only tactic in hand
-- doesn't keep the Kingdom window open.
playerHasActionMove :: PlayerKey -> Game -> Bool
playerHasActionMove pk g =
  any (isNothing . assessHandCard g pk) (lookupPlayer pk g).hand
    || any (unitActionUsable resources) ownUnits
    || any (otherActionUsable resources) ownSupports
    || any (otherActionUsable resources) ownQuests
    || any (otherActionUsable resources) ownLegends
    -- Opponent-only actions on ENEMY units (Morathi's Pegasus) are
    -- this player's to trigger.
    || any (enemyActionUsable resources) enemyUnits
  where
    Resources resources = (lookupPlayer pk g).resources
    ownUnits = filter (\u -> u.controller == pk) g.units
    enemyUnits = filter (\u -> u.controller /= pk) g.units
    ownSupports = filter (\s -> s.controller == pk) g.supports
    ownQuests = filter (\q -> q.controller == pk) g.quests
    ownLegends = filter (\l -> l.controller == pk) g.legends
    unitActionUsable have u =
      any
        (\a -> not a.actionOpponentOnly && unitActionLegal u have a)
        (unitActions u)
    enemyActionUsable have u =
      any
        (\a -> a.actionOpponentOnly && unitActionLegal u have a)
        (unitActions u)
    unitActionLegal u have a =
      let zoneOk = maybe True (== u.zone) a.availableInZone
       in zoneOk && have >= a.actionCost
    otherActionUsable
      :: HasField "cardDef" a (CardDef k)
      => Int -> a -> Bool
    otherActionUsable have x = any (\a -> have >= a.actionCost) x.cardDef.actions

-- | Enqueue a 'PassPriority' for the named player when they have
-- nothing playable in this window. Phase-level windows opt in via
-- the host's 'autoSkipActionWindows' setting; combat sub-step
-- windows ('isCombatSubStepWindow') always auto-pass when the player
-- has no available move, so the engine doesn't make either side
-- click through five empty windows per attack.
maybeAutoPassPriority
  :: ActionWindowTrigger -> PlayerKey -> StateT Game GameT ()
maybeAutoPassPriority trigger pk = do
  g <- get
  let hostOptedIn = g.autoSkipActionWindows && isAutoSkippableTrigger trigger
      combatAuto = isCombatSubStepWindow trigger
  when
    ((hostOptedIn || combatAuto) && not (playerHasActionMove pk g))
    (send (PassPriority pk))

-- | Translate a resolved prompt into engine messages. Today all
-- prompts are 'CallbackInlinePrompt' — the receive body that issued
-- the prompt resumes inline via 'askPrompt' returning the answer, so
-- there's nothing to dispatch here. Kept as the seam for future
-- callback-style flows (e.g. cross-card chained prompts).
dispatchPromptCallback
  :: PromptCallback
  -> PromptResult
  -> StateT Game GameT ()
dispatchPromptCallback _cb _result = pure ()

-- | Fire post-combat "when this unit damages an enemy" effects. Read
-- 'damagedInCurrentCombat' to know which units actually took damage,
-- then iterate Plaguebearer / Beasts-of-Nurgle participants on each
-- side and corrupt damaged enemies they were dealing damage to.
firePerSourceCombatEffects :: Game -> CombatState -> StateT Game GameT ()
firePerSourceCombatEffects g cs = do
  fireFor cs.attackingPlayer cs.attackers
  fireFor cs.defendingPlayer cs.defenders
  where
    damagedEnemiesOf side =
      [ k
      | k <- (historyOfScope ThisCombat g).damagedUnits
      , Just u <- [findUnit k g]
      , u.controller /= side
      , not u.corrupted
      ]
    hasCorruptOnDamage u = (unitExtrasOf u).corruptsOnCombatDamage
    fireFor side keys = do
      let sources = mapMaybe (`findUnit` g) keys
      when (any hasCorruptOnDamage sources) $
        traverse_ (send . CorruptUnit) (damagedEnemiesOf side)

-- | Per-turn damage caps that some cards impose on themselves —
-- e.g. Daemonettes of Slaanesh "cannot be assigned more than 1
-- damage per turn". Given how much damage has already landed on the
-- unit this turn and how much is incoming, returns the amount that
-- actually lands.
applyPerTurnCap :: UnitDetails -> Int -> Int -> Int
applyPerTurnCap u already incoming = case perTurnCap u of
  Nothing -> incoming
  Just cap -> max 0 (min incoming (cap - already))

-- | The per-turn damage cap for a unit, if any. Driven by the per-card
-- 'damageCap' slice on 'UnitExtras'; suppressed while the unit's text
-- box is blanked.
perTurnCap :: UnitDetails -> Maybe Int
perTurnCap u = (unitExtrasOf u).damageCap

-- | True iff the card carries the 'Limited' keyword.
isLimitedCard :: CardDef k -> Bool
isLimitedCard cd = Limited `elem` cd.keywords

-- | True iff the named player already controls a copy of this card
-- code in play (units, supports, quests, or legends). Discard / hand
-- copies do not count.
controlsCopyInPlay :: PlayerKey -> CardCode -> Game -> Bool
controlsCopyInPlay pk code g =
  hit g.units || hit g.supports || hit g.quests || hit g.legends
  where
    hit
      :: ( HasField "controller" a PlayerKey
         , HasField "cardDef" a (CardDef k)
         )
      => [a] -> Bool
    hit = any \x -> x.controller == pk && x.cardDef.code == code

-- | Refuse if a unique card already has a copy under the player's
-- control, if a Limited card has already been played this turn, or if
-- the card's per-card 'canPlay' predicate refuses (e.g. Stubborn
-- Refusal requires a damaged unit and a peer in its zone).
canPlayCard :: PlayerKey -> CardDef k -> Game -> Bool
canPlayCard pk cd g =
  (not cd.unique || not (controlsCopyInPlay pk cd.code g))
    && (not (isLimitedCard cd) || (historyOfScope ThisTurn g).limitedPlayed == 0)
    && cd.canPlay g pk

-- | Zone-entry restriction keywords ("Battlefield only." etc.). These
-- gate ENTERING play only; once in play, card effects may move the
-- card to any zone.
zoneEntryAllowed :: CardDef k -> ZoneKind -> Bool
zoneEntryAllowed cd zone = all ok cd.keywords
  where
    ok BattlefieldOnly = zone == BattlefieldZone
    ok KingdomOnly = zone == KingdomZone
    ok QuestOnly = zone == QuestZone
    ok _ = True

-- | "Limit one Hero per zone": while a unit carrying the keyword sits
-- in a (player, zone) location, no other unit carrying the keyword may
-- be played into, moved into, or put into that same location — by
-- either player. @exclude@ lets move-checks ignore the moving unit
-- itself.
heroLimitBlocks
  :: Game -> PlayerKey -> CardDef k -> ZoneKind -> Maybe UnitKey -> Bool
heroLimitBlocks g controller cd zone exclude =
  LimitOneHeroPerZone `elem` cd.keywords
    && any
      ( \u ->
          u.controller == controller
            && u.zone == zone
            && Just u.key /= exclude
            -- A blanked hero's printed limit text is gone (Witch
            -- Hag's Curse), so it no longer blocks arrivals.
            && LimitOneHeroPerZone `elem` unitKeywords u
      )
      g.units

-- | Combined entry gate for a unit/support/quest going into a zone:
-- zone-restriction keywords plus the Hero-per-zone limit.
canEnterZone :: Game -> PlayerKey -> CardDef k -> ZoneKind -> Bool
canEnterZone g pk cd zone =
  zoneEntryAllowed cd zone && not (heroLimitBlocks g pk cd zone Nothing)

-- | Decide whether a hand card is currently unplayable, and why.
-- 'Nothing' = playable; 'Just issue' = surface @issue@ to the client
-- (dimmed card, tap-for-reason).
--
-- Mirrors the engine's actual gating in 'withPaidPlay' / 'PlayTactic'
-- / 'PlayLegend' so we don't drift: the same predicate that refuses
-- to act on a 'PlayUnit' message is the one that classifies the
-- hand card as unplayable up-front. If you add a new refusal path,
-- thread it here too.
assessHandCard :: Game -> PlayerKey -> Card -> Maybe PlayabilityIssue
assessHandCard g pk c = case c.def of
  UnitCardDef cd -> assessNonTactic g pk cd
  SupportCardDef cd -> assessNonTactic g pk cd
  QuestCardDef cd -> assessNonTactic g pk cd
  LegendCardDef cd -> assessLegend g pk cd
  TacticCardDef cd -> assessTactic g pk cd

assessNonTactic :: Game -> PlayerKey -> CardDef k -> Maybe PlayabilityIssue
assessNonTactic g pk cd
  -- "You may play this unit from your hand any time you could take
  -- an action." — tactic timing instead of the capital window.
  | PlayAnytime `elem` cd.keywords = assessTactic g pk cd
  | g.currentPlayer /= pk = Just NotYourTurn
  | otherwise = case g.actionWindow of
      Nothing -> Just NotInActionWindow
      Just aw
        | priorityHolder aw.awaiting /= pk -> Just NotInActionWindow
        | aw.trigger /= CapitalActionWindow -> Just WrongActionWindow
        | otherwise -> baseUnplayable g pk cd

assessTactic :: Game -> PlayerKey -> CardDef k -> Maybe PlayabilityIssue
assessTactic g pk cd = case g.actionWindow of
  Nothing -> Just NotInActionWindow
  Just aw
    | priorityHolder aw.awaiting /= pk -> Just NotInActionWindow
    | otherwise -> baseUnplayable g pk cd

assessLegend :: Game -> PlayerKey -> CardDef 'Legend -> Maybe PlayabilityIssue
assessLegend g pk cd = case assessNonTactic g pk cd of
  Just issue -> Just issue
  Nothing
    | any (\l -> l.controller == pk) g.legends -> Just LegendAlreadyInPlay
    | otherwise -> Nothing

baseUnplayable :: Game -> PlayerKey -> CardDef k -> Maybe PlayabilityIssue
baseUnplayable g pk cd
  | cd.unique && controlsCopyInPlay pk cd.code g = Just UniqueAlreadyInPlay
  | isLimitedCard cd && (historyOfScope ThisTurn g).limitedPlayed > 0 =
      Just LimitedAlreadyPlayed
  | not (cd.canPlay g pk) = Just NoValidTarget
  | otherwise = case cd.cost of
      Variable -> Nothing
      Fixed _ ->
        let needed = effectiveTotalCost g pk cd
            Resources have = (lookupPlayer pk g).resources
            -- Dat's Mine!: quest tokens stretch the budget for
            -- Attachment cards, mirroring the PlayAttachment payment
            -- path.
            tokenCredit
              | Attachment `elem` cd.traits =
                  sum
                    [ q.tokens
                    | q <- g.quests
                    , q.controller == pk
                    , q.cardDef.extras.paysForAttachments
                    ]
              | otherwise = 0
         in if have + tokenCredit < needed
              then Just (InsufficientResources needed have)
              else Nothing

-- | Rewrite 'handPlayability' on both players to reflect 'assessHandCard'
-- over the current snapshot. Invoked from the publish hook so the wire
-- view always carries up-to-date reasons; the engine itself never
-- consults this map.
attachHandPlayability :: Game -> Game
attachHandPlayability g =
  g
    { player1 = annotate Player1 g.player1
    , player2 = annotate Player2 g.player2
    }
  where
    annotate pk p =
      p
        { handPlayability =
            Map.fromList
              [ (c.key, issue)
              | c <- p.hand
              , Just issue <- [assessHandCard g pk c]
              ]
        }

-- | Bump the Limited-played counter for the current turn if appropriate.
markPlayedLimited :: CardDef k -> StateT Game GameT ()
markPlayedLimited cd =
  when (isLimitedCard cd) $
    recordEvent \h -> h {limitedPlayed = h.limitedPlayed + 1}

-- | Sum of all Toughness contributions on a unit. Fixed values come
-- straight from the keyword; 'Toughness Variable' (Ironbreakers of
-- Ankhor) scales with the number of developments in the unit's zone.
-- Plus any aura toughness granted by other in-play units (Big 'Uns)
-- and supports (Gromril Armour). 'LoseAllToughness' (Morathi's
-- Pegasus) zeroes the lot.
-- | A unit's effective Counterstrike value: the printed 'Counterstrike'
-- keyword(s) plus any game-state-derived "Counterstrike X"
-- ('selfCounterstrikeBonus': Anlec Lookout, Herald of Morai-Heg,
-- Wardancer). Does not include the turn-scoped Ulric's-Fury bonus,
-- which the combat code adds per defending player.
totalCounterstrike :: Game -> UnitDetails -> Int
totalCounterstrike g u =
  sum [n | Counterstrike n <- unitKeywords u]
    + (unitExtrasOf u).selfCounterstrikeBonus g u
    + sum [n | Modifier (GainCounterstrike n) _ <- mods]
  where
    mods = fromMaybe [] (Map.lookup (UnitRef u.key) g.modifiers)

totalToughness :: Game -> UnitDetails -> Int
totalToughness g u
  | hasModifier g.modifiers u.key LoseAllToughness = 0
  | otherwise = printed + selfBonus + modifierBonus + aura + supportAura
  where
    printed = sum (map asInt (unitKeywords u))
    selfBonus = (unitExtrasOf u).selfToughnessBonus g u
    modifierBonus =
      let mods = fromMaybe [] (Map.lookup (UnitRef u.key) g.modifiers)
       in sum [n | Modifier (GainToughness n) _ <- mods]
    asInt (Toughness (Fixed n)) = n
    asInt (Toughness Variable) = devsInZone g u
    asInt _ = 0
    aura = sum [(unitExtrasOf src).unitAuraToughness g src u | src <- g.units]
    supportAura =
      sum
        [ s.cardDef.extras.supportAuraToughness g s u
        | s <- allInPlaySupports g
        ]

-- 'devsInZone' moved to 'Invasion.Card' so both engine and card defs
-- can read it.

-- | Count race symbols matching 'r' that the named player controls.
-- The player's capital board contributes 1 for its faction; every
-- in-play card (unit, support, quest, legend) bearing the race adds
-- 1 more per instance.
raceSymbolCount :: Game -> PlayerKey -> Race -> Int
raceSymbolCount g pk r =
  capitalSymbol + count g.units + count g.supports + count g.quests + count g.legends
  where
    capitalSymbol = if (lookupPlayer pk g).race == r then 1 else 0
    count
      :: ( HasField "controller" a PlayerKey
         , HasField "cardDef" a (CardDef k)
         )
      => [a] -> Int
    count xs = length [x | x <- xs, x.controller == pk, r `elem` x.cardDef.races]

-- | Loyalty surcharge: each loyalty icon costs 1 resource, reduced by
-- matching race symbols you control (floor at 0). For multi-race
-- cards we take the most generous (largest) symbol count across the
-- card's races.
loyaltySurcharge :: Game -> PlayerKey -> CardDef k -> Int
loyaltySurcharge g pk cardDef
  | loyaltyWaived g pk cardDef = 0
  | otherwise =
      let perRace = map (raceSymbolCount g pk) cardDef.races
          bestMatch = if null perRace then 0 else maximum perRace
       in max 0 (cardDef.loyalty - bestMatch)

-- | Number of cards of race @r@ that @pk@ has played so far this turn.
-- Read from the turn history's 'playedBy' filters (which carry races).
-- Used to snapshot/consume loyalty waivers.
racePlaysThisTurn :: Game -> PlayerKey -> Race -> Int
racePlaysThisTurn g pk r =
  length
    [ ()
    | f <- Map.findWithDefault [] pk (historyOfScope ThisTurn g).playedBy
    , r `elem` f.cfRaces
    ]

-- | Is this card's loyalty cost waived by a live Embassy/Offering waiver?
-- A waiver @(race, snapshot)@ is live while no further card of that race
-- has been played since it was granted (its play count still equals the
-- snapshot), so the very next matching card consumes it.
loyaltyWaived :: Game -> PlayerKey -> CardDef k -> Bool
loyaltyWaived g pk cardDef =
  any
    (\(wr, snap) -> wr `elem` cardDef.races && racePlaysThisTurn g pk wr == snap)
    (Map.findWithDefault [] pk g.loyaltyWaivers)

-- | Card-specific adjustments to the printed (non-loyalty) part of a
-- play cost. Pulls per-card slices: a self adjustment lives directly
-- on 'CardDef.selfCostAdjustment' (Bloodcrusher); external
-- adjustments come from in-play supports via the per-support
-- 'globalCostAdjustment' slice (Imperial Crown, Master Rune of
-- Dismay). Result may be negative — the final cost is clamped in
-- 'effectiveTotalCost'.
printedCostAdjustment :: Game -> PlayerKey -> CardDef k -> Int
printedCostAdjustment g pk cardDef =
  cardDef.selfCostAdjustment g pk + supportAdjust + unitAdjust
  where
    filt = cardCodeFilter cardDef
    supportAdjust =
      sum
        [ s.cardDef.extras.globalCostAdjustment g s pk filt
        | s <- g.supports
        ]
    unitAdjust =
      sum
        [ (unitExtrasOf u).unitCostAdjustment g u pk filt
        | u <- g.units
        ]

-- | Additional resource cost an effect must pay to target the unit
-- referenced in the supplied 'ActionTarget'. Sums the unit's own
-- 'extraTargetTax' (King Kazador-style) with any 'supportTargetTax'
-- contribution from in-play supports that cover the targeted unit
-- (Church of Sigmar).
extraTargetTax :: PlayerKey -> ActionTarget -> Game -> Int
extraTargetTax caster target g = case target of
  TargetUnit k -> case findUnit k g of
    Just u ->
      (unitExtrasOf u).extraTargetTax g caster u
        + sum
            [ s.cardDef.extras.supportTargetTax g s caster u
            | s <- g.supports
            ]
        -- Iron Discipline: a turn-scoped tax on anyone's actions
        -- against this unit.
        + sum
            [ n
            | Modifier (TargetTaxBonus n) _ <-
                fromMaybe [] (Map.lookup (UnitRef k) g.modifiers)
            ]
    Nothing -> 0
  _ -> 0

-- | Total cost to play a card: max(0, printed + adjustments) + loyalty
-- surcharge. Works uniformly for every card kind because the inputs
-- (printed cost, loyalty icons, races) all live on 'CardDef'.
effectiveTotalCost :: Game -> PlayerKey -> CardDef k -> Int
effectiveTotalCost g pk cardDef =
  let printed = case cardDef.cost of
        Fixed n -> n
        Variable -> 0
      adjustedPrinted = max 0 (printed + printedCostAdjustment g pk cardDef)
   in adjustedPrinted + loyaltySurcharge g pk cardDef

-- | Legacy entry-point preserved for callers that haven't migrated
-- yet. Returns the same answer as 'effectiveTotalCost' for units.
effectiveUnitCost :: Game -> PlayerKey -> CardDef Unit -> Int -> Int
effectiveUnitCost g pk cardDef _printed = effectiveTotalCost g pk cardDef

-- | Printed HP for a 'CardDef Unit' (no in-play context). Variable HP
-- defaults to 1; we'll grow this once X-cost units are in scope.
unitPrintedHPFromDef :: CardDef Unit -> Int
unitPrintedHPFromDef cd = case cd.hitPoints of
  Just (Fixed n) -> n
  Just Variable -> 1
  Nothing -> 1

-- | Recompute cached effective stats for every in-play unit. Called
-- after each engine step so 'effectivePower', 'effectiveMaxHP', and
-- combat-role flags ('attacking' / 'defending') always reflect
-- current attachments, experiences, scoped modifiers, zone state,
-- and the in-flight combat.
recomputeUnitStats :: Game -> Game
recomputeUnitStats g = g {units = map update g.units}
  where
    combatAttackers = maybe [] (.attackers) g.combat
    combatDefenders = maybe [] (.defenders) g.combat
    -- Computed ONCE per recompute pass instead of once per unit:
    -- recompute runs after every message, so the per-unit walks here
    -- were the engine's main O(units × supports) hot spot.
    inPlaySupports = allInPlaySupports g
    -- Witch Hag's Curse: blanked-ness is derived from attachments
    -- up-front so that within this pass every cross-unit read agrees
    -- on who is blanked.
    isBlankedNow u = any (.cardDef.extras.blanksHost) u.attachments
    extrasOf u = if isBlankedNow u then defaultExtras @'Unit else u.cardDef.extras
    update u0 =
      let u = (u0 {blanked = isBlankedNow u0}) :: UnitDetails
       in u
            { effectivePower = computePower u
            , effectiveMaxHP = computeMaxHP u
            , attacking = u.key `elem` combatAttackers
            , defending = u.key `elem` combatDefenders
            }
            :: UnitDetails
    computePower u =
      u.cardDef.power
        + sum (map (attachmentPowerBonus u) u.attachments)
        + modifierPowerBonus u
        + sum [(extrasOf v).unitAuraPower g v u | v <- g.units]
        + sum [s.cardDef.extras.supportAuraPower g s u | s <- inPlaySupports]
        + sum [q.cardDef.extras.questUnitAuraPower g q u | q <- g.quests]
        + (extrasOf u).selfPowerBonus g u
        + activeBonusPower ((extrasOf u).runtimeEffects g u)
    computeMaxHP u =
      let printedHP = case u.cardDef.hitPoints of
            Just (Fixed n) -> n
            -- X hit points: the printed base is 0; 'selfHPBonus'
            -- supplies the live X.
            Just Variable -> 0
            Nothing -> 1
          base = printedHP
            + sum (map (attachmentHPBonus u) u.attachments)
            + (extrasOf u).selfHPBonus g u
            + sum [(extrasOf v).unitAuraHp g v u | v <- g.units]
          minus = modifierHPPenalty u
          plus = modifierHPBonus u
          supportAura =
            sum [s.cardDef.extras.supportAuraHP g s u | s <- inPlaySupports]
          -- Variable-HP units (Cold One Chariot) live and die by
          -- their X: zero developments means zero HP, not the
          -- 1-point floor printed cards get.
          floorHP = case u.cardDef.hitPoints of
            Just Variable -> 0
            _ -> 1
       in max floorHP (base + supportAura + plus - minus)
    modifierPowerBonus u =
      let mods = fromMaybe [] (Map.lookup (UnitRef u.key) g.modifiers)
       in sum [n | Modifier (GainPower n) _ <- mods]
    modifierHPPenalty u =
      let mods = fromMaybe [] (Map.lookup (UnitRef u.key) g.modifiers)
       in sum [n | Modifier (LoseHitPoints n) _ <- mods]
    modifierHPBonus u =
      let mods = fromMaybe [] (Map.lookup (UnitRef u.key) g.modifiers)
       in sum [n | Modifier (GainHitPoints n) _ <- mods]

-- | Per-attachment power contribution. Read from the support's
-- 'attachmentPowerBonus' slice on 'SupportExtras'.
attachmentPowerBonus :: UnitDetails -> SupportDetails -> Int
attachmentPowerBonus _host s = s.cardDef.extras.attachmentPowerBonus

-- | Per-attachment HP contribution. Read from the support's
-- 'attachmentHPBonus' slice on 'SupportExtras'.
attachmentHPBonus :: UnitDetails -> SupportDetails -> Int
attachmentHPBonus _host s = s.cardDef.extras.attachmentHPBonus

-- | Continuous aura contributions to a unit's effective power, summed
-- across every in-play unit's 'unitAuraPower' slice (Karl Franz,
-- Templar of Sigmar) and every in-play support's 'supportAuraPower'
-- slice (Iron Tower, Cauldron of Blood). Read by 'recomputeUnitStats'
-- so the bonus is visible in every zone-dependent calculation
-- (resources, quest draw, combat).
auraPowerBonus :: Game -> UnitDetails -> Int
auraPowerBonus g u =
  sum [v.cardDef.extras.unitAuraPower g v u | v <- g.units]
    + sum [s.cardDef.extras.supportAuraPower g s u | s <- allInPlaySupports g]
    + sum [q.cardDef.extras.questUnitAuraPower g q u | q <- g.quests]

-- | Self-scaling power based on game state. Reads the per-card
-- 'selfPowerBonus' slice on 'UnitExtras' (which subsumes the old
-- 'experiencePowerBonus' for cards that scale with experience tokens).
selfScalingPowerBonus :: Game -> UnitDetails -> Int
selfScalingPowerBonus g u = u.cardDef.extras.selfPowerBonus g u

-- | Pure read of a 'Scope's 'History' bucket. Missing buckets fall
-- back to 'mempty' so card bodies can treat all scopes uniformly.
historyOfScope :: Scope -> Game -> History
historyOfScope s g = Map.findWithDefault mempty s g.history

-- | Apply a transformation to every 'Scope's 'History' bucket. Used
-- by the engine when an event happens — every scope advances in
-- lockstep, and individual scopes are then truncated by 'BeginTurn'
-- / 'BeginPhase' / 'BeginCombat' resets.
recordEvent :: (History -> History) -> StateT Game GameT ()
recordEvent f = modify \g -> g {history = Map.map f g.history}

-- | Power bonus produced by 'runtimeEffects' — the per-tick builder
-- output authored via the 'battlefield' / 'kingdom' / 'quest' high-level
-- DSL. Folded into 'effectivePower' alongside the legacy
-- 'selfScalingPowerBonus' slot.
runtimeEffectsPowerBonus :: Game -> UnitDetails -> Int
runtimeEffectsPowerBonus g u = activeBonusPower (u.cardDef.extras.runtimeEffects g u)

takeUnitFromDiscard :: UnitKey -> Player -> Maybe (CardDef Unit, Player)
takeUnitFromDiscard = takeFromPile discardPile asUnit

takeUnitFromDeck :: UnitKey -> Player -> Maybe (CardDef Unit, Player)
takeUnitFromDeck = takeFromPile deckPile asUnit

takeSupportFromDiscard :: UnitKey -> Player -> Maybe (CardDef Support, Player)
takeSupportFromDiscard = takeFromPile discardPile asSupport

takeTacticFromHand :: UnitKey -> Player -> Maybe (CardDef Tactic, Player)
takeTacticFromHand = takeFromPile handPile asTactic

takeLegendFromHand :: UnitKey -> Player -> Maybe (CardDef Legend, Player)
takeLegendFromHand = takeFromPile handPile asLegend

-- | Printed HP for a legend card definition. Mirrors
-- 'unitPrintedHPFromDef'; 'Variable' falls back to 1 for now.
legendPrintedHPFromDef :: CardDef Legend -> Int
legendPrintedHPFromDef cd = case cd.hitPoints of
  Just (Fixed n) -> n
  Just Variable -> 1
  Nothing -> 1

-- | Replace the keyed element whose 'key' matches @x@\'s 'key'. No-op
-- if no such element exists (e.g. concurrent destroy). Works for any
-- record with a @key :: UnitKey@ field — units, supports, quests,
-- legends, all use the same identity.
replaceById :: HasField "key" a UnitKey => a -> [a] -> [a]
replaceById x = map \v -> if v.key == x.key then x else v

replaceUnit :: UnitDetails -> [UnitDetails] -> [UnitDetails]
replaceUnit = replaceById

replaceSupport :: SupportDetails -> [SupportDetails] -> [SupportDetails]
replaceSupport = replaceById

-- | Write back a support whether it's free-standing (in 'g.supports') or
-- attached (inside some unit's 'attachments'), matched by key.
replaceSupportAnywhere :: SupportDetails -> Game -> Game
replaceSupportAnywhere s' g
  | any ((== s'.key) . (.key)) g.supports =
      g {supports = replaceSupport s' g.supports}
  | otherwise =
      g
        { units =
            [ if any ((== s'.key) . (.key)) u.attachments
                then u {attachments = replaceSupport s' u.attachments}
                else u
            | u <- g.units
            ]
        }

replaceQuest :: QuestDetails -> [QuestDetails] -> [QuestDetails]
replaceQuest = replaceById

replaceLegend :: LegendDetails -> [LegendDetails] -> [LegendDetails]
replaceLegend = replaceById

-- | Dispatch a message to a snapshot of in-play units. The snapshot is
-- usually taken BEFORE 'Run Game.receive' runs, so a unit being
-- destroyed by this very message still sees the destruction notice and
-- can fire leave-play hooks. The current 'Player' records are sourced
-- from the post-receive 'Game' so log lines and re-reads of game state
-- see the latest mutations.
-- | Fire one in-play card's 'receive' against a message. The card's
-- own record stands in for the @InPlay k@ argument; the @owner@ player
-- is looked up from its 'controller' field. Polymorphic over kind so a
-- single helper serves units / supports / quests / legends.
fireReceive
  :: ( HasField "controller" a PlayerKey
     , HasField "cardDef" a (CardDef k)
     , InPlay k ~ a
     )
  => Game -> Message -> a -> GameT ()
fireReceive g msg self = case self.cardDef.receive of
  Receive f -> f msg (lookupPlayer self.controller g) self

dispatchToInPlayUnits :: Message -> [UnitDetails] -> Game -> GameT ()
dispatchToInPlayUnits msg snapshot g = for_ snapshot \u -> do
  -- A blanked unit's own printed reactions don't exist (Witch Hag's
  -- Curse); its attachments are other cards and still fire.
  unless u.blanked $ fireReceive g msg u
  -- Also deliver to each attached support so attachment receives
  -- (Daemonsword, Branded by Khorne, Mark of Chaos, …) fire.
  traverse_ (fireReceive g msg) u.attachments

dispatchToInPlaySupports :: Message -> [SupportDetails] -> Game -> GameT ()
dispatchToInPlaySupports msg snapshot g = traverse_ (fireReceive g msg) snapshot

dispatchToInPlayQuests :: Message -> [QuestDetails] -> Game -> GameT ()
dispatchToInPlayQuests msg snapshot g = traverse_ (fireReceive g msg) snapshot

dispatchToInPlayLegends :: Message -> [LegendDetails] -> Game -> GameT ()
dispatchToInPlayLegends msg snapshot g = traverse_ (fireReceive g msg) snapshot

-- | Append a single transcript line to the running 'Game.log'. Each
-- entry carries an i18n key and a map of interpolation params; the
-- frontend resolves them via 'frontend/src/locales/'. Capped at the
-- most recent 'logCap' entries so a long-running game can't balloon
-- the JSON snapshot pushed to clients on every update.
logIt
  :: LogCategory
  -> Text
  -- ^ i18n key, e.g. @"log.phase.begins"@.
  -> [(Text, Text)]
  -- ^ Interpolation params. Enum-shaped values (player keys, phases,
  -- triggers, reasons) are written raw — the frontend resolves them
  -- through further i18n lookups before substitution.
  -> StateT Game GameT ()
logIt cat key params = do
  now <- liftIO getCurrentTime
  modify \g ->
    let entry = LogEntry
          { at = now
          , category = cat
          , key
          , params = Map.fromList params
          }
        appended = g.log <> [entry]
        capped =
          if length appended > logCap
            then drop (length appended - logCap) appended
            else appended
     in g {log = capped}

logCap :: Int
logCap = 500

-- | Param-value encodings. These deliberately echo the wire-side
-- constructor names ('Player1', 'KingdomPhase', etc.) so the frontend
-- can key directly into the matching i18n bundle without an extra
-- mapping layer. Each is just 'tshow' on an enum whose derived 'Show'
-- yields the constructor name; the named aliases document intent at
-- call sites.
playerParam :: PlayerKey -> Text
playerParam = tshow

phaseParam :: Phase -> Text
phaseParam = tshow

triggerParam :: ActionWindowTrigger -> Text
triggerParam = tshow

elimReasonParam :: EliminationReason -> Text
elimReasonParam = tshow

winReasonParam :: WinReason -> Text
winReasonParam = tshow

turnText :: Turn -> Text
turnText (Turn n) = tshow n

-- | Process the given messages on top of a game value, pumping the
-- queue until it drains (or the game ends). Returns the resulting
-- game.
--
-- The engine state lives in a fresh 'Env' for each call: the queue
-- starts empty and is built from the provided messages, then drained.
-- This is the shape the WebSocket runner will use to apply each
-- incoming player frame to the current game value held in its TVar.
applyMessages :: Game -> [Message] -> IO Game
applyMessages g msgs = do
  env <- newEnv g
  runGame (traverse_ send msgs >> gameMain) env

-- | Convenience: apply a single message.
applyMessage :: Game -> Message -> IO Game
applyMessage g m = applyMessages g [m]

-- | Test variant: apply messages while feeding 'askPrompt' a fixed
-- script of answers (in order). Once the script is exhausted the
-- engine falls back to 'autoResolve' (same as the no-context path),
-- so a too-short script behaves like declining the remaining prompts.
applyMessagesWithAnswers :: Game -> [PromptResult] -> [Message] -> IO Game
applyMessagesWithAnswers g answers msgs = do
  env <- newEnvWithAnswers g answers
  runGame (traverse_ send msgs >> gameMain) env

-- | Deal hands and pick a first player. The returned game is paused in
-- 'GameSetup' — to actually begin turn 1, follow with
-- @'applyMessage' g 'BeginGame'@.
runSetup :: IO (Either DeckLoadError Game)
runSetup = case newGame (starterDeckFor Dwarf) (starterDeckFor Dwarf) defaultGameOptions of
  Left err -> pure $ Left err
  Right game -> Right <$> applyMessage game Setup
