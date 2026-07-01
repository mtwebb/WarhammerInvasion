{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Wire types for the lobby and per-game WebSockets.
--
-- The frontend mirror lives in @frontend/src/api/protocol.ts@; keep the
-- two in lockstep. Aeson's default 'TaggedObject' encoding produces
-- @{ "tag": "ConstructorName", ... }@ which the TS side discriminates on.
module Invasion.Server.Protocol
  ( -- * Common
    UserInfo (..)
  , ChatLine (..)
  , Visibility (..)
  , GameStatus (..)
  , GameSummary (..)
  , SeatView (..)
  , DeckView (..)
  , GameView (..)
  , MaintenanceState (..)
    -- * Lobby socket
  , LobbyIn (..)
  , LobbyOut (..)
    -- * Game socket
  , GameIn (..)
  , GameOut (..)
  , ZoneTarget (..)
  , PromptResultWire (..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Aeson.TH (deriveJSON, defaultOptions)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Invasion.CardDef (Trait)
import Invasion.Game (TargetOption)
import Invasion.Prelude
import Invasion.Types (CardCode, PlayerKey, Race, UnitKey, ZoneKind)

-- ----------------------------------------------------------------------------
-- Common

data UserInfo = UserInfo
  { userId :: UUID
  , displayName :: Text
  }
  deriving stock (Show, Eq, Generic)

data ChatLine = ChatLine
  { from :: UserInfo
  , text :: Text
  , at :: UTCTime
  }
  deriving stock (Show, Generic)

data Visibility = Public | Private
  deriving stock (Show, Eq, Generic)

data GameStatus
  = StatusWaiting
  | StatusPlaying
  | StatusEnded
  deriving stock (Show, Eq, Generic)

-- | A scheduled-deploy / maintenance window. When 'Just' on the lobby,
-- clients render a banner and the server refuses 'LobbyCreateGame'.
-- The server itself does not auto-shutdown at 'until' — the deploy
-- pipeline owns the actual restart; this is purely a player-facing
-- heads-up. Cleared by the admin endpoint or by a server restart.
data MaintenanceState = MaintenanceState
  { until :: UTCTime
  , message :: Maybe Text
  }
  deriving stock (Show, Eq, Generic)

-- | Summary of the deck loaded into a seat. Two shapes share the
-- record: a user-built deck carries @deckId :: Just …@ with the row's
-- name/capital; a pre-built starter carries @starterRace :: Just …@
-- (and a 'Nothing' @deckId@). At most one of the two is ever set.
data DeckView = DeckView
  { deckId :: Maybe UUID
  , starterRace :: Maybe Race
  , name :: Text
  , capital :: Maybe Text
  , size :: Int
  }
  deriving stock (Show, Generic)

data SeatView = SeatView
  { seat :: Text -- "Player1" | "Player2"
  , user :: UserInfo
  , isHost :: Bool
  , deck :: Maybe DeckView
  }
  deriving stock (Show, Generic)

-- | What lobby clients see in the game list. Private games are not
-- broadcast at all, so this only ever describes public ones.
data GameSummary = GameSummary
  { gameId :: UUID
  , name :: Text
  , host :: UserInfo
  , visibility :: Visibility
  , hasPassword :: Bool
  , filledSeats :: Int -- 0..2
  , status :: GameStatus
  , allowSpectators :: Bool
  , spectatorCount :: Int
  }
  deriving stock (Show, Generic)

-- | The full server-side view of a game, sent to clients connected to
-- its game socket. 'inviteToken' is only populated for the host.
data GameView = GameView
  { gameId :: UUID
  , name :: Text
  , host :: UserInfo
  , visibility :: Visibility
  , hasPassword :: Bool
  , allowSpectators :: Bool
  , spectatorCount :: Int
  , useStarterDecks :: Bool
    -- ^ When True, the waiting room offers a race picker instead of the
    -- saved-deck picker; the server seats the pre-built 40-card
    -- starter for the chosen race when the game begins.
  , inviteToken :: Maybe Text
  , seats :: [SeatView]
  , status :: GameStatus
  , chat :: [ChatLine]
  , engine :: Maybe Value
    -- ^ The engine 'Game' value, serialized as a JSON blob, present once
    -- the game has been started. Sent as opaque 'Value' here so this
    -- module doesn't need to import the engine; the frontend has a
    -- typed view of the relevant subset. Eventually this becomes a
    -- per-viewer snapshot (opponent's hand hidden).
  }
  deriving stock (Show, Generic)

-- ----------------------------------------------------------------------------
-- Lobby socket

data LobbyIn
  = -- | Send a chat line to the global lobby.
    LobbyChatSend { text :: Text }
  | -- | Create a new game slot. Server replies with 'LobbyGameCreated'.
    -- 'allowSpectators' is optional: if absent, the server defaults to
    -- True for public games and False for private ones.
    -- 'autoSkipActionWindows' is optional: when 'Just True' the engine
    -- automatically passes priority for whichever player has no tactic
    -- in hand and no in-play card carrying an action ability. Defaults
    -- to False (every action window waits for an explicit pass).
    -- 'useStarterDecks' is optional: when 'Just True' players pick a
    -- race in the waiting room (instead of one of their saved decks)
    -- and the server seats the pre-built 40-card starter for that
    -- race. Defaults to False.
    LobbyCreateGame
      { name :: Text
      , visibility :: Visibility
      , password :: Maybe Text
      , allowSpectators :: Maybe Bool
      , autoSkipActionWindows :: Maybe Bool
      , useStarterDecks :: Maybe Bool
      }
  | -- | Join a public game (no password needed). Reply: 'LobbyGameJoinOk'.
    LobbyJoinPublic { gameId :: UUID }
  | -- | Join a password-gated game. Reply: 'LobbyGameJoinOk' or 'LobbyError'.
    LobbyJoinWithPassword { gameId :: UUID, password :: Maybe Text }
  deriving stock (Show, Generic)

data LobbyOut
  = LobbyWelcome
      { you :: Maybe UserInfo
        -- ^ 'Nothing' for guest (unauthenticated) connections. Guests
        -- see the chat + game list but can't post or host.
      , users :: [UserInfo]
      , games :: [GameSummary]
      , chat :: [ChatLine]
      , maintenance :: Maybe MaintenanceState
      }
  | LobbyChatNew { line :: ChatLine }
  | LobbyUsersUpdate { users :: [UserInfo] }
  | LobbyGamesUpdate { games :: [GameSummary] }
  | LobbyGameCreated
      { gameId :: UUID
      , inviteToken :: Maybe Text
      }
  | LobbyGameJoinOk
      { gameId :: UUID
      , inviteToken :: Maybe Text
      }
  | -- | Broadcast when the server enters or leaves a maintenance window.
    -- 'Nothing' clears the banner; 'Just' starts or updates it.
    LobbyMaintenance { state :: Maybe MaintenanceState }
  | LobbyError { code :: Text }
  deriving stock (Show, Generic)

-- ----------------------------------------------------------------------------
-- Per-game socket

data GameIn
  = GameChatSend { text :: Text }
  | -- | Replace this seat's loaded deck with the deck identified by id.
    -- The deck must belong to the seated user. Rejected when the slot
    -- was created with 'useStarterDecks' — use 'GameSelectStarter'
    -- instead.
    GameSelectDeck { deckId :: UUID }
  | -- | Load a pre-built starter for the named race into this seat.
    -- Only valid when the slot was created with 'useStarterDecks'.
    GameSelectStarter { race :: Race }
  | -- | Clear the loaded deck for this seat.
    GameClearDeck
  | -- | Host-only. Transition Waiting -> Playing if both seats have decks.
    GameStart
  | -- | Pass the current action window. The server fills in the
    -- 'PlayerKey' from the sender's seat; if it isn't this player's
    -- priority the engine will silently ignore the message.
    GamePassPriority
  | -- | Play a card from the sender's hand. The card is identified by
    -- its stable 'UnitKey' (the same key the frontend sees on the card
    -- it clicked) so duplicates in hand are addressed unambiguously.
    --
    --   * Unit / Support (non-attachment): 'zone' picks which zone the
    --     card enters. Required for those kinds; ignored otherwise.
    --   * Support (attachment trait): 'target' picks the host unit's
    --     'UnitKey'. Required for attachments; ignored otherwise.
    --   * Quest / Tactic / Legend: neither 'zone' nor 'target' is read.
    GamePlayCard
      { cardKey :: UnitKey
      , zone :: Maybe ZoneKind
      , target :: Maybe UnitKey
      }
  | -- | Trigger a printed action ability on an in-play card. The
    -- engine validates that the source belongs to the sender, debits
    -- the resource cost, and checks the supplied target against the
    -- action's declared 'TargetSchema'.
    GameTriggerAction
      { source :: UnitKey
      , actionIndex :: Int
      , target :: Maybe UnitKey
      , targetZone :: Maybe ZoneTarget
      }
  | -- | Resolve the engine's currently-pending prompt. The server
    -- validates that the sender's seat matches the prompt's player.
    GameResolvePrompt
      { result :: PromptResultWire
      }
  | -- | Declare an attack against the opponent. The server only
    -- forwards this to the engine when the sender holds priority in
    -- the BattlefieldActionWindow during their own battlefield phase
    -- and no combat is already in flight. Engine still applies its
    -- own per-card attacker eligibility check after that.
    GameDeclareAttack
      { attackZone :: ZoneKind
      , attackerKeys :: [UnitKey]
      }
  | -- | The active player plays a hand card face-down as their
    -- once-per-turn development. Engine refuses outside the
    -- CapitalActionWindow or after a development has already been
    -- played this turn.
    GamePlayDevelopment
      { cardKey :: UnitKey
      , developmentZone :: ZoneKind
      }
  | -- | Drop this user from the seat, broadcast to the other seat.
    GameLeave
  deriving stock (Show, Generic)

-- | Wire-side mirror of the engine's typed 'PromptResult'. The server
-- converts before applying so the engine stays decoupled from the
-- on-wire shape.
data PromptResultWire
  = PromptUnitsWire { unitKeys :: [UnitKey] }
  | PromptBoolWire { yes :: Bool }
  | PromptTargetOptionWire { option :: TargetOption }
  | PromptAmountWire { amount :: Int }
  | PromptTraitWire { trait :: Trait }
  | PromptNoneWire
  deriving stock (Show, Generic)

-- | Zone reference for action targets that point at a capital zone.
data ZoneTarget = ZoneTarget
  { player :: PlayerKey
  , kind :: ZoneKind
  }
  deriving stock (Show, Generic)

data GameOut
  = GameWelcome
      { you :: Maybe UserInfo
        -- ^ 'Nothing' for guest spectators (no signed-in account).
      , game :: GameView
      , maintenance :: Maybe MaintenanceState
      }
  | GameUpdate { game :: GameView }
  | GameChatNew { line :: ChatLine }
  | GameError { code :: Text }
  | -- | Sent when the slot is being torn down. Frontend should redirect
    -- back to the lobby.
    GameClosed { reason :: Text }
  | -- | Mirror of 'LobbyMaintenance' for clients connected to a game.
    GameMaintenance { state :: Maybe MaintenanceState }
  deriving stock (Show, Generic)

mconcat
  [ deriveJSON defaultOptions ''UserInfo
  , deriveJSON defaultOptions ''ChatLine
  , deriveJSON defaultOptions ''Visibility
  , deriveJSON defaultOptions ''GameStatus
  , deriveJSON defaultOptions ''DeckView
  , deriveJSON defaultOptions ''SeatView
  , deriveJSON defaultOptions ''GameSummary
  , deriveJSON defaultOptions ''GameView
  , deriveJSON defaultOptions ''MaintenanceState
  , deriveJSON defaultOptions ''LobbyIn
  , deriveJSON defaultOptions ''LobbyOut
  , deriveJSON defaultOptions ''ZoneTarget
  , deriveJSON defaultOptions ''PromptResultWire
  , deriveJSON defaultOptions ''GameIn
  , deriveJSON defaultOptions ''GameOut
  ]

-- Pacify -Wunused-top-binds on the generic-derived hooks above.
_unused :: (FromJSON UserInfo, ToJSON UserInfo) => ()
_unused = ()
