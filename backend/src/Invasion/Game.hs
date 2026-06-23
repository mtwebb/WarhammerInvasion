{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Game (module Invasion.Game) where

import Control.Monad.State.Strict
import Data.Aeson (ToJSON, ToJSONKey (..))
import Data.Aeson.Types (toJSONKeyText)
import Data.Aeson.TH
import Data.Map.Strict (Map)
import Data.Map.Strict qualified as Map
import Data.Text (Text)
import Data.Time (UTCTime)
import {-# SOURCE #-} Invasion.Card.Types (Card)
import Invasion.Capital
import Invasion.CardDef (ActionTarget, CardCodeFilter)
import Invasion.Entity (LegendDetails, QuestDetails, SupportDetails, UnitDetails)
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types

class Monad m => HasGame m where
  getGame :: m Game

instance HasGame m => HasGame (StateT s m) where
  getGame = lift getGame

-- | 1-indexed counter of player-turns played so far. Turn 1 is the
-- first player's first turn (during which they skip the quest and
-- battlefield phases per the first-turn-penalty rule).
newtype Turn = Turn Int
  deriving stock Show
  deriving newtype (Eq, Ord, Num, ToJSON)

-- | Lifecycle of the game as a whole.
data GameState
  = GameSetup
    -- ^ Before play starts: decks shuffled, hands dealt, first player
    -- chosen, but no turn has begun.
  | GamePlaying
    -- ^ A turn is in progress.
  | GameFinished GameResult
  deriving stock Show

data GameResult = GameResult
  { winner :: PlayerKey
  , reason :: WinReason
  }
  deriving stock Show

data WinReason
  = OpponentDeckedOut
  | OpponentCapitalBurned
  deriving stock Show

-- | An action window is an explicit pause in the engine where both
-- players have an opportunity to take actions. The window closes when
-- both players pass consecutively without acting.
data ActionWindow = ActionWindow
  { trigger :: ActionWindowTrigger
  , awaiting :: PassState
  }
  deriving stock Show

-- | Context that opened the action window — useful for the client (to
-- know what's actionable here) and for restricting which card effects
-- can be played in this window.
data ActionWindowTrigger
  = BeginningOfTurnActionWindow
    -- ^ FAQ 2.2 Phase 0. Opened after every "at the beginning of the
    -- turn" triggered Constant / Forced effect has resolved, before
    -- the Kingdom phase begins. Either player may take actions here.
  | KingdomActionWindow
    -- ^ Opened after resources are collected.
  | QuestActionWindow
    -- ^ Opened after quest-zone cards are drawn.
  | CapitalActionWindow
    -- ^ The capital phase IS one big action window: the active player
    -- may additionally play units/supports/quests/developments here.
  | BattlefieldActionWindow
    -- ^ Opened on entering the battlefield phase, before any attack is
    -- declared. Acts as the "do you want to attack?" pause; passing
    -- here ends the phase. Combat sub-steps emit their own windows.
  -- The 5 combat sub-step windows, emitted only when an attack is
  -- actually declared. Unused until combat is implemented.
  | AfterDeclareCombatTarget
  | AfterDeclareAttackers
  | AfterDeclareDefenders
  | AfterAssignCombatDamage
  | AfterApplyCombatDamage
  | EndOfTurnActionWindow
    -- ^ FAQ 2.2 Phase 5. Opened at end of turn before "at the end of
    -- the turn" triggers resolve and EndOfTurn modifiers expire.
  deriving stock (Show, Eq)

-- | The pass-bookkeeping needed to detect "both pass consecutively."
-- An action taken by either player resets the state to 'NoPasses', with
-- priority returning to the active player.
data PassState
  = NoPasses PlayerKey
    -- ^ The named player holds priority and has not passed.
  | OnePass PlayerKey
    -- ^ The named player holds priority; their opponent just passed.
    -- If this player passes (without acting), the window closes.
  deriving stock Show

priorityHolder :: PassState -> PlayerKey
priorityHolder = \case
  NoPasses p -> p
  OnePass p -> p

-- | A pending choice the engine is waiting for the client to resolve.
-- gameMain pauses while one is set; the client posts a 'ResolvePrompt'
-- message carrying a 'PromptResult', the engine clears the slot and
-- fires the callback Message constructed from the result.
data Prompt = Prompt
  { player :: PlayerKey
    -- ^ Which player is being asked. The wire layer should only
    -- accept a resolution from this seat.
  , kind :: PromptKind
    -- ^ Static metadata describing what the player must pick. The
    -- client renders this.
  , callback :: PromptCallback
    -- ^ Tag identifying which engine effect to fire once the player
    -- has chosen.
  }
  deriving stock Show

-- | What kind of choice the player has to make.
data PromptKind
  = ChooseUnits
      { filterSpec :: PromptFilter
      , minPick :: Int
      , maxPick :: Int
      , description :: Text
      }
    -- ^ Pick a list of units matching the filter, between min and max
    -- entries (inclusive). 'min == 0' means the player may pass.
  | ChooseSacrifice
      { zone :: ZoneKind
      , optional :: Bool
      , description :: Text
      }
    -- ^ Pick one of your own units in the named zone to sacrifice.
    -- 'optional' lets the player skip if no eligible target exists.
  | ChooseYesNo
      { description :: Text
      }
    -- ^ Simple boolean choice — for "you may pay X to do Y" gates.
  | ChooseFromCards
      { cards :: [Card]
      , minPick :: Int
      , maxPick :: Int
      , description :: Text
      }
    -- ^ Pick between min and max cards from an explicit list. The
    -- engine embeds the actual card data so the prompted player's
    -- client can render the choices even when the source list (e.g.
    -- the top of a hidden deck) isn't normally visible.
  | ChooseTargetOption
      { options :: [TargetOption]
      , description :: Text
      }
    -- ^ Unified target picker for "X or Y"-style abilities (Dwarf
    -- Ranger: "deal 1 damage to one target unit or capital"). The
    -- options list is a heterogeneous mix of units / capital zones /
    -- … and the player picks exactly one. The response is the
    -- chosen 'TargetOption'.
  | ChooseAmount
      { minAmount :: Int
      , maxAmount :: Int
      , description :: Text
      }
    -- ^ Pick an integer in @[minAmount, maxAmount]@. Used by
    -- variable-cost cards (Smash-Go-Boom!, Flames of Tzeentch) to
    -- ask the player how much X to pay; response is the chosen X.
  deriving stock Show

-- | A single tagged target option offered to the player by
-- 'ChooseTargetOption'. The engine maps the chosen option back to
-- its typed value via the @Target a@ enumeration that produced it.
data TargetOption
  = TargetUnitOption UnitKey
  | TargetZoneOption PlayerKey ZoneKind
  | TargetSupportOption UnitKey
    -- ^ An in-play support card (free-standing or attached),
    -- identified by its support key. Used by "destroy one target
    -- support card or development"-style unified pickers.
  deriving stock (Show, Eq)

-- | Predicate describing which units a 'ChooseUnits' prompt accepts.
-- Kept as a tagged value (not a function) so it serializes onto the
-- wire and the client can do its own filtering.
data PromptFilter
  = AnyOwnUnit
    -- ^ Any unit the prompted player controls.
  | AnyUnitInPlay
    -- ^ Any unit in play, regardless of controller. Used by action
    -- 'withTarget' AnyUnit picks (Runesmith, …).
  | UnitsFromList [UnitKey]
    -- ^ Pick from this explicit list of in-play unit keys. Used when
    -- the candidate set is computed by the card (e.g. "destroy up to
    -- two attacking units" — the attackers are 'cs.attackers').
  | OwnUnitsFromHandByRace Race
    -- ^ Cards in the prompted player's hand whose CardDef carries the
    -- named race. Used for Iron Throneroom's summon-from-hand half.
  | OwnUnitsFromDiscardByRace Race
    -- ^ Same as above but from discard.
  | OwnUnitsFromHandOrDiscardByRace Race
    -- ^ Union of hand and discard, filtered by race.
  deriving stock Show

-- | A tag identifying which engine continuation owns this prompt.
-- All in-tree prompts now use 'CallbackInlinePrompt' (the receive
-- body resumes via 'askPrompt' returning the answer directly), but
-- the field is kept for forward-compat with any future re-entrant
-- callback flows.
data PromptCallback
  = CallbackInlinePrompt
    -- ^ Generic marker for prompts whose continuation is inline (via
    -- 'askPrompt' returning the answer directly to the receive body).
    -- No engine-side dispatch — the constructor exists only as a wire
    -- tag on the 'Prompt' record. Used by 'withTarget', 'askYesNo',
    -- and any other 'askPrompt'-style helper.
  deriving stock Show

-- | A scheduled effect that fires at a specific trigger.
data PendingEffect
  = PEDealDamageToUnit UnitKey Int
    -- ^ Deal N damage to the named unit when the effect fires.
  | PESacrificeAttackersThisPhase
    -- ^ Destroy every unit currently recorded in
    -- 'Game.attackersThisPhase'. Used by Reckless Attack.
  | PEDestroyUnit UnitKey
    -- ^ Destroy the named unit unconditionally when the effect
    -- fires. Used by Rip Dere 'eads Off! and similar
    -- "sacrifice at end of turn" effects.
  | PEGiveControl UnitKey PlayerKey
    -- ^ Hand control of the named unit to the named player when the
    -- effect fires. Used by Grasping Darkness to return its stolen
    -- unit at end of turn.
  | PERemoveAnimatedUnit UnitKey
    -- ^ Remove a Bolt of Change animated development from play (the
    -- development itself stays where it was). No leave-play hooks —
    -- the card never "left play", it just stops being a unit.
  deriving stock Show

-- | In-flight combat state. Set on 'BeginCombat', mutated through the
-- sub-steps, cleared on 'EndCombat'.
data CombatState = CombatState
  { attackingPlayer :: PlayerKey
  , defendingPlayer :: PlayerKey
  , targetZone :: ZoneKind
  , targetLegend :: Maybe UnitKey
    -- ^ When 'Just', the attacker is targeting the defender's legend
    -- through the named zone (rather than the capital section
    -- itself). Damage spillover lands on the legend (capped at its
    -- HP) and never reaches the zone. When 'Nothing', overflow
    -- assigns to the capital section as usual.
  , attackers :: [UnitKey]
  , defenders :: [UnitKey]
  , attackerPowerPenalty :: Int
    -- ^ Per-attacker power penalty for this combat. Currently set
    -- by Rune of Fortitude (core-013) when the attacker can't afford
    -- the 1-per-attacker tax.
  , pendingAssignments :: [PendingDamage]
    -- ^ Damage tokens placed during the Assign step (step 4) but not
    -- yet committed. Cancellation effects (Defenders of the Faith,
    -- Master Rune of Valaya) mutate this list during the
    -- AfterAssignCombatDamage window; AdvanceCombatToApply converts
    -- each entry into a real DealDamageToUnit / DealDamageToZone
    -- message.
  }
  deriving stock Show

-- | A single placed-but-not-applied damage assignment.
data PendingDamage = PendingDamage
  { target :: PendingTarget
  , cancellable :: Int
  , uncancellable :: Int
  }
  deriving stock Show

-- | Targets a 'PendingDamage' entry can name.
data PendingTarget
  = PDUnit UnitKey
  | PDZone PlayerKey ZoneKind
  | PDLegend UnitKey
  deriving stock (Show, Eq)

-- | A single line in the game-event transcript. The engine appends
-- entries as it processes messages; the frontend renders them in the
-- side-panel above chat. The engine never produces user-visible text:
-- 'key' is an i18n key (resolved in @frontend/src/locales/@) and
-- 'params' are the interpolation arguments. Enum-shaped param values
-- (e.g. @"Player1"@, @"KingdomPhase"@) are themselves resolved via
-- nested i18n lookups on the client so player display names and phase
-- labels respect the active locale.
data LogEntry = LogEntry
  { at :: UTCTime
  , category :: LogCategory
  , key :: Text
  , params :: Map Text Text
  }
  deriving stock Show

-- | Tag for client-side styling. Add cases as new event groupings
-- become useful; the wire JSON uses the constructor name verbatim.
data LogCategory
  = LogSystem
    -- ^ Engine bookkeeping: setup, shuffles, draws, resources, action
    -- window open/close.
  | LogPhase
    -- ^ Phase boundaries.
  | LogTurn
    -- ^ Turn boundaries.
  | LogPlayerAction
    -- ^ Choices originating from a player (currently just
    -- 'PassPriority'; will grow as cards/abilities land).
  | LogResult
    -- ^ Eliminations and the final game-over line.
  deriving stock (Show, Eq)

-- | A time window for tracked events. Cards that reference "this
-- turn" / "this phase" / "this combat" read from the matching
-- 'History' bucket; the engine resets each scope at its boundary.
data Scope = ThisTurn | ThisPhase | ThisCombat
  deriving stock (Show, Eq, Ord)

-- | One declared attack, recorded into 'History.combats' at
-- 'BeginCombat' and completed with the defender list at
-- 'DeclareDefenders'. Cards that reference "units that attacked this
-- zone" (Tyriel) or "could have defended but did not" (Malus
-- Darkblade) read these.
data CombatRecord = CombatRecord
  { attacker :: PlayerKey
  , defender :: PlayerKey
  , zone :: ZoneKind
  , attackerKeys :: [UnitKey]
  , defenderKeys :: [UnitKey]
  }
  deriving stock Show

-- | One armed "cancel damage to your capital" grant (Flagellants,
-- Gifts of Aenarion). Distinct from the once-per-turn
-- 'capitalShieldPerTurn' supports: grants are armed by effects, last
-- until end of turn, and may refund resources per point cancelled.
data CapitalShieldGrant = CapitalShieldGrant
  { points :: Maybe Int
    -- ^ 'Just n' cancels up to n more points; 'Nothing' cancels all.
  , refundPer :: Int
    -- ^ Resources credited to the shield's owner per point cancelled
    -- (Gifts of Aenarion: 1).
  }
  deriving stock Show

-- | Aggregated record of events that happened during a 'Scope'.
-- Cards consult this to scale effects ("for each unit that entered a
-- discard pile this turn", "sacrifice all units that attacked this
-- phase", …).
data History = History
  { unitsDiscarded :: Int
    -- ^ Count of units that entered a discard pile in this scope.
  , attackersDeclared :: [UnitKey]
    -- ^ Units that have been declared as attackers in this scope.
  , damagedUnits :: [UnitKey]
    -- ^ Units that had non-zero damage land on them in this scope.
  , damageTaken :: Map UnitKey Int
    -- ^ Total damage landed on each unit in this scope.
  , limitedPlayed :: Int
    -- ^ How many Limited cards have been played in this scope.
  , supportsPlayedBy :: Map PlayerKey Int
    -- ^ Per-player count of Support cards that player has played in
    -- this scope. Read by cost-discount cards that fire on the
    -- first support of the turn (Nuln Tinkerers, Grimgor's Camp).
  , unitsPlayedBy :: Map PlayerKey Int
    -- ^ Per-player count of Unit cards that player has played in
    -- this scope. Used by We'z Bigga!-style "next unit"
    -- discounts when that lands.
  , drawnBy :: Map PlayerKey Int
    -- ^ Per-player count of standard draws taken in this scope.
    -- Compared against 'Game.drawCaps' (Infiltrate the tactic).
  , combats :: [CombatRecord]
    -- ^ Attacks declared in this scope, newest first. Defender lists
    -- are filled in when 'DeclareDefenders' fires.
  , playedBy :: Map PlayerKey [CardCodeFilter]
    -- ^ Per-player static descriptions of every card played in this
    -- scope, newest first. Lets cost-adjusters and reactions ask
    -- "have I played a Skaven / Spell card yet this turn?" without a
    -- registry lookup (Greyseer's Lair, Ancient Waystone, Plague
    -- Monk).
  , discardedUnitsBy :: Map PlayerKey [UnitKey]
    -- ^ Per-player keys of unit cards that entered that player's
    -- discard pile from play in this scope (Stand Your Ground).
  }
  deriving stock Show

instance Semigroup History where
  a <> b = History
    { unitsDiscarded = a.unitsDiscarded + b.unitsDiscarded
    , attackersDeclared = a.attackersDeclared <> b.attackersDeclared
    , damagedUnits = a.damagedUnits <> b.damagedUnits
    , damageTaken = Map.unionWith (+) a.damageTaken b.damageTaken
    , limitedPlayed = a.limitedPlayed + b.limitedPlayed
    , supportsPlayedBy = Map.unionWith (+) a.supportsPlayedBy b.supportsPlayedBy
    , unitsPlayedBy = Map.unionWith (+) a.unitsPlayedBy b.unitsPlayedBy
    , drawnBy = Map.unionWith (+) a.drawnBy b.drawnBy
    , combats = a.combats <> b.combats
    , playedBy = Map.unionWith (<>) a.playedBy b.playedBy
    , discardedUnitsBy = Map.unionWith (<>) a.discardedUnitsBy b.discardedUnitsBy
    }

instance Monoid History where
  mempty = History
    { unitsDiscarded = 0
    , attackersDeclared = []
    , damagedUnits = []
    , damageTaken = Map.empty
    , limitedPlayed = 0
    , supportsPlayedBy = Map.empty
    , unitsPlayedBy = Map.empty
    , drawnBy = Map.empty
    , combats = []
    , playedBy = Map.empty
    , discardedUnitsBy = Map.empty
    }

-- | Initial history map with every 'Scope' present at 'mempty'.
emptyHistory :: Map Scope History
emptyHistory = Map.fromList [(s, mempty) | s <- [ThisTurn, ThisPhase, ThisCombat]]

data Game = Game
  { player1 :: Player
  , player2 :: Player
  , firstPlayer :: PlayerKey
  , currentPlayer :: PlayerKey
  , turn :: Turn
  , phase :: Maybe Phase
    -- ^ 'Nothing' before the first 'BeginTurn'; otherwise the phase
    -- currently being processed.
  , actionWindow :: Maybe ActionWindow
    -- ^ Top of the action-window stack — the window 'PassPriority'
    -- is currently directed at. Always equal to @listToMaybe
    -- actionWindowStack@; kept denormalized so existing wire
    -- clients can keep reading a single window.
  , actionWindowStack :: [ActionWindow]
    -- ^ Stack of currently-open action windows. The head is the
    -- topmost window; OpenActionWindow pushes, CloseActionWindow
    -- pops. Combat sub-step windows live on top of the
    -- BattlefieldActionWindow that opened them.
  , modifiers :: Map (Ref Target) [Modifier]
  , lifecycle :: GameState
    -- ^ Named 'lifecycle' (not 'state') because 'Player' also has a
    -- 'state' field, and using the same name would force every record
    -- update site to annotate which type it's updating.
  , log :: [LogEntry]
    -- ^ Append-only transcript of engine events, oldest first. Capped
    -- at 500 entries (see 'Invasion.Engine.logIt').
  , units :: [UnitDetails]
    -- ^ All units in play across both capitals. Each carries its
    -- 'controller' and 'zone' so callers filter rather than indexing
    -- through 'Capital'. 'Zone' lives in 'Invasion.Capital', which is
    -- compiled below this module, so we can't hang the list off
    -- 'Zone' directly.
  , supports :: [SupportDetails]
    -- ^ Free-standing (non-attached) support cards across both
    -- capitals. Attached supports live inside their host unit's
    -- 'attachments' field.
  , quests :: [QuestDetails]
    -- ^ Quest cards currently in play (sit in the quest zone for the
    -- controller who played them, or — for Mission quests — in an
    -- opponent's zone).
  , legends :: [LegendDetails]
    -- ^ Legends currently in play. By rule each player may control at
    -- most one legend at a time; the engine enforces that gate on
    -- 'PlayLegend'. Legends live on their controller's capital board
    -- (not inside a zone) but contribute power to all three zones.
  , nextUnitKey :: UnitKey
    -- ^ Monotonic counter for minting fresh 'UnitKey's as units enter
    -- play.
  , pendingEndOfTurn :: [PendingEffect]
    -- ^ Effects scheduled to fire at the next 'EndTurn'. Cleared as
    -- they fire so they don't leak across turns.
  , combat :: Maybe CombatState
    -- ^ 'Just' while a combat is in progress between 'BeginCombat' and
    -- 'EndCombat'. Card receives consult this to know they're in the
    -- combat path.
  , pendingEndOfPhase :: [(Phase, PendingEffect)]
    -- ^ Effects scheduled to fire on 'EndPhase' for a specific phase.
    -- Entries matching the firing phase are extracted, run, and
    -- discarded.
  , zoneDamageDrawWatchers :: [(PlayerKey, PlayerKey, ZoneKind)]
    -- ^ "Until the end of the phase, draw a card for each damage dealt
    -- to that zone." (Get 'Em Ladz!) Each tuple is
    -- (watching player, zone owner, zone); when damage lands on the
    -- watched zone the watcher draws that many cards. Cleared at
    -- 'EndPhase'.
  , history :: Map Scope History
    -- ^ Per-scope event log (counts of units discarded, attackers
    -- declared, damage taken, etc.). Engine bumps every scope's
    -- entry when an event happens and resets the relevant scope on
    -- its boundary ('BeginTurn' / 'BeginPhase' / 'BeginCombat').
    -- Cards read via 'historyOf' / 'withHistory'.
  , pendingPrompt :: Maybe Prompt
    -- ^ When 'Just', the engine is waiting for the named player to
    -- post a 'ResolvePrompt' carrying their choice. 'gameMain'
    -- returns early as long as this is set, leaving the queue
    -- partially-drained so the wire layer can push the state and
    -- wait for the client's response.
  , autoSkipActionWindows :: Bool
    -- ^ Host-controlled setting captured at game creation. When 'True'
    -- the engine auto-passes priority whenever the holder of a phase
    -- action window has neither a Tactic card in hand nor an in-play
    -- own card carrying an action ability. Combat sub-step windows
    -- already auto-pass regardless of this flag.
  , capitalDefenseUsed :: Map UnitKey Int
    -- ^ Per-source-card usage counter for once-per-turn capital
    -- defenses ("cancel 1 damage to your capital each turn",
    -- "redirect the first point of damage done to your capital each
    -- turn"). Keyed by the in-play support / quest key supplying the
    -- defense; bumped when 'DealDamageToZone' consumes it and reset
    -- at 'BeginTurn'. Evaluating eligibility live at damage time
    -- (instead of arming tokens at turn start) is what makes these
    -- defenses work on the opponent's turn, when attacks actually
    -- happen.
  , pendingUnitDiscount :: Map PlayerKey Int
    -- ^ Per-player "next unit you play costs N less". Decremented
    -- to 0 on first 'PlayUnit'. Reset at 'BeginTurn'. Written by
    -- We'z Bigga!.
  , pendingUnitOnPlayDamage :: Map PlayerKey Int
    -- ^ Per-player "next unit you play comes in with N damage".
    -- Consumed on first 'PlayUnit'. Reset at 'BeginTurn'. Written
    -- by We'z Bigga!.
  , unitsRedirectedThisTurn :: Map UnitKey Int
    -- ^ Per-unit count of "first damage redirect" effects already
    -- consumed this turn. Cards with a once-per-turn redirect
    -- (Warrior Priests) check this before firing. Reset at
    -- 'BeginTurn'.
  , lastResolvedTactic :: Maybe (CardCode, ActionTarget, Int)
    -- ^ The most recently resolved tactic, with the target it
    -- carried and the X paid. Used by Twin-Tailed Comet to copy
    -- the previous tactic. Cleared at end of turn.
  , lastRevealed :: [Card]
    -- ^ Cards most recently revealed to BOTH players (reveal-the-top
    -- effects). Public information — the frontend flashes them.
    -- Overwritten by each reveal; cleared at 'BeginTurn'.
  , pendingActionCancel :: Map PlayerKey Int
    -- ^ Per-player count of "next card action cancel" tokens.
    -- Decremented and consumed on the next 'TriggerCardAction'
    -- firing for that player. Written by Bright Wizard Apprentice;
    -- reset at end of turn.
  , developmentPlayedThisTurn :: Bool
    -- ^ True after the active player plays a face-down development
    -- this turn. Enforces the once-per-turn development rule. Reset
    -- to False at 'BeginTurn'.
  , attackBlockedThisTurn :: [PlayerKey]
    -- ^ Players who may not declare another attack for the rest of
    -- this turn (Fulminating Cage). Checked in 'BeginCombat'; reset
    -- at 'BeginTurn'.
  , drawCaps :: Map PlayerKey Int
    -- ^ Per-player limit on standard draws for the rest of the turn
    -- (Infiltrate the tactic: 1). Checked against the ThisTurn
    -- 'drawnBy' count; cleared at end of turn.
  , capitalShields :: Map PlayerKey [CapitalShieldGrant]
    -- ^ Armed "cancel damage to your capital" grants (Flagellants,
    -- Gifts of Aenarion), consumed in order at damage time. Cleared
    -- at end of turn.
  , defenderCounterstrikeBonus :: Map PlayerKey Int
    -- ^ "Each of your defending units gains Counterstrike N until the
    -- end of the turn." (Ulric's Fury.) Added to every defending
    -- unit's printed Counterstrike when defenders are declared.
    -- Cleared at end of turn.
  , pendingFreeTactic :: Map PlayerKey Int
    -- ^ Count of "the next non-Epic tactic you play this turn costs
    -- 0" grants (Runefang of Solland). Consumed on play; cleared at
    -- end of turn.
  , tacticDamageContext :: Maybe PlayerKey
    -- ^ Set while the damage messages queued by a resolving tactic
    -- drain, so 'tacticDamageBonus' supports (Hellcannon Reserves)
    -- can amplify them. Cleared by the trailing
    -- 'ClearTacticDamageContext' message.
  , combatDamageUncancellable :: Bool
    -- ^ "Until the end of the turn, combat damage cannot be
    -- cancelled." (Mob Up.) While set, all combat damage assigns into
    -- the uncancellable bucket (bypassing Toughness and cancel
    -- effects). Cleared at end of turn.
  }
  deriving stock Show

-- | Concrete answer the client posts back with 'ResolvePrompt'.
data PromptResult
  = PickUnits [UnitKey]
    -- ^ List of chosen unit keys (in-play, in-hand, or in-discard
    -- depending on the prompt). The engine validates against the
    -- prompt's filter / bounds before firing the callback.
  | PickBool Bool
    -- ^ Yes/No answer for 'ChooseYesNo'.
  | PickTargetOption TargetOption
    -- ^ The chosen tagged option from a 'ChooseTargetOption' prompt.
  | PickAmount Int
    -- ^ Integer answer to a 'ChooseAmount' prompt.
  | PickNone
    -- ^ Player declined / no eligible target.
  deriving stock Show

instance HasField "over" Game Bool where
  getField g = case g.lifecycle of
    GameFinished _ -> True
    _ -> False

getAllModifiers :: HasGame m => m (Map (Ref Target) [Modifier])
getAllModifiers = do
  g <- getGame
  pure g.modifiers

getPlayer :: HasGame m => PlayerKey -> m Player
getPlayer pkey = do
  g <- getGame
  pure $ case pkey of
    Player1 -> g.player1
    Player2 -> g.player2

getBattleField :: HasGame m => PlayerKey -> m Zone
getBattleField pkey = do
  p <- getPlayer pkey
  pure $ p.battlefield

getKingdom :: HasGame m => PlayerKey -> m Zone
getKingdom pkey = do
  p <- getPlayer pkey
  pure $ p.capital.kingdom

getQuestZone :: HasGame m => PlayerKey -> m Zone
getQuestZone pkey = do
  p <- getPlayer pkey
  pure $ p.capital.quest

getCapital :: HasGame m => PlayerKey -> m Capital
getCapital pkey = do
  p <- getPlayer pkey
  pure $ p.capital

battlefield :: (HasGame m, HasField "controller" a PlayerKey) => a -> (Zone -> m ()) -> m ()
battlefield a f = getBattleField a.controller >>= f

capital :: HasGame m => PlayerKey -> (Capital -> m ()) -> m ()
capital pkey f = getCapital pkey >>= f

mconcat
  [ deriveToJSON defaultOptions ''PassState
  , deriveToJSON defaultOptions ''ActionWindowTrigger
  , deriveToJSON defaultOptions ''ActionWindow
  , deriveToJSON defaultOptions ''WinReason
  , deriveToJSON defaultOptions ''GameResult
  , deriveToJSON
      defaultOptions {tagSingleConstructors = True, allNullaryToStringTag = True}
      ''LogCategory
  , deriveToJSON defaultOptions ''LogEntry
  , deriveJSON defaultOptions ''Scope
  , deriveToJSON defaultOptions ''CombatRecord
  , deriveToJSON defaultOptions ''CapitalShieldGrant
  , deriveToJSON defaultOptions ''History
  , deriveToJSON defaultOptions ''PendingEffect
  , deriveToJSON defaultOptions ''PendingTarget
  , deriveToJSON defaultOptions ''PendingDamage
  , deriveToJSON defaultOptions ''CombatState
  , deriveToJSON defaultOptions ''PromptFilter
  , deriveJSON defaultOptions ''TargetOption
  , deriveToJSON defaultOptions ''PromptKind
  , deriveToJSON defaultOptions ''PromptCallback
  , deriveToJSON defaultOptions ''Prompt
  , deriveJSON defaultOptions ''PromptResult
  ]

instance ToJSONKey Scope where
  toJSONKey = toJSONKeyText \case
    ThisTurn -> "ThisTurn"
    ThisPhase -> "ThisPhase"
    ThisCombat -> "ThisCombat"

mconcat
  [ deriveToJSON defaultOptions ''Game
  , deriveToJSON defaultOptions ''GameState
  ]
