{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Entity
  ( Entity (..)
  , Field (..)
  , UnitDetails (..)
  , SupportDetails (..)
  , QuestDetails (..)
  , TacticContext (..)
  , LegendDetails (..)
  , unitPrintedHP
  , getModifiers
  ) where

import Data.Aeson (ToJSON (..), object, (.=))
import Data.Map.Strict qualified as Map
import Data.Maybe
import Invasion.Capital (Damage (..))
import Invasion.CardDef
import Invasion.Modifier
import Invasion.Prelude
import Invasion.Types
import {-# SOURCE #-} Invasion.Game

type family DetailsOfKind (k :: CardKind)
type family KeyOfKind (k :: CardKind)

type instance DetailsOfKind Unit = UnitDetails
type instance KeyOfKind Unit = UnitKey

data family Field (k :: CardKind) typ

data instance Field Unit typ where
  UnitController :: Field Unit PlayerKey
  UnitZone :: Field Unit ZoneKind
  UnitPower :: Field Unit Int

class Entity (k :: CardKind) a where
  toDetails :: a -> DetailsOfKind k
  toKey :: a -> KeyOfKind k
  project :: HasGame m => Field k typ -> a -> m typ

-- | A unit in play: the card definition together with its bookkeeping
-- (unique 'UnitKey', controlling player, zone it occupies, accumulated
-- damage).
data UnitDetails = UnitDetails
  { key :: UnitKey
  , controller :: PlayerKey
  , zone :: ZoneKind
  , cardDef :: CardDef Unit
  , damage :: Damage
  , corrupted :: Bool
    -- ^ A corrupted unit is "wrecked" — its abilities are suppressed
    -- and it doesn't quest, defend, or attack. Cleared during the
    -- kingdom phase restoration step (one per turn, controller's
    -- choice). Cards (Daemonsword, Festering Nurglings, Dominion of
    -- Chaos, …) flip this on.
  , attachments :: [SupportDetails]
    -- ^ Support cards attached to this unit (Daemonsword, Branded by
    -- Khorne, Mark of Chaos, …). When the unit leaves play, all
    -- attachments leave with it.
  , experiences :: [CardCode]
    -- ^ Cards (usually destroyed enemy units) facedown-attached as
    -- "experience" markers, e.g. Skulltaker. Each one is functionally a
    -- counter that the host card's text can reference.
  , effectivePower :: Int
    -- ^ Cached current power: printed value plus contributions from
    -- attachments, experiences, and other buffs. The engine
    -- recomputes this after every message so cards, the wire, and the
    -- frontend always see the same value.
  , effectiveMaxHP :: Int
    -- ^ Cached current max HP. Damage destruction is checked against
    -- this, not the printed 'hitPoints'.
  , attacking :: Bool
    -- ^ True iff this unit is currently declared as an attacker in
    -- the in-flight combat. Cached so card-side predicates can read
    -- @unit.attacking@ without threading game state.
  , defending :: Bool
    -- ^ True iff this unit is currently declared as a defender in
    -- the in-flight combat.
  , tokens :: Int
    -- ^ Resource tokens sitting on this unit (Silver Helm Detachment
    -- enters with 3, War Hydra with 5). Adjusted via
    -- 'AdjustUnitTokens'; clamped non-negative.
  , blanked :: Bool
    -- ^ True while an attached support blanks this unit's printed
    -- text box (Witch Hag's Curse). Recomputed each engine step; the
    -- engine suppresses the unit's receive, actions, keywords, and
    -- extras slices while set. Traits are unaffected.
  }
  deriving stock Show

-- | A unit's printed hit points. 'Variable' is treated as 1 until we
-- have an X-aware evaluator (Toughness X cards aren't in the active
-- decks yet).
unitPrintedHP :: UnitDetails -> Int
unitPrintedHP u = case u.cardDef.hitPoints of
  Just (Fixed n) -> n
  Just Variable -> 1
  Nothing -> 1

-- | A support card in play. Has the same shape as 'UnitDetails' for now;
-- when supports gain effects or attachments they'll grow distinct
-- fields.
data SupportDetails = SupportDetails
  { key :: UnitKey
  , controller :: PlayerKey
  , zone :: ZoneKind
  , cardDef :: CardDef Support
  , attachedTo :: Maybe UnitKey
    -- ^ 'Just' for supports that are attached to a unit; 'Nothing' for
    -- supports sitting freely in a zone. The host's controller is not
    -- necessarily this support's controller (Branded by Khorne can be
    -- played on an enemy unit).
  , tokens :: Int
    -- ^ Generic counter slot. Iron Throneroom counts down from 4 here;
    -- other tokenised supports (resource-storing siege engines, …)
    -- will reuse it.
  , corrupted :: Bool
    -- ^ Whether this support is corrupted (turned sideways). Set as the
    -- cost of "Corrupt this card" artefact actions (Eye of Sheerian);
    -- cleared by the kingdom-phase restore step. Mirrors the unit-side
    -- 'corrupted' flag.
  }
  deriving stock Show

-- | A quest card in play (always sits in the quest area, hence no
-- 'zone' field).
data QuestDetails = QuestDetails
  { key :: UnitKey
  , controller :: PlayerKey
    -- ^ The player who owns the card and receives its benefits.
  , zoneOwner :: PlayerKey
    -- ^ The player whose play area visually houses this quest. Equal
    -- to 'controller' for most quests; for Dominion of Chaos it's the
    -- opponent (the card says "Play in any opponent's zone under your
    -- control").
  , cardDef :: CardDef Quest
  , tokens :: Int
    -- ^ Token accumulator: Raiding Camps tracks none, A Glorious Death
    -- accumulates resource counters, Dominion of Chaos stores combat
    -- damage that's been routed here. The exact semantics live in each
    -- card's 'receive'.
  , questingUnit :: Maybe UnitKey
    -- ^ The unit currently questing on this card, if any. Only one
    -- unit may quest on a given quest at a time. When the questing
    -- unit leaves play the slot clears and accumulated 'tokens' are
    -- lost.
  }
  deriving stock Show

-- | A tactic doesn't persist in play — it resolves and goes to the
-- discard. The "in-play" record for the duration of resolution carries
-- the player who's resolving and the card itself.
data TacticContext = TacticContext
  { controller :: PlayerKey
  , cardDef :: CardDef Tactic
  , xValue :: Int
    -- ^ The 'X' the player chose at play time for variable-cost
    -- tactics. 0 for fixed-cost tactics. The engine has already
    -- debited @xValue@ resources by the time the effect body
    -- runs, so the body just reads this to decide how much to do.
  }
  deriving stock Show

-- | A Legend card in play. Legends are persistent like units, with HP
-- and damage, but they're their own type and aren't targetable by
-- unit-targeting effects. Mirrors 'UnitDetails' for now; specialised
-- fields can land later.
data LegendDetails = LegendDetails
  { key :: UnitKey
  , controller :: PlayerKey
  , zone :: ZoneKind
  , cardDef :: CardDef Legend
  , damage :: Damage
  , corrupted :: Bool
    -- ^ A corrupted legend cannot attack or defend (it still sits on
    -- the board and can be targeted / take damage).
  , attachments :: [SupportDetails]
    -- ^ Attachment supports attached to this legend (Descendant of
    -- Gods, the "Hero or legend" artefacts). Mirrors
    -- 'UnitDetails.attachments'.
  }
  deriving stock Show

-- Hook each in-play record into the open type family declared in
-- 'Invasion.CardDef'.
type instance InPlay Unit = UnitDetails
type instance InPlay Support = SupportDetails
type instance InPlay Quest = QuestDetails
type instance InPlay Tactic = TacticContext
type instance InPlay Legend = LegendDetails

instance Reference UnitDetails where
  toRef details = UnitRef details.key

instance Entity Unit UnitDetails where
  toDetails = id
  toKey = (.key)
  project = \case
    UnitController -> pure . (.controller)
    UnitZone -> pure . (.zone)
    UnitPower -> \details -> do
      mods <- getModifiers details
      let additionalPower = sum [n | Modifier (GainPower n) _ <- mods]
      pure $ details.cardDef.power + additionalPower

instance ToJSON UnitDetails where
  toJSON d =
    object
      [ "key" .= d.key
      , "controller" .= d.controller
      , "zone" .= d.zone
      , "cardDef" .= d.cardDef
      , "damage" .= d.damage
      , "corrupted" .= d.corrupted
      , "attachments" .= d.attachments
      , "experiences" .= d.experiences
      , "effectivePower" .= d.effectivePower
      , "effectiveMaxHP" .= d.effectiveMaxHP
      , "attacking" .= d.attacking
      , "defending" .= d.defending
      , "tokens" .= d.tokens
      , "blanked" .= d.blanked
      ]

instance ToJSON SupportDetails where
  toJSON d =
    object
      [ "key" .= d.key
      , "controller" .= d.controller
      , "zone" .= d.zone
      , "cardDef" .= d.cardDef
      , "attachedTo" .= d.attachedTo
      , "tokens" .= d.tokens
      ]

instance ToJSON QuestDetails where
  toJSON d =
    object
      [ "key" .= d.key
      , "controller" .= d.controller
      , "zoneOwner" .= d.zoneOwner
      , "cardDef" .= d.cardDef
      , "tokens" .= d.tokens
      , "questingUnit" .= d.questingUnit
      ]

instance ToJSON TacticContext where
  toJSON d =
    object
      [ "controller" .= d.controller
      , "cardDef" .= d.cardDef
      ]

instance ToJSON LegendDetails where
  toJSON d =
    object
      [ "key" .= d.key
      , "controller" .= d.controller
      , "zone" .= d.zone
      , "cardDef" .= d.cardDef
      , "damage" .= d.damage
      , "corrupted" .= d.corrupted
      , "attachments" .= d.attachments
      , "kingdomPower" .= d.cardDef.extras.kingdomPower
      , "questPower" .= d.cardDef.extras.questPower
      , "battlefieldPower" .= d.cardDef.extras.battlefieldPower
      ]

getModifiers :: (HasGame m, Reference a) => a -> m [Modifier]
getModifiers a = fromMaybe [] . Map.lookup (toRef a) <$> getAllModifiers
