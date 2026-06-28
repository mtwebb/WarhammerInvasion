{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Data definitions for 'SomeCardDef' and 'Card'. Split out from the
-- main barrel so the (non-trivial) DSL modules and the per-race card
-- definition files can pattern-match on the constructors without
-- importing the barrel — which would form a cycle with the
-- 'allCards' registry that lives there.
module Invasion.Card.Types (module Invasion.Card.Types) where

import Data.Aeson
import Data.Aeson.Key qualified as AesonKey
import Data.Aeson.KeyMap qualified as KeyMap
import Invasion.CardDef
import Invasion.Prelude
import Invasion.Types

data SomeCardDef where
  UnitCardDef :: CardDef 'Unit -> SomeCardDef
  SupportCardDef :: CardDef 'Support -> SomeCardDef
  QuestCardDef :: CardDef 'Quest -> SomeCardDef
  TacticCardDef :: CardDef 'Tactic -> SomeCardDef
  LegendCardDef :: CardDef 'Legend -> SomeCardDef

instance Show SomeCardDef where
  show (UnitCardDef card) = show card
  show (SupportCardDef card) = show card
  show (QuestCardDef card) = show card
  show (TacticCardDef card) = show card
  show (LegendCardDef card) = show card

instance ToJSON SomeCardDef where
  toJSON (UnitCardDef card) = toJSON card
  toJSON (SupportCardDef card) = toJSON card
  toJSON (QuestCardDef card) = toJSON card
  toJSON (TacticCardDef card) = toJSON card
  toJSON (LegendCardDef card) = toJSON card

-- | A card instance: a definition paired with the stable 'UnitKey' that
-- identifies this specific copy across the entire game. The same key is
-- minted when the card is shuffled into the deck, carried through the
-- hand, into play (as a 'UnitDetails' / 'SupportDetails' / …), and back
-- into discard if the card leaves play. Keys never recycle.
--
-- The frontend uses the key as a CSS view-transition name so a card
-- visually morphs from one location to another as state updates.
data Card = Card
  { key :: UnitKey
  , def :: SomeCardDef
  }
  deriving stock Show

-- | Flatten the card definition into a JSON object and splice in the
-- 'key' field, so the wire shape is the existing 'CardDef' surface
-- (code, title, traits, …) plus a stable integer key. Falls back to a
-- tagged object if the inner JSON is somehow not an object (defensive;
-- 'CardDef' always serializes as one today).
instance ToJSON Card where
  toJSON c = case toJSON c.def of
    Object o -> Object (KeyMap.insert (AesonKey.fromString "key") (toJSON c.key) o)
    other -> object ["key" .= c.key, "def" .= other]

-- | Anything that carries a race list. Used by 'isRace' so card
-- bodies can write @unit \`isRace\` Dwarf@ uniformly across card
-- definitions and in-play / departed records. The class definition
-- lives here (alongside the 'SomeCardDef' type) so the per-kind
-- instances in "Invasion.Card.Effects" and the 'SomeCardDef'
-- instance in "Invasion.Card" both attach to the same class without
-- forming an orphan.
class HasRaces a where
  racesOf :: a -> [Race]

instance HasRaces SomeCardDef where
  racesOf = \case
    UnitCardDef cd -> cd.races
    SupportCardDef cd -> cd.races
    QuestCardDef cd -> cd.races
    TacticCardDef cd -> cd.races
    LegendCardDef cd -> cd.races

instance HasRaces (CardDef k) where
  racesOf cd = cd.races

-- | @x \`isRace\` r@ — does the value carry the given race?
isRace :: HasRaces a => a -> Race -> Bool
isRace x r = r `elem` racesOf x

-- | The printed trait list of any card definition, regardless of
-- kind. Used by effects that filter mixed card lists by trait
-- (Chittering Horde reveals "Skaven cards" of any kind).
someCardTraits :: SomeCardDef -> [Trait]
someCardTraits = \case
  UnitCardDef cd -> cd.traits
  SupportCardDef cd -> cd.traits
  QuestCardDef cd -> cd.traits
  TacticCardDef cd -> cd.traits
  LegendCardDef cd -> cd.traits

-- | The printed title of any card definition, regardless of kind. Used
-- by name-matching effects (Squig Trackers' "Squig"/"Squigs" search).
someCardTitle :: SomeCardDef -> String
someCardTitle = \case
  UnitCardDef cd -> cd.title
  SupportCardDef cd -> cd.title
  QuestCardDef cd -> cd.title
  TacticCardDef cd -> cd.title
  LegendCardDef cd -> cd.title

-- | The card code of any card definition, regardless of kind.
someCardCode :: SomeCardDef -> CardCode
someCardCode = \case
  UnitCardDef cd -> cd.code
  SupportCardDef cd -> cd.code
  QuestCardDef cd -> cd.code
  TacticCardDef cd -> cd.code
  LegendCardDef cd -> cd.code

-- | The printed keywords of any card definition, regardless of kind.
-- Used by the combat Ambush step, which reads 'Ambush' off facedown
-- developments without knowing their card kind.
someCardKeywords :: SomeCardDef -> [Keyword]
someCardKeywords = \case
  UnitCardDef cd -> cd.keywords
  SupportCardDef cd -> cd.keywords
  QuestCardDef cd -> cd.keywords
  TacticCardDef cd -> cd.keywords
  LegendCardDef cd -> cd.keywords

-- | The 'Ambush' X cost of a card, if it carries the keyword. Returns
-- the smallest printed value if (improbably) more than one is present.
someCardAmbushCost :: SomeCardDef -> Maybe Int
someCardAmbushCost d =
  case [n | Ambush n <- someCardKeywords d] of
    [] -> Nothing
    ns -> Just (minimum ns)

-- | The printed resource cost of any card definition, as an 'Int'.
-- 'Variable' ("X") costs report 0 — the callers that read this (e.g.
-- Vaedra Bloodsworn, gaining power equal to a discarded card's cost)
-- treat an unknown X as nothing.
someCardCost :: SomeCardDef -> Int
someCardCost d = case asFixed d of
  Fixed n -> n
  Variable -> 0
  where
    asFixed = \case
      UnitCardDef cd -> cd.cost
      SupportCardDef cd -> cd.cost
      QuestCardDef cd -> cd.cost
      TacticCardDef cd -> cd.cost
      LegendCardDef cd -> cd.cost

-- | The printed loyalty of any card definition. Read by the
-- "discard a card from your hand with X loyalty …" cards (Storm of
-- Change, Inflame, Doubling of the Guard) to scale their effect.
someCardLoyalty :: SomeCardDef -> Int
someCardLoyalty = \case
  UnitCardDef cd -> cd.loyalty
  SupportCardDef cd -> cd.loyalty
  QuestCardDef cd -> cd.loyalty
  TacticCardDef cd -> cd.loyalty
  LegendCardDef cd -> cd.loyalty
