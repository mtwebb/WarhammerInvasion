{-# LANGUAGE TemplateHaskell #-}
module Invasion.Types (module Invasion.Types) where

import Data.String (IsString)
import Data.Text qualified as T
import GHC.Records
import Invasion.Prelude
import Data.Aeson
import Data.Aeson.TH
import Data.Aeson.Types (toJSONKeyText)

newtype CardCode = CardCode String
  deriving newtype (Eq, Ord, Show, IsString, ToJSON, FromJSON)

data PlayerKey = Player1 | Player2
  deriving stock (Show, Eq, Ord)

instance HasField "next" PlayerKey PlayerKey where
  getField Player1 = Player2
  getField Player2 = Player1

data Number = Fixed Int | Variable
  deriving stock (Show, Eq)

-- | Printed keyword abilities. Lives here (rather than 'Invasion.CardDef')
-- so 'Invasion.Modifier' can reference it without a module cycle — the
-- 'GainKeyword' modifier grants keywords (Swift-moving Storm: Scout).
data Keyword
  = Toughness Number
  | BattlefieldOnly
  | KingdomOnly
  | QuestOnly
  | Scout
  | Limited
  | DamageCannotBeCancelled
  | Counterstrike Int
  | Raider Int
    -- ^ Raider X (Eternal War cycle): after combat damage is applied,
    -- the attacking player gains resources equal to the combined
    -- Raider X of all his attacking units that survived combat.
  | PlayInOpponentArea
    -- ^ Quest enters play in the opponent's play area while remaining
    -- under the playing player's control. Used by Dominion of Chaos.
  | PlayInOpponentControl
    -- ^ "Invasion" quests: played from your hand but enters play in an
    -- opponent's area AND under that opponent's control.
  | Ambush Int
    -- ^ Ambush X (Hidden Kingdoms): may be played facedown as a
    -- development, then flipped faceup for X resources during the
    -- combat Ambush step.
  | Savage Int
    -- ^ Savage X (Lizardmen): after this unit is dealt damage and
    -- survives, its controller may deal X damage to a target unit in a
    -- corresponding zone.
  | OrderOnly
    -- ^ Neutral-card restriction: cannot be included in a Destruction deck.
  | DestructionOnly
    -- ^ Neutral-card restriction: cannot be included in an Order deck.
  | LimitOneHeroPerZone
    -- ^ Hero restriction: only one Hero per zone across both players.
  | PlayAnytime
    -- ^ "You may play this unit from your hand any time you could take
    -- an action." (Nordland Halberdiers.)
  | Necromancy
    -- ^ "You may play this card from your discard pile…" (March of the
    -- Damned Undead.)
  | Feared Int
    -- ^ "Feared X (while attacking, blank the text box of X target
    -- units except for Traits)."
  | Grudge
    -- ^ "Grudge" supports: "When your capital is dealt combat damage,
    -- you may put this card into play from your hand."
  deriving stock (Show, Eq)

data CardKind = Unit | Support | Quest | Tactic | Legend | DraftFormat
  deriving stock (Eq, Show)

-- | Identifies which of a capital's three zones something belongs to.
-- Used both to tag a 'Zone' (its identity) and to record where a unit
-- has been played.
data ZoneKind = KingdomZone | QuestZone | BattlefieldZone
  deriving stock (Show, Eq, Ord)

newtype UnitKey = UnitKey Int
  deriving stock (Show, Eq, Ord)

data RefKind = Target | Source

type role Ref phantom
newtype Ref (k :: RefKind) = UnitRef UnitKey
  deriving stock (Show, Eq, Ord)

class Reference a where
  toRef :: a -> Ref k

data Phase = KingdomPhase | QuestPhase | CapitalPhase | BattlefieldPhase
  deriving stock (Show, Eq, Ord)

-- | Phase that follows the given one, or 'Nothing' if the turn ends.
nextPhase :: Phase -> Maybe Phase
nextPhase = \case
  KingdomPhase -> Just QuestPhase
  QuestPhase -> Just CapitalPhase
  CapitalPhase -> Just BattlefieldPhase
  BattlefieldPhase -> Nothing

data Race = Dwarf | Empire | HighElf | Chaos | Orc | DarkElf
  deriving stock (Show, Eq)


mconcat
  [ deriveToJSON defaultOptions ''Ref
  , deriveToJSON defaultOptions ''Keyword
  , deriveJSON defaultOptions ''UnitKey
  , deriveJSON defaultOptions ''PlayerKey
  , deriveToJSON defaultOptions ''Number
  , deriveToJSON defaultOptions ''CardKind
  , -- 'Race' serializes as the bare constructor name (e.g. @"Dwarf"@).
    -- 'tagSingleConstructors' is a holdover from when this was a single
    -- constructor; with the full six-race set it's harmless. We need
    -- 'FromJSON' too because the lobby ships 'GameSelectStarter' frames
    -- carrying a 'Race' picked by the seated player.
    deriveJSON
      defaultOptions {tagSingleConstructors = True, allNullaryToStringTag = True}
      ''Race
  , deriveToJSON defaultOptions ''Phase
  , deriveJSON defaultOptions ''ZoneKind
  ]

instance ToJSONKey (Ref k)

-- Text-keyed map encodings: the empty-body default is
-- 'ToJSONKeyValue', which serializes a @Map k v@ as an ARRAY of
-- [key, value] pairs. The frontend indexes 'handPlayability' (and the
-- wire redaction layer walks 'developmentCards') as JSON OBJECTS, so
-- these keys must encode as text.
instance ToJSONKey ZoneKind where
  toJSONKey = toJSONKeyText (T.pack . show)
instance ToJSONKey UnitKey where
  toJSONKey = toJSONKeyText \(UnitKey n) -> T.pack (show n)
instance ToJSONKey PlayerKey where
  toJSONKey = toJSONKeyText (T.pack . show)
