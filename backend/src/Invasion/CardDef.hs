{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.CardDef (module Invasion.CardDef) where

import Control.Monad.IO.Class (MonadIO)
import Data.Aeson
import Data.Aeson.TH
import Data.Monoid (Sum (..))
import Data.Text (Text)
import Invasion.Player (Player)
import Invasion.Prelude
import Invasion.Types
import Queue (HasQueue)
import {-# SOURCE #-} Invasion.Engine (HasPromptIO)
import {-# SOURCE #-} Invasion.Game (Game, HasGame)
import {-# SOURCE #-} Invasion.Message (Message)

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
    -- Multiple instances on one unit (e.g. printed + an attachment)
    -- add together. Handled centrally by the combat pipeline
    -- ('FireRaiderResources'), mirroring 'Scout'.
  | PlayInOpponentArea
    -- ^ Quest enters play in the opponent's play area while remaining
    -- under the playing player's control. Used by Dominion of Chaos.
  | Ambush Int
    -- ^ Ambush X (Hidden Kingdoms): the card may be played facedown as a
    -- development, then flipped faceup for X resources during the combat
    -- Ambush step (after Declare Attackers, before Declare Defenders) on
    -- a development in the defending zone. The card becomes its printed
    -- type; a flipped unit must be declared as a defender that step.
  | OrderOnly
    -- ^ Neutral-card restriction: cannot be included in a Destruction
    -- (Chaos / Orc / Dark Elf) deck.
  | DestructionOnly
    -- ^ Neutral-card restriction: cannot be included in an Order
    -- (Empire / Dwarf / High Elf) deck.
  | LimitOneHeroPerZone
    -- ^ Hero restriction. While a player controls a Hero in a given
    -- zone, neither player may put, play, or move another Hero into
    -- that same zone (FAQ 2.2 clarification).
  | PlayAnytime
    -- ^ "You may play this unit from your hand any time you could
    -- take an action." (Nordland Halberdiers.) The engine relaxes the
    -- capital-window gate to tactic timing for cards carrying this.
  deriving stock (Show, Eq)

data Cost = PayResources Number | NoCost

data Trait
  = Warrior
  | Spell
  | Engineer
  | Elite
  | Slayer
  | Priest
  | Hero
  | Ranger
  | Rune
  | Building
  | Attachment
  | Weapon
  | Siege
  | Daemon
  | Creature
  | Sorceror
    -- ^ Printed spelling on the cards.
  | Knight
  | Cavalry
  | Mission
  | QuestTrait
  | Wasteland
  | CapitalCenter
  | Rift
  | Relic
  | Banner
  | Goblin
  | Mage
  | Mutation
  | Noble
  | Shaman
  | Skill
  | Warpstone
  | Zealot
  | Skaven
  | WitchHunter
    -- ^ Printed as both "Witchhunter." (Marius the Righteous) and
    -- "Witch Hunter." (Zealot Hunter); one constructor covers both.
  | Hex
  | Vault
    -- ^ Resource-generating support trait (Wealth of the Hold).
  | Berserker
    -- ^ Aggressive melee unit trait (Norse Clansman, Ded Scary Boy).
  | Dragon
    -- ^ Dragon creature trait (Chaos Dragon, Great Fire Dragon).
  | WarMachine
    -- ^ War Machine trait (Steel Behemoth, Dead-Eye Cannon Crew).
  | Musician
    -- ^ Musician trait (Imperial Drummer, Dragon Caller).
  | StandardBearer
    -- ^ Standard Bearer trait (Doom Bearer, Banna Thief, Bannerman of
    -- the Crag). Distinct from the support-side 'Banner' trait.
  | Location
    -- ^ Location support trait (Straits of Lothern, Capital-cycle locations).
  | Fortification
    -- ^ Fortification support trait (Harpy Aerie, Wall of Maggots).
  | Epic
    -- ^ Half of the printed "Epic Spell." trait line. Epic spells are
    -- excluded from cost-reduction effects like Runefang of Solland.
  | Troll
    -- ^ Troll creature trait (River Troll).
  | WitchElf
    -- ^ Witch Elf trait (Frenzied Witch Elf).
  | Disease
    -- ^ Disease trait on disease spells (Plague Bomb).
  | Cultist
    -- ^ Cultist trait (Scheming Cultist, Herald of Change, Esli'an).
  | Bretonnian
    -- ^ Bretonnian trait (Grail Knight, Battle Pilgrims).
  | Thief
    -- ^ Thief trait (Mountain Brigands, Treasure Thieves).
  | Slave
    -- ^ Slave trait (Dwarf Slaves).
  | Environment
    -- ^ Environment support trait (Higher Ground).
  | Messenger
    -- ^ Messenger trait (Envoy from Averlorn).
  | Martyr
    -- ^ Martyr trait (Walking Sacrifice).
  | Initiate
    -- ^ Initiate trait (Initiate of Saphery).
  | Lizardmen
    -- ^ Lizardmen trait (Skinks of Sotek, Saurus Warriors, the March
    -- of the Damned Order minor-faction cards).
  | Undead
    -- ^ Undead trait (Crypt Ghouls, Skeletal Horde, the March of the
    -- Damned Destruction minor-faction cards).
  | Ship
    -- ^ Ship trait (Elven Warship, Corsair Raider).
  | Vampire
    -- ^ Vampire trait (Blood Dragon Knight, the Legends Undead cards).
  | WoodElf
    -- ^ Wood Elf trait (Wild Rider, Shadow Sentinel, Protective Spites).
  | Condition
    -- ^ Condition support trait (Big Guns, Garrisoned).
  | Item
    -- ^ Item support trait (Arcane Orrery).
  | Pyramid
    -- ^ Pyramid support trait (Lizardmen temples in Hidden Kingdoms:
    -- Great Temple of Tlazcotl, Sun Temple of Chotec, Ziggurat of Quetli).
  | Artefact
    -- ^ Artefact support trait — the self-protecting attachments
    -- (Dawnstar Sword, Eye of Sheerian, Windcatcher Prism, …).
  | Shield
    -- ^ Shield item trait (Shield of Aeons).
  | Arcane
    -- ^ Arcane artefact trait (Windcatcher Prism, Star Crown Fragments).
  deriving stock (Show, Eq)

mconcat
  [ deriveToJSON defaultOptions ''Keyword
  , deriveToJSON defaultOptions ''Trait
  ]

-- | Open type family of in-play self-references, indexed by card kind.
-- Instances are declared next to each kind's in-play record in
-- 'Invasion.Entity'.
type family InPlay (k :: CardKind)

-- | What kind of target an action requires. The engine validates the
-- supplied 'ActionTarget' against this schema before invoking the
-- action's effect.
data TargetSchema
  = NoTargetSchema
    -- ^ The action takes no target.
  | AnyUnitTargetSchema
    -- ^ Any unit currently in play.
  | EnemyUnitTargetSchema
    -- ^ A unit controlled by the opponent.
  | FriendlyUnitTargetSchema
    -- ^ A unit controlled by the player triggering the action.
  | AnyZoneTargetSchema
    -- ^ A zone of any player.
  | EnemyZoneTargetSchema
    -- ^ A zone controlled by the opponent.
  | SupportTargetSchema
    -- ^ A free-standing support in play.
  deriving stock (Show, Eq)

-- | The concrete target supplied with a 'TriggerCardAction' message.
data ActionTarget
  = NoTarget
  | TargetUnit UnitKey
  | TargetZone PlayerKey ZoneKind
  | TargetSupport UnitKey
  deriving stock (Show, Eq)

mconcat
  [ deriveToJSON defaultOptions ''TargetSchema
  , deriveToJSON defaultOptions ''ActionTarget
  ]

-- | The bespoke effect a card's action runs once costs have been
-- paid and the target has been validated. The body receives an
-- 'ActionUsage' record with the firing player, this in-play card,
-- the resolved target, and receipts for any extra costs paid.
newtype ActionEffect k = ActionEffect
  { unEffect
      :: forall m
       . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
      => ActionUsage k -> m ()
  }

-- | Non-resource costs an action may impose. The engine validates
-- every extra cost before firing the action (e.g. "sacrifice a unit"
-- requires the player to control at least one unit) and prompts for
-- any choices needed to pay them. Failing the validation means the
-- action can't be triggered at all — no resources are spent.
data ExtraCost
  = SacrificeUnit
    -- ^ Sacrifice one of your own units. Engine prompts the firing
    -- player; the picked unit is destroyed before the action's
    -- effect fires.
  | SacrificeSelf
    -- ^ Sacrifice the card hosting this action (Flagellants, Snotling
    -- Saboteurs, Thick-Skinned). Units are destroyed, supports
    -- discarded, before the effect fires.
  | CorruptSelf
    -- ^ Corrupt the unit hosting this action (Clan Rats, Poison Wind
    -- Globadiers, Deathmaster Sniktch). Validation requires the host
    -- to be an uncorrupted unit.
  | SacrificeDevelopment
    -- ^ Sacrifice one of your own developments (Reckless Engineer,
    -- Mineshaft Engineer). Validation requires at least one development
    -- in any of the firing player's zones; the engine prompts for which
    -- zone when more than one has developments.
  deriving stock (Show, Eq)

-- | A receipt for one extra cost paid during action triggering. Cards
-- whose effect depends on which units were sacrificed / cards
-- discarded inspect 'ActionUsage.payments' to find the receipts.
data Payment
  = SacrificedUnit UnitKey
    -- ^ Key of the unit destroyed to pay a 'SacrificeUnit' (or
    -- 'SacrificeSelf') cost.
  | CorruptedSelf
    -- ^ Receipt for a paid 'CorruptSelf' cost.
  | SacrificedDevelopment ZoneKind
    -- ^ Receipt for a paid 'SacrificeDevelopment' cost: the zone the
    -- development was popped from.
  deriving stock (Show, Eq)

-- | Everything an action's effect body needs to know when it fires:
-- who triggered it, the in-play card the action lives on, the
-- target the player picked (if any), and receipts for any extra
-- costs paid.
data ActionUsage k = ActionUsage
  { user :: PlayerKey
  , self :: InPlay k
  , target :: ActionTarget
  , payments :: [Payment]
  }

-- | A single Action ability printed on a card. Cards may declare zero
-- or more actions; the engine enumerates them during action windows.
-- Field names are prefixed with @action@ so they can't shadow
-- 'CardDef'\'s own @cost@/@target@ fields under
-- @DuplicateRecordFields@.
data ActionDef k = ActionDef
  { actionName :: Text
  , actionCost :: Int
  , actionExtraCosts :: [ExtraCost]
    -- ^ Non-resource costs (sacrifice, discard, …) the engine must
    -- validate and pay before firing the effect.
  , actionTarget :: TargetSchema
  , actionEffect :: ActionEffect k
  , availableInZone :: Maybe ZoneKind
    -- ^ Zone-gate on the action's availability. When 'Just z', the
    -- action only triggers while the host card sits in zone 'z' —
    -- mirrors the printed "[Zone]." prefix. 'Nothing' means the
    -- action is always available.
  , actionOpponentOnly :: Bool
    -- ^ "Only an opponent may trigger this ability." (Morathi's
    -- Pegasus.) When set, the engine accepts the trigger only from
    -- the host's opponent, who also pays the action's cost.
  }

-- | Open type family of per-kind extras records. The engine queries
-- these fields when it would otherwise have to case on a card's
-- 'code'. A new kind's instance is declared next to its in-play
-- record in 'Invasion.Entity'.
type family Extras (k :: CardKind)

-- | Per-kind defaults. Each instance provides a record whose fields
-- are no-ops — cards override the slices they care about via the
-- builder helpers in 'Invasion.Card'.
class HasDefaultExtras (k :: CardKind) where
  defaultExtras :: Extras k

-- | Unit-specific tunables read by the engine each turn / each combat.
data UnitExtras = UnitExtras
  { selfPowerBonus :: Game -> InPlay Unit -> Int
    -- ^ Game-state-derived self power bonus (Troll Slayers,
    -- Durgnar the Bold, Korhil, Crone Hellebron, Skulltaker). Folded
    -- into the unit's cached @effectivePower@ each step.
  , combatPowerBonus :: Game -> InPlay Unit -> Int
    -- ^ Extra damage the unit deals during combat (Lord of Khorne
    -- per burning zone, Gorbad Ironclaw while attacking). The
    -- function inspects @g.combat@ to decide attacker vs defender
    -- vs out-of-combat behavior.
  , unitAuraPower :: Game -> InPlay Unit -> InPlay Unit -> Int
    -- ^ Power this unit grants to another unit while both are in
    -- play (Karl Franz buffs other Empire units; Templar of Sigmar
    -- buffs other Warriors in the battlefield). Args: game, source
    -- (this unit), target unit.
  , canAttackZone :: Game -> PlayerKey -> ZoneKind -> InPlay Unit -> Bool
    -- ^ Whether this unit may be declared as an attacker against
    -- the named defender zone (Sworn of Khorne requires a corrupted
    -- defender). Default: always True.
  , damageCap :: Maybe Int
    -- ^ Per-turn damage cap on this unit (Daemonettes of Slaanesh).
  , corruptsOnCombatDamage :: Bool
    -- ^ Corrupt any enemy this unit dealt non-zero combat damage to
    -- (Plaguebearers of Nurgle, Beasts of Nurgle).
  , extraTargetTax :: Game -> PlayerKey -> InPlay Unit -> Int
    -- ^ Extra resources the named (effect-firing) player must pay to
    -- target this unit (King Kazador: 3 for opponents). Args: game,
    -- the player firing the effect, this unit.
  , damageMultiplierWhileInPlay :: Int
    -- ^ Multiplier on every applied damage event while this unit is
    -- in play (Bloodletter: 2). Multipliers stack multiplicatively.
  , runtimeEffects :: Game -> InPlay Unit -> ActiveEffect
    -- ^ Per-tick static effects authored via the high-level zone-gate
    -- builders ('battlefield', 'kingdom', 'quest') in 'Invasion.Card'.
    -- Returns an 'ActiveEffect' monoid that the engine folds into the
    -- unit's cached stats alongside 'selfPowerBonus' and aura sources.
  , unitCostAdjustment :: Game -> InPlay Unit -> PlayerKey -> CardCodeFilter -> Int
    -- ^ Cost-of-play adjustment this in-play unit imposes on another
    -- card being played (Nuln Tinkerers: -1 on the controller's
    -- first support of the turn). Mirrors the support-side
    -- 'globalCostAdjustment' slot.
  , unitAuraToughness :: Game -> InPlay Unit -> InPlay Unit -> Int
    -- ^ Extra Toughness this in-play unit grants another unit while
    -- both are in play (Big 'Uns: +1 toughness to my damaged units
    -- while it's on the battlefield). Sums across stacked sources.
  , unitAuraHp :: Game -> InPlay Unit -> InPlay Unit -> Int
    -- ^ Extra hit points this in-play unit grants another unit while
    -- both are in play (Mountain Sentry: +2 HP to Rangers in its
    -- zone). The unit-side mirror of 'supportAuraHP'; folded into the
    -- target's 'effectiveMaxHP' alongside 'unitAuraToughness'.
  , preDamageRedirect :: Game -> InPlay Unit -> Int -> Maybe PreDamageRedirect
    -- ^ Consulted by the engine's 'DealDamageToUnit' handler BEFORE
    -- the damage lands. Args: game, the unit about to take damage,
    -- the inbound (post-multiplier, post-toughness) amount. Return
    -- 'Just plan' to claim some or all of the damage and route it
    -- elsewhere; 'Nothing' lets the damage land normally.
  , selfToughnessBonus :: Game -> InPlay Unit -> Int
    -- ^ Game-state-derived bonus to the unit's own Toughness (Ludwig
    -- Schwarzheim: X = experiences attached). Folded into
    -- 'totalToughness' alongside the printed keyword and auras. Distinct
    -- from 'Toughness Variable', which the engine reads as
    -- developments-in-zone.
  , selfCounterstrikeBonus :: Game -> InPlay Unit -> Int
    -- ^ Game-state-derived "Counterstrike X" value (Anlec Lookout:
    -- X = highest loyalty on a Dark Elf card you control; Wardancer:
    -- X = developments in this zone). Added to the printed
    -- 'Counterstrike' keyword total when the unit fires Counterstrike
    -- in combat.
  , selfHPBonus :: Game -> InPlay Unit -> Int
    -- ^ Game-state-derived bonus to the unit's own max HP (Cold One
    -- Chariot: X = developments in this zone). Folded into the cached
    -- @effectiveMaxHP@ alongside attachment and aura HP.
  , cancelAllDamageWhen :: Game -> InPlay Unit -> Bool
    -- ^ "Cancel all damage to this unit while CONDITION." (Gustav the
    -- Bear.) Checked on the cancellable damage path only —
    -- uncancellable damage ignores it, per the keyword rules.
  , perHitDamageCap :: Maybe Int
    -- ^ "Whenever this unit is assigned damage, cancel all but N of
    -- that damage." (Dragonmage: 1.) Caps each cancellable damage
    -- event after Toughness; distinct from the per-turn 'damageCap'.
  , cannotDefend :: Bool
    -- ^ Printed "This unit cannot defend." (Clan Moulder's Elite.)
    -- Excluded from the defender candidate pool.
  , attackEligibleZones :: [ZoneKind]
    -- ^ Zones this unit may attack from. Default battlefield only;
    -- Greyseer Thanquol attacks from anywhere, Dragonslayer also from
    -- the quest zone.
  , destroyedToZone :: Game -> InPlay Unit -> Maybe ZoneKind
    -- ^ Destruction replacement: instead of going to the discard
    -- pile, re-enter play in the named zone (Vigilant Pistoliers:
    -- battlefield -> kingdom). Attachments are still discarded and
    -- leave-play hooks still fire.
  , defenderDamageToAllAttackers :: Bool
    -- ^ "When this unit defends, it deals its combat damage to all
    -- attacking units." (Juvenile Wyvern.) The assign step gives each
    -- attacker the unit's full combat damage instead of pooling it.
  }

-- | Card-supplied redirect plan returned from 'preDamageRedirect'.
-- The engine pulls @amount@ off the original target's incoming
-- damage and runs @run@, which is expected to enqueue the
-- redirected damage and mark whatever per-turn state the card uses
-- to avoid double-triggering.
data PreDamageRedirect = PreDamageRedirect
  { amount :: Int
  , run :: ActionEffect 'Unit
    -- ^ Reuses 'ActionEffect' purely to get the same constrained
    -- monad (HasGame + HasPromptIO + HasQueue Message). The
    -- 'ActionUsage' it receives carries the unit being redirected
    -- and the firing player (always the unit's controller).
  }

-- | Accumulator for the runtime output of an 'EffectM' builder block.
-- Today carries a power-bonus contribution; future fields can carry
-- HP bonuses, conditional keywords, etc., without changing the engine
-- read path.
data ActiveEffect = ActiveEffect
  { bonusPower :: Sum Int
  }

instance Semigroup ActiveEffect where
  a <> b = ActiveEffect {bonusPower = a.bonusPower <> b.bonusPower}

instance Monoid ActiveEffect where
  mempty = ActiveEffect (Sum 0)

-- | Sum of the power bonus contributions in an 'ActiveEffect'. The
-- engine calls this when computing 'effectivePower'.
activeBonusPower :: ActiveEffect -> Int
activeBonusPower e = getSum e.bonusPower

-- | Support-specific tunables.
data SupportExtras = SupportExtras
  { attachmentPowerBonus :: Int
    -- ^ Static power contribution when this support is attached to
    -- a unit (Daemonsword, Hammer of Sigmar, etc.).
  , attachmentHPBonus :: Int
    -- ^ Static HP contribution when attached (Daemonsword).
  , grantsUncancellableDamage :: Bool
    -- ^ While attached, the host unit's combat damage is
    -- uncancellable (Hammer of Sigmar).
  , supportAuraPower :: Game -> InPlay Support -> InPlay Unit -> Int
    -- ^ Power this support grants to a unit (Iron Tower → Chaos
    -- units in battlefield; Cauldron of Blood → Witch Elves; Da Bad
    -- Moon static slice).
  , supportCombatBonus :: Game -> InPlay Support -> InPlay Unit -> Int
    -- ^ Extra combat damage this support grants to a unit (Rift of
    -- Battle gives every unit +1; Organ Gun adds +2 while the host
    -- defends; Da Bad Moon and Big Boss's Banner buff Orc
    -- attackers).
  , zonePowerBonus :: Game -> InPlay Support -> ZoneKind -> Int
    -- ^ Extra power this support contributes to a zone of its
    -- controller (Lighthouse of Lothern, Rift of Chaos).
  , globalCostAdjustment :: Game -> InPlay Support -> PlayerKey -> CardCodeFilter -> Int
    -- ^ Adjustment this support imposes on the printed cost of
    -- another card being played. Args: game, this support, playing
    -- player, a filter describing the target card.
    --
    -- Imperial Crown: -1 for the controller's Empire heroes while
    -- in their kingdom. Master Rune of Dismay: +1 for the opponent's
    -- units while in the opponent's kingdom.
  , runeOfFortitudeTax :: Bool
    -- ^ Marks the printed Rune of Fortitude effect: every attacker
    -- of the zone owes 1 resource to its controller or eats a
    -- @-1@ power penalty for the combat.
  , supportTargetTax :: Game -> InPlay Support -> PlayerKey -> InPlay Unit -> Int
    -- ^ Extra resources an effect must pay to target one of this
    -- support's controller's units (Church of Sigmar: 1 for
    -- opponents while in kingdom). Args: game, this support, the
    -- player firing the effect, the unit being targeted. Stacks
    -- with King Kazador-style per-unit 'extraTargetTax'.
  , supportAuraHP :: Game -> InPlay Support -> InPlay Unit -> Int
    -- ^ Extra HP this support grants (positive) or subtracts
    -- (negative) from a unit while both are in play (Horrific
    -- Mutation: -1 HP to defenders while host attacks). Read by
    -- 'recomputeUnitStats' alongside 'attachmentHPBonus'.
  , capitalShieldPerTurn :: Bool
    -- ^ Marks "cancel 1 damage to your capital each turn" supports
    -- (Contested Fortress). Evaluated at damage time by
    -- 'DealDamageToZone'; the engine tracks per-source usage in
    -- 'Game.capitalDefenseUsed' so the cancel fires at most once per
    -- turn regardless of whose turn it is.
  , supportAuraToughness :: Game -> InPlay Support -> InPlay Unit -> Int
    -- ^ Extra Toughness this support grants a unit (Gromril Armour:
    -- +1 to the attached unit). Summed into 'totalToughness'
    -- alongside the unit-side 'unitAuraToughness'.
  , searchDepthBonus :: Game -> InPlay Support -> PlayerKey -> Int
    -- ^ "Whenever you search your deck, you may search an additional
    -- card." (Scout Camp.) Added to the depth of every
    -- 'searchTopOfDeck' run by the named player.
  , tacticDamageBonus :: Game -> InPlay Support -> PlayerKey -> Int
    -- ^ "Whenever a tactic you play deals damage to one or more
    -- targets, deal an additional damage to each target." (Hellcannon
    -- Reserves.) Consulted by the damage handlers while
    -- 'Game.tacticDamageContext' names the playing player.
  , capitalDamageDoubler :: Game -> InPlay Support -> PlayerKey -> Bool
    -- ^ "While attached unit is attacking, double all damage dealt to
    -- the defending opponent's capital." (Basha's Bloodaxe.) Args:
    -- game, this support, the player whose capital is being damaged.
  , blanksHost :: Bool
    -- ^ "Treat attached unit as though its printed text box were
    -- blank (except for Traits)." (Witch Hag's Curse.) The engine
    -- suppresses the host's receive, actions, keywords, and extras
    -- while attached.
  , hostDestroyRansom :: Maybe Int
    -- ^ "If attached unit would be destroyed, you may pay N resources
    -- to (instead of destroying it) leave it in play and remove all
    -- damage from it." (Hydra Blade: 2.)
  , revertToUnit :: Maybe (CardDef Unit)
    -- ^ Set on synthetic attachments that are physically unit cards
    -- (Vigilant Elector after its quest action). When the attachment
    -- leaves play, the discard pile receives this unit def instead of
    -- the synthetic support def.
  , grantsHostDamageImmunity :: Game -> InPlay Support -> InPlay Unit -> Bool
    -- ^ "Cancel all (cancellable) damage assigned to the attached unit
    -- while CONDITION." (Shield of Aeons: while its Hero or legend host
    -- is participating in combat.) OR-ed into the host's
    -- 'cancelAllDamageWhen' immunity check by the damage handler;
    -- uncancellable damage ignores it, like the unit-side field.
  , selfUntargetable :: Game -> InPlay Support -> Maybe Bool
    -- ^ "This card cannot be targeted by card effects." Returns
    -- @Just opponentOnly@ while the immunity is active (@True@ blocks
    -- only the opponent, @False@ blocks everyone), @Nothing@ when the
    -- support is freely targetable. The self-protecting artefacts
    -- (Dawnstar Sword, Eye of Sheerian, …) return a constant
    -- @Just False@; Helm of Fortune gates it on the host questing.
    -- Consulted by 'targetableBy' alongside the modifier map.
  }

-- | Static metadata about a card that's currently being played, used
-- by external cost-adjustment hooks. Decouples the @globalCostAdjustment@
-- callback from any one @CardDef k@ so it can be invoked uniformly
-- across all card kinds. Fields are prefixed with @cf@ so record-update
-- syntax doesn't become ambiguous with 'CardDef'\'s @races@/@traits@/…
data CardCodeFilter = CardCodeFilter
  { cfCode :: CardCode
  , cfKind :: CardKind
  , cfRaces :: [Race]
  , cfTraits :: [Trait]
  }
  deriving stock Show

-- | Quest-specific tunables.
data QuestExtras = QuestExtras
  { capitalRedirectFirstDamage :: Game -> InPlay Quest -> Bool
    -- ^ While 'True', the first point of damage dealt to the
    -- controller's capital each turn is redirected to a target unit
    -- or capital section of the controller's choice (Defend the
    -- Border while it holds 3+ resource tokens). Evaluated at damage
    -- time by 'DealDamageToZone'; once-per-turn usage is tracked in
    -- 'Game.capitalDefenseUsed'.
  , questerDefendsAnyZone :: Bool
    -- ^ "Any unit questing on this card can defend any of your zones
    -- when they are attacked." (Protect the Empire.) Expands the
    -- defender candidate pool past the attacked zone.
  , paysForAttachments :: Bool
    -- ^ "You may spend resources from this card to pay for Attachment
    -- cards that are played from your hand." (Dat's Mine!.) The
    -- attachment-play payment path drains this quest's tokens before
    -- the resource pool.
  , questerAttacksAnyZone :: Bool
    -- ^ "Any unit questing on this card may attack as though it were in
    -- your battlefield." (Sack Tor Aendris.) Lets the questing unit be
    -- declared as an attacker despite sitting in the quest zone.
  , questUnitAuraPower :: Game -> InPlay Quest -> InPlay Unit -> Int
    -- ^ Continuous power this quest grants the controller's units
    -- (Night Raids while it holds 3+ resource tokens). Folded into each
    -- unit's effective power, like the unit/support auras.
  }

-- | Tactic-specific tunables. Empty for now.
data TacticExtras = TacticExtras

-- | Legend-specific tunables. Empty for now.
data LegendExtras = LegendExtras

type instance Extras Unit = UnitExtras
type instance Extras Support = SupportExtras
type instance Extras Quest = QuestExtras
type instance Extras Tactic = TacticExtras
type instance Extras Legend = LegendExtras

instance HasDefaultExtras Unit where
  defaultExtras = UnitExtras
    { selfPowerBonus = \_ _ -> 0
    , combatPowerBonus = \_ _ -> 0
    , unitAuraPower = \_ _ _ -> 0
    , canAttackZone = \_ _ _ _ -> True
    , damageCap = Nothing
    , corruptsOnCombatDamage = False
    , extraTargetTax = \_ _ _ -> 0
    , damageMultiplierWhileInPlay = 1
    , runtimeEffects = \_ _ -> mempty
    , unitCostAdjustment = \_ _ _ _ -> 0
    , unitAuraToughness = \_ _ _ -> 0
    , unitAuraHp = \_ _ _ -> 0
    , preDamageRedirect = \_ _ _ -> Nothing
    , selfToughnessBonus = \_ _ -> 0
    , selfCounterstrikeBonus = \_ _ -> 0
    , selfHPBonus = \_ _ -> 0
    , cancelAllDamageWhen = \_ _ -> False
    , perHitDamageCap = Nothing
    , cannotDefend = False
    , attackEligibleZones = [BattlefieldZone]
    , destroyedToZone = \_ _ -> Nothing
    , defenderDamageToAllAttackers = False
    }

instance HasDefaultExtras Support where
  defaultExtras = SupportExtras
    { attachmentPowerBonus = 0
    , attachmentHPBonus = 0
    , grantsUncancellableDamage = False
    , supportAuraPower = \_ _ _ -> 0
    , supportCombatBonus = \_ _ _ -> 0
    , zonePowerBonus = \_ _ _ -> 0
    , globalCostAdjustment = \_ _ _ _ -> 0
    , supportTargetTax = \_ _ _ _ -> 0
    , supportAuraHP = \_ _ _ -> 0
    , runeOfFortitudeTax = False
    , capitalShieldPerTurn = False
    , supportAuraToughness = \_ _ _ -> 0
    , searchDepthBonus = \_ _ _ -> 0
    , tacticDamageBonus = \_ _ _ -> 0
    , capitalDamageDoubler = \_ _ _ -> False
    , blanksHost = False
    , hostDestroyRansom = Nothing
    , revertToUnit = Nothing
    , grantsHostDamageImmunity = \_ _ _ -> False
    , selfUntargetable = \_ _ -> Nothing
    }

instance HasDefaultExtras Quest where
  defaultExtras = QuestExtras
    { capitalRedirectFirstDamage = \_ _ -> False
    , questerDefendsAnyZone = False
    , paysForAttachments = False
    , questerAttacksAnyZone = False
    , questUnitAuraPower = \_ _ _ -> 0
    }

instance HasDefaultExtras Tactic where
  defaultExtras = TacticExtras

instance HasDefaultExtras Legend where
  defaultExtras = LegendExtras

-- | A card's reaction to engine events. Wrapped in a newtype because
-- record fields can't directly hold a polymorphic function. The
-- constraints (@HasGame@, @MonadIO@) describe the engine capabilities
-- card code is allowed to use; widen them when card behavior needs
-- more.
newtype Receive k = Receive
  { unReceive
      :: forall m
       . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
      => Message -> Player -> InPlay k -> m ()
  }

-- | No-op receiver: the default for cards without bespoke behavior.
noReceive :: Receive k
noReceive = Receive \_ _ _ -> pure ()

data CardDef (k :: CardKind) = CardDef
  { code :: CardCode
  , title :: String
  , kind :: CardKind
  , races :: [Race]
  , cost :: Number
  , loyalty :: Int
  , power :: Int
  , hitPoints :: Maybe Number
  , traits :: [Trait]
  , text :: Maybe String
  , flavor :: Maybe String
  , keywords :: [Keyword]
  , unique :: Bool
  , actions :: [ActionDef k]
  , receive :: Receive k
  , extras :: Extras k
    -- ^ Per-kind tunables the engine reads instead of casing on a
    -- card's 'code'. Defaults come from 'defaultExtras'; cards
    -- override the slices they care about via the builder helpers
    -- in 'Invasion.Card'.
  , selfCostAdjustment :: Game -> PlayerKey -> Int
    -- ^ Adjustment to this card's printed play cost when played by
    -- @pk@ (e.g. Bloodcrusher: @-1@ per burning zone). Lives on the
    -- top-level 'CardDef' because any kind can be played and any
    -- kind might want a self adjustment in the future. May be
    -- negative; the final cost is clamped non-negative.
  , canPlay :: Game -> PlayerKey -> Bool
    -- ^ Per-card playability check beyond the engine's baseline
    -- (resources, unique, Limited). Used to gate cards whose effect
    -- only makes sense when valid targets exist (e.g. Stubborn
    -- Refusal needs a damaged unit with a peer in its zone).
    -- Default: always 'True'.
  }

-- | Convenience: build the 'CardCodeFilter' describing a card. Used by
-- the engine when invoking 'globalCostAdjustment'.
cardCodeFilter :: CardDef k -> CardCodeFilter
cardCodeFilter c = CardCodeFilter
  { cfCode = c.code
  , cfKind = c.kind
  , cfRaces = c.races
  , cfTraits = c.traits
  }

-- The 'receive' function field can't be 'Show'n, so we derive a manual
-- instance that prints just enough to identify the card in trace logs.
instance Show (CardDef k) where
  showsPrec d c =
    showParen (d > 10) $
      showString "CardDef "
        . shows c.code
        . showString " "
        . shows c.title

-- The 'receive' function field is not JSON-encodable; the frontend only
-- needs the static metadata anyway. Hand-roll the instance so the field
-- is silently dropped. Actions also serialize as just their static
-- metadata (name/cost/target schema) so the client can render the
-- available-actions list without seeing the effect closures.
instance ToJSON (CardDef k) where
  toJSON c =
    object
      [ "code" .= c.code
      , "title" .= c.title
      , "kind" .= c.kind
      , "races" .= c.races
      , "cost" .= c.cost
      , "loyalty" .= c.loyalty
      , "power" .= c.power
      , "hitPoints" .= c.hitPoints
      , "traits" .= c.traits
      , "text" .= c.text
      , "flavor" .= c.flavor
      , "keywords" .= c.keywords
      , "unique" .= c.unique
      , "actions" .= map actionDefMeta c.actions
      ]
    where
      actionDefMeta a =
        object
          [ "name" .= a.actionName
          , "cost" .= a.actionCost
          , "target" .= a.actionTarget
          , "opponentOnly" .= a.actionOpponentOnly
          ]

-- ToJSON because 'CardCodeFilter' rides inside 'History.playedBy'
-- (and 'Game' serializes its history map to the wire). Lives at the
-- bottom of the module so the TH splice doesn't cut earlier
-- declarations off from later ones.
deriveToJSON defaultOptions ''CardCodeFilter
