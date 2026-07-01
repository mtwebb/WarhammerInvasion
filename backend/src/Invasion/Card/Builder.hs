{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Card.Builder (module Invasion.Card.Builder) where

import Control.Monad.State.Strict
import Invasion.Card.Effects
import Invasion.CardDef
import Invasion.CardDef qualified as CardDef
import Invasion.Entity (LegendDetails (..), QuestDetails (..), SupportDetails (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Player
import Invasion.Prelude
import Invasion.Types

newtype CardBuilder k a = CardBuilder (State (CardDef k) a)
  deriving newtype (Functor, Applicative, Monad, MonadState (CardDef k))

emptyCardDef :: forall k. HasDefaultExtras k => CardCode -> String -> CardKind -> CardDef k
emptyCardDef code title kind =
  CardDef
    code
    title
    kind
    []
    (Fixed 0)
    0
    0
    Nothing
    []
    Nothing
    Nothing
    []
    False
    []
    noReceive
    (defaultExtras @k)
    (\_ _ -> 0)
    (\_ _ -> True)

unitCard :: CardCode -> String -> CardBuilder Unit () -> CardDef Unit
unitCard code title = buildCard $ (emptyCardDef code title Unit) {CardDef.hitPoints = Just (Fixed 1)}

supportCard :: CardCode -> String -> CardBuilder Support a -> CardDef Support
supportCard code title = buildCard $ emptyCardDef code title Support

questCard :: CardCode -> String -> CardBuilder Quest a -> CardDef Quest
questCard code title = buildCard $ emptyCardDef code title Quest

tacticCard :: CardCode -> String -> CardBuilder Tactic a -> CardDef Tactic
tacticCard code title = buildCard $ emptyCardDef code title Tactic

-- | Legends are persistent like units (they have hit points and live on
-- the capital board) but they're their own card type and are not
-- targeted by unit/support/tactic effects. The HP default of 1 mirrors
-- 'unitCard'; legend cards will normally override via 'hitPoints'.
legendCard :: CardCode -> String -> CardBuilder Legend () -> CardDef Legend
legendCard code title = buildCard $ (emptyCardDef code title Legend) {CardDef.hitPoints = Just (Fixed 1)}

buildCard :: CardDef k -> CardBuilder k a -> CardDef k
buildCard def (CardBuilder inner) = execState inner def

unique :: CardBuilder k ()
unique = modify \cardDef -> cardDef {unique = True}

race :: Race -> CardBuilder k ()
race r = modify \cardDef -> cardDef {races = r : cardDef.races}

cost :: Int -> CardBuilder k ()
cost c = modify \cardDef -> cardDef {cost = Fixed c}

-- | Variable-cost cards: the printed cost is "X" and the actual cost
-- is determined when the card is played (Smash-Go-Boom!, Flames of
-- Tzeentch, ...). The engine skips the standard pay-X-resources flow
-- for these — per-card resolution is up to the effect body.
costVariable :: CardBuilder k ()
costVariable = modify \cardDef -> cardDef {cost = Variable}

loyalty :: Int -> CardBuilder k ()
loyalty l = modify \cardDef -> cardDef {loyalty = l}

power :: Int -> CardBuilder k ()
power p = modify \cardDef -> cardDef {power = p}

-- | A legend's per-zone power, printed split across kingdom / quest /
-- battlefield. Records all three on 'LegendExtras' and sets the single
-- 'power' field to the weakest zone — the value used "for card-effect
-- purposes" by the rules (see 'LegendExtras').
legendPower :: Int -> Int -> Int -> CardBuilder Legend ()
legendPower k q b = modify \cardDef ->
  cardDef
    { power = minimum [k, q, b]
    , extras = cardDef.extras {kingdomPower = k, questPower = q, battlefieldPower = b}
    }

-- | Cards that carry hit points: units and legends. Other kinds reject
-- 'hitPoints' at the type level so a tactic builder can't silently set
-- an HP value that the engine would never read.
class HasHitPoints (k :: CardKind)
instance HasHitPoints 'Unit
instance HasHitPoints 'Legend

hitPoints :: HasHitPoints k => Int -> CardBuilder k ()
hitPoints hp = modify \cardDef -> cardDef {CardDef.hitPoints = Just (Fixed hp)}

-- | Printed "X" hit points (Cold One Chariot). The base is 0; pair
-- with 'selfHP' to supply the live X value each engine tick.
hitPointsX :: HasHitPoints k => CardBuilder k ()
hitPointsX = modify \cardDef -> cardDef {CardDef.hitPoints = Just Variable}

trait :: Trait -> CardBuilder k ()
trait t = modify \cardDef -> cardDef {traits = t : cardDef.traits}

traits :: [Trait] -> CardBuilder k ()
traits = traverse_ trait

body :: String -> CardBuilder k ()
body f = modify \cardDef -> cardDef {CardDef.text = Just f}

flavor :: String -> CardBuilder k ()
flavor f = modify \cardDef -> cardDef {flavor = Just f}

keyword :: Keyword -> CardBuilder k ()
keyword k = modify \cardDef -> cardDef {keywords = k : cardDef.keywords}

toughness :: Int -> CardBuilder Unit ()
toughness n = keyword (Toughness $ Fixed n)

toughnessX :: CardBuilder Unit ()
toughnessX = keyword (Toughness Variable)

scout :: CardBuilder Unit ()
scout = keyword Scout

-- | "Necromancy (You may play this card from your discard pile. If you
-- do, put it on the bottom of your deck at the end of the turn.)"
necromancy :: CardBuilder Unit ()
necromancy = keyword Necromancy

-- | "Feared X (while this unit is attacking, blank the text box of X
-- target units except for Traits)." The blanking itself is wired per
-- card via an attack-declared trigger; this records the keyword for
-- display and future generic handling.
feared :: Int -> CardBuilder Unit ()
feared n = keyword (Feared n)

-- | Raider X keyword: after combat damage, the attacking player gains
-- X resources for this unit if it survived the combat it attacked in.
-- Stacks across multiple instances. Resolved centrally by the combat
-- pipeline ('FireRaiderResources').
raider :: Int -> CardBuilder Unit ()
raider n = keyword (Raider n)

-- | Counterstrike N keyword: while declared as a defender, this unit
-- immediately deals N uncancellable damage to one attacker of its
-- choice before regular combat damage assigns.
counterstrike :: Int -> CardBuilder Unit ()
counterstrike n = keyword (Counterstrike n)

-- | Savage X keyword (Lizardmen): after this unit survives damage, its
-- controller may deal X damage to a target unit in a corresponding
-- zone. The engine dispatches this generically off 'totalSavage'; the
-- builder only stamps the printed keyword.
savage :: Int -> CardBuilder Unit ()
savage n = keyword (Savage n)

-- | Ambush X keyword (Hidden Kingdoms): the card may be played facedown
-- as a development and flipped faceup for X resources during the combat
-- Ambush step. @ambush 0@ is a free flip.
ambush :: Int -> CardBuilder k ()
ambush n = keyword (Ambush n)

-- | Limited keyword: only one Limited card may be played each turn,
-- regardless of which Limited card it is. Enforced by the engine in
-- 'canPlayCard' via the per-turn 'limitedPlayed' history bucket.
limited :: CardBuilder k ()
limited = keyword Limited

-- | Order Only keyword: cannot be included in a Destruction deck.
orderOnly :: CardBuilder k ()
orderOnly = keyword OrderOnly

-- | Destruction Only keyword: cannot be included in an Order deck.
destructionOnly :: CardBuilder k ()
destructionOnly = keyword DestructionOnly

-- | "Grudge" support keyword: "When your capital is dealt combat damage,
-- you may put this card into play from your hand (in any zone)." The
-- engine offers the support from hand when its owner's capital takes
-- combat damage (see the 'DealDamageToZone' handler).
grudge :: CardBuilder Support ()
grudge = keyword Grudge

-- | "Invasion" quest meta-builder: adds the 'Invasion' trait and the
-- 'PlayInOpponentControl' keyword, so the quest is played from your hand
-- but enters under the opponent's control (controller = zoneOwner =
-- opponent). Its Forced drawbacks then key off the opponent's turn.
invasion :: CardBuilder Quest ()
invasion = trait Invasion >> keyword PlayInOpponentControl

-- | Limit one Hero per zone: while a player controls this Hero in a
-- zone, neither player may put another Hero into that same zone.
limitOneHeroPerZone :: CardBuilder Unit ()
limitOneHeroPerZone = keyword LimitOneHeroPerZone

-- | Hero meta-builder. Sets 'unique', adds the 'Hero' trait, and
-- installs the "Limit one Hero per zone" keyword in one step. Combine
-- with additional traits ('trait Warrior', 'trait Sorcerer', …).
--
-- > durgnarTheBold = unitCard "core-006" "Durgnar the Bold" do
-- >   hero
-- >   trait Warrior
-- >   race Dwarf
-- >   ...
hero :: CardBuilder Unit ()
hero = do
  unique
  trait Hero
  limitOneHeroPerZone

-- | Zone-restriction keywords for cards that may only enter play in a
-- specific zone.
kingdomOnly :: CardBuilder k ()
kingdomOnly = keyword KingdomOnly

questOnly :: CardBuilder k ()
questOnly = keyword QuestOnly

battlefieldOnly :: CardBuilder k ()
battlefieldOnly = keyword BattlefieldOnly

-- | Append a 'Receive' handler. The existing receiver runs first, then
-- the new one; this lets a card stack several event-specific hooks
-- without one stomping the previous one. 'emptyCardDef' starts with
-- 'noReceive', so the first append is effectively a set.
onReceive :: Receive k -> CardBuilder k ()
onReceive (Receive new) = modify \cardDef ->
  let Receive prev = cardDef.receive
   in cardDef
        { receive = Receive \msg owner self -> do
            prev msg owner self
            new msg owner self
        }

-- | Compose two 'Receive' handlers sequentially. The first runs, then
-- the second.
composeReceive :: Receive k -> Receive k -> Receive k
composeReceive (Receive a) (Receive b) = Receive \msg owner self -> do
  a msg owner self
  b msg owner self

-- | Wrap a 'Receive' handler so it only fires while the host is in
-- the given zone.
gateReceive
  :: HasField "zone" (InPlay k) ZoneKind
  => ZoneKind -> Receive k -> Receive k
gateReceive z (Receive r) = Receive \msg owner self ->
  when (self.zone == z) (r msg owner self)

-- ---------------------------------------------------------------------
-- Extras builders
--
-- The engine reads kind-specific 'extras' fields instead of casing on a
-- card's 'code'. Each helper below sets a single slice; absence keeps
-- the corresponding default from 'defaultExtras' (no-op for that
-- behavior).
-- ---------------------------------------------------------------------

modifyUnitExtras :: (UnitExtras -> UnitExtras) -> CardBuilder Unit ()
modifyUnitExtras f =
  modify \cd -> cd {extras = f cd.extras}

modifySupportExtras :: (SupportExtras -> SupportExtras) -> CardBuilder Support ()
modifySupportExtras f =
  modify \cd -> cd {extras = f cd.extras}

modifyQuestExtras :: (QuestExtras -> QuestExtras) -> CardBuilder Quest ()
modifyQuestExtras f =
  modify \cd -> cd {extras = f cd.extras}

modifyLegendExtras :: (LegendExtras -> LegendExtras) -> CardBuilder Legend ()
modifyLegendExtras f =
  modify \cd -> cd {extras = f cd.extras}

-- | Continuous power a legend grants units (Grombrindal, Gorbad
-- Ironclaw). Folded into each unit's effective power like the other
-- auras. Args: game, the legend, the candidate unit.
legendUnitAura
  :: (Game -> LegendDetails -> UnitDetails -> Int) -> CardBuilder Legend ()
legendUnitAura f = modifyLegendExtras \e -> e {legendUnitAuraPower = f}

-- | Cost adjustment a legend applies to cards its controller plays
-- (Balthasar Gelt). Mirrors the unit-side cost-adjustment slice.
legendCostAdjust
  :: (Game -> LegendDetails -> PlayerKey -> CardCodeFilter -> Int)
  -> CardBuilder Legend ()
legendCostAdjust f = modifyLegendExtras \e -> e {legendCostAdjustment = f}

-- | A legend grants untargetability-by-opponents to units matching the
-- predicate (Azhag → damaged units you control).
legendUntargetableAura
  :: (Game -> LegendDetails -> UnitDetails -> Bool) -> CardBuilder Legend ()
legendUntargetableAura f = modifyLegendExtras \e -> e {legendGrantsUntargetable = f}

-- | "Cancel 1 damage to your capital each turn" (Contested Fortress).
-- Evaluated live by the engine's 'DealDamageToZone' pipeline, once per
-- turn per copy, on either player's turn.
capitalShieldEachTurn :: CardBuilder Support ()
capitalShieldEachTurn =
  modifySupportExtras \e -> e {capitalShieldPerTurn = True}

-- | "Redirect the first point of damage done to your capital each turn
-- to another target unit or capital" while the supplied predicate
-- holds (Defend the Border with 3+ resource tokens). Evaluated live at
-- damage time; once per turn per copy.
redirectsFirstCapitalDamage
  :: (Game -> QuestDetails -> Bool) -> CardBuilder Quest ()
redirectsFirstCapitalDamage f =
  modifyQuestExtras \e -> e {capitalRedirectFirstDamage = f}

-- | Game-state-derived self power bonus (Troll Slayers, Korhil, …).
selfPower :: (Game -> UnitDetails -> Int) -> CardBuilder Unit ()
selfPower f = modifyUnitExtras \e -> e {selfPowerBonus = f}

-- | Extra combat damage this unit deals (Lord of Khorne, Gorbad).
combatPower :: (Game -> UnitDetails -> Int) -> CardBuilder Unit ()
combatPower f = modifyUnitExtras \e -> e {combatPowerBonus = f}

-- | Power this unit grants to other in-play units (Karl Franz, Templar
-- of Sigmar). Args to @f@: game, this unit, target unit.
unitAura :: (Game -> UnitDetails -> UnitDetails -> Int) -> CardBuilder Unit ()
unitAura f = modifyUnitExtras \e -> e {unitAuraPower = f}

-- | Aura toughness granted to other units. Source unit is 'self';
-- target unit is the third arg. Used by Big 'Uns ("Your damaged
-- units gain Toughness 1").
toughnessAura :: (Game -> UnitDetails -> UnitDetails -> Int) -> CardBuilder Unit ()
toughnessAura f = modifyUnitExtras \e -> e {unitAuraToughness = f}

-- | Counterstrike this unit grants other units (Luthor Huss: +1 to units
-- in his zone). Source unit is 'self'; the target is the third argument.
counterstrikeAura :: (Game -> UnitDetails -> UnitDetails -> Int) -> CardBuilder Unit ()
counterstrikeAura f = modifyUnitExtras \e -> e {unitAuraCounterstrike = f}

-- | Aura hit points granted to other units. Source unit is 'self';
-- target unit is the third arg. The unit-side mirror of the support
-- 'supportAuraHP' slot; used by Mountain Sentry ("Ranger units in
-- this zone get +2 hit points").
hpAura :: (Game -> UnitDetails -> UnitDetails -> Int) -> CardBuilder Unit ()
hpAura f = modifyUnitExtras \e -> e {unitAuraHp = f}

-- | Install a pre-damage redirect plan. The slice receives the
-- in-flight damage and decides whether (and how much) to claim;
-- when it returns 'Just', the engine pulls that many points off
-- the original target and runs the supplied body, which is
-- expected to enqueue the redirected damage and mark its own
-- per-turn cooldown if applicable. Used by Warrior Priests and
-- Defend the Border.
preDamageRedirectHook
  :: (Game -> UnitDetails -> Int -> Maybe PreDamageRedirect)
  -> CardBuilder Unit ()
preDamageRedirectHook f =
  modifyUnitExtras \e -> e {preDamageRedirect = f}

-- | Predicate gating attacker eligibility (Sworn of Khorne).
canAttack :: (Game -> PlayerKey -> ZoneKind -> UnitDetails -> Bool) -> CardBuilder Unit ()
canAttack f = modifyUnitExtras \e -> e {canAttackZone = f}

-- | Predicate gating defender eligibility (Daemon Prince). Symmetric to
-- 'canAttack'.
canDefend :: (Game -> PlayerKey -> ZoneKind -> UnitDetails -> Bool) -> CardBuilder Unit ()
canDefend f = modifyUnitExtras \e -> e {canDefendZone = f}

-- | "This unit cannot be restored." (White Lion Champion.) Stays
-- corrupted once corrupted — excluded from the restore prompt.
cannotBeRestored :: CardBuilder Unit ()
cannotBeRestored = modifyUnitExtras \e -> e {cannotBeRestored = True}

-- | "Action: When one of your zones [matching @pred@] is attacked, put
-- this unit into play in that zone from your hand, declared as a
-- defender." (Bladesinger.) The engine offers it at 'BeginCombat' and
-- compels it with 'MustDefend'.
defenderFromHandWhen
  :: (Game -> PlayerKey -> ZoneKind -> Bool) -> CardBuilder Unit ()
defenderFromHandWhen f = modifyUnitExtras \e -> e {defenderFromHandWhen = Just f}

-- | Per-turn damage cap (Daemonettes of Slaanesh).
perTurnDamageCap :: Int -> CardBuilder Unit ()
perTurnDamageCap n = modifyUnitExtras \e -> e {damageCap = Just n}

-- | "This unit can attack or defend (from any zone) whenever a [Race]
-- legend you control attacks or defends." (Da Immortulz, Swords of
-- Chaos, Black Guards.)
bodyguardForLegend :: Race -> CardBuilder Unit ()
bodyguardForLegend r = modifyUnitExtras \e -> e {bodyguardLegendRace = Just r}

-- | Mark the unit as corrupting any enemy it deals non-zero combat
-- damage to (Plaguebearers of Nurgle, Beasts of Nurgle).
corruptsOnDamage :: CardBuilder Unit ()
corruptsOnDamage = modifyUnitExtras \e -> e {corruptsOnCombatDamage = True}

-- | Game-state-derived bonus to the unit's own max HP (Cold One
-- Chariot's X). Folded into the cached @effectiveMaxHP@.
selfHP :: (Game -> UnitDetails -> Int) -> CardBuilder Unit ()
selfHP f = modifyUnitExtras \e -> e {selfHPBonus = f}

-- | Game-state-derived bonus to the unit's own Toughness (Ludwig
-- Schwarzheim: X = experiences attached). Folded into 'totalToughness'.
-- Distinct from the 'toughnessX' keyword, which the engine reads as
-- developments-in-zone.
selfToughness :: (Game -> UnitDetails -> Int) -> CardBuilder Unit ()
selfToughness f = modifyUnitExtras \e -> e {selfToughnessBonus = f}

-- | "Counterstrike X" — a counterstrike value derived from board state
-- (Anlec Lookout, Herald of Morai-Heg, Wardancer). Added to any printed
-- 'Counterstrike' keyword when the unit fires Counterstrike in combat.
counterstrikeX :: (Game -> UnitDetails -> Int) -> CardBuilder Unit ()
counterstrikeX f = modifyUnitExtras \e -> e {selfCounterstrikeBonus = f}

-- | "Cancel all damage to this unit while CONDITION." (Gustav the
-- Bear.) Only cancellable damage is affected.
damageImmuneWhen :: (Game -> UnitDetails -> Bool) -> CardBuilder Unit ()
damageImmuneWhen f = modifyUnitExtras \e -> e {cancelAllDamageWhen = f}

-- | "Whenever this unit is assigned damage, cancel all but N of that
-- damage." (Dragonmage: 1.)
perHitCap :: Int -> CardBuilder Unit ()
perHitCap n = modifyUnitExtras \e -> e {perHitDamageCap = Just n}

-- | Printed "This unit cannot defend." (Clan Moulder's Elite.)
neverDefends :: CardBuilder Unit ()
neverDefends = modifyUnitExtras \e -> e {cannotDefend = True}

-- | Zones this unit may attack from (default: battlefield only).
-- Greyseer Thanquol passes all three; Dragonslayer adds the quest
-- zone.
attacksFromZones :: [ZoneKind] -> CardBuilder Unit ()
attacksFromZones zs = modifyUnitExtras \e -> e {attackEligibleZones = zs}

-- | Destruction replacement: re-enter play in the named zone instead
-- of hitting the discard pile (Vigilant Pistoliers).
onDestroyedRelocate :: (Game -> UnitDetails -> Maybe ZoneKind) -> CardBuilder Unit ()
onDestroyedRelocate f = modifyUnitExtras \e -> e {destroyedToZone = f}

-- | "When this unit defends, it deals its combat damage to all
-- attacking units." (Juvenile Wyvern.)
defenderHitsAllAttackers :: CardBuilder Unit ()
defenderHitsAllAttackers =
  modifyUnitExtras \e -> e {defenderDamageToAllAttackers = True}

-- | Extra resources a non-controller must spend to target this unit
-- (King Kazador).
targetTax :: (Game -> PlayerKey -> UnitDetails -> Int) -> CardBuilder Unit ()
targetTax f = modifyUnitExtras \e -> e {extraTargetTax = f}

-- | Multiplier on ALL applied damage while this unit is in play
-- (Bloodletter).
damageMultiplier :: Int -> CardBuilder Unit ()
damageMultiplier n = modifyUnitExtras \e -> e {damageMultiplierWhileInPlay = n}

-- | Self-cost adjustment when playing this card (Bloodcrusher: -1
-- per burning zone). May be negative.
selfCostAdjust :: (Game -> PlayerKey -> Int) -> CardBuilder k ()
selfCostAdjust f = modify \cd -> cd {selfCostAdjustment = f}

-- | Per-card playability predicate. The engine refuses to play this
-- card if the predicate returns 'False'. Stacks with the engine's
-- baseline checks (resources, unique, Limited).
--
-- > playableWhen \g _pk ->
-- >   any (\u -> ... source candidate ...) g.units
playableWhen :: (Game -> PlayerKey -> Bool) -> CardBuilder k ()
playableWhen pred = modify \cd -> cd {canPlay = pred}

-- | Static power contribution while attached (Daemonsword, Banner of
-- Sigmar, …).
attachmentPower :: Int -> CardBuilder Support ()
attachmentPower n = modifySupportExtras \e -> e {attachmentPowerBonus = n}

-- | Static HP contribution while attached (Daemonsword).
attachmentHp :: Int -> CardBuilder Support ()
attachmentHp n = modifySupportExtras \e -> e {attachmentHPBonus = n}

-- | "Attached legend can defend any of your zones." (Descendant of Gods.)
legendDefendsAnyZone :: CardBuilder Support ()
legendDefendsAnyZone = modifySupportExtras \e -> e {grantsLegendDefendAnyZone = True}

-- | Combat power this attachment grants its legend host (read only while
-- the legend is in combat). Mirrors the unit-side 'supportCombatBonus'
-- for legend hosts (Dawnstar Sword +5; Morglor's +2/+4 via the function
-- form). Constant convenience wrapper; use 'legendCombatBonusWith' for
-- state-dependent values.
legendCombatBonus :: Int -> CardBuilder Support ()
legendCombatBonus n = legendCombatBonusWith \_ _ -> n

legendCombatBonusWith
  :: (Game -> SupportDetails -> Int) -> CardBuilder Support ()
legendCombatBonusWith f =
  modifySupportExtras \e -> e {attachmentLegendCombatBonus = f}

-- | Grant Savage X to the host while attached (Cloak of Feathers).
attachmentSavage :: Int -> CardBuilder Support ()
attachmentSavage n = modifySupportExtras \e -> e {attachmentSavageBonus = n}

-- | "Attached unit gains Counterstrike X." (Duelist Training, Blessed
-- Hammer.) Summed into the host's 'totalCounterstrike'.
attachmentCounterstrike :: Int -> CardBuilder Support ()
attachmentCounterstrike n = modifySupportExtras \e -> e {attachmentCounterstrikeBonus = n}

-- | "Attached unit gains Toughness X." (Clockwork Horse.) Summed into
-- the host's 'totalToughness'.
attachmentToughness :: Int -> CardBuilder Support ()
attachmentToughness n = modifySupportExtras \e -> e {attachmentToughnessBonus = n}

-- | "Attached unit gains Raider X." (Plunderer.) Summed into the
-- attacker's Raider total when combat damage resolves.
attachmentRaider :: Int -> CardBuilder Support ()
attachmentRaider n = modifySupportExtras \e -> e {attachmentRaiderBonus = n}

-- | While attached, the host unit's combat damage is uncancellable
-- (Hammer of Sigmar).
grantsUncancellable :: CardBuilder Support ()
grantsUncancellable =
  modifySupportExtras \e -> e {grantsUncancellableDamage = True}

-- | Power this support grants to a unit (Iron Tower, Cauldron of
-- Blood, Da Bad Moon static slice).
supportAura :: (Game -> SupportDetails -> UnitDetails -> Int) -> CardBuilder Support ()
supportAura f = modifySupportExtras \e -> e {supportAuraPower = f}

-- | Extra combat damage this support adds to a unit (Rift of Battle,
-- Organ Gun while defending, Da Bad Moon and Big Boss's Banner for Orc
-- attackers).
supportCombat :: (Game -> SupportDetails -> UnitDetails -> Int) -> CardBuilder Support ()
supportCombat f = modifySupportExtras \e -> e {supportCombatBonus = f}

-- | "Attach to a target unit. Attached unit gains …" Runs the body
-- each engine recompute with the support and the unit it's attached
-- to; 'gainPower unit n' (and future buff verbs) emit contributions
-- that flow into the attached unit's effective stats. Body fires
-- only while attached.
--
-- > organGun = supportCard "core-015" "Organ Gun" do
-- >   ...
-- >   attachedTo \_self unit ->
-- >     when unit.defending $ gainPower unit 2
attachedTo
  :: (SupportDetails -> UnitDetails -> EffectM ())
  -> CardBuilder Support ()
attachedTo body = modifySupportExtras \e -> e
  { supportAuraPower = \g s u ->
      let prev = e.supportAuraPower g s u
          extra = case s.attachedTo of
            Just k | k == u.key ->
              activeBonusPower (execEffectM g (body s u))
            _ -> 0
       in prev + extra
  }

-- | Bonus power this support contributes to a zone of its controller
-- (Lighthouse of Lothern, Rift of Chaos).
zonePowerAura :: (Game -> SupportDetails -> ZoneKind -> Int) -> CardBuilder Support ()
zonePowerAura f = modifySupportExtras \e -> e {zonePowerBonus = f}

-- | "These cards also count as developments." Contributes extra
-- developments to a zone of the support's controller, raising that
-- zone's burn threshold (The Oak of Ages, Higher Learning). Args:
-- game, this support, the zone being queried.
countsAsDevelopments
  :: (Game -> SupportDetails -> ZoneKind -> Int) -> CardBuilder Support ()
countsAsDevelopments f = modifySupportExtras \e -> e {developmentBonusInZone = f}

-- | Cost-of-play adjustment this support imposes on other cards being
-- played (Imperial Crown, Master Rune of Dismay).
globalCostAdjust
  :: (Game -> SupportDetails -> PlayerKey -> CardCodeFilter -> Int)
  -> CardBuilder Support ()
globalCostAdjust f = modifySupportExtras \e -> e {globalCostAdjustment = f}

-- | "Lower the cost of the first card matching @match@ that the support's
-- controller plays each turn by @amount@." The reduction applies only
-- while the controller has not yet played a matching card this turn
-- (tracked via the per-turn 'cardsPlayedThisTurn' history), so it hits
-- exactly the first one. The "first … you play each turn" cost cycle
-- (Sun Temple of Chotec, Master Moulder).
reducesFirstPerTurn
  :: (CardCodeFilter -> Bool)
  -> (Game -> PlayerKey -> Int)
  -> CardBuilder Support ()
reducesFirstPerTurn match amount = modifySupportExtras \e ->
  e
    { globalCostAdjustment = \g s pk filt ->
        if pk == s.controller
          && match filt
          && not (any match (cardsPlayedThisTurn g pk))
          then negate (amount g pk)
          else 0
    }

-- | Cost-of-play adjustment this in-play unit imposes on other cards
-- being played (Nuln Tinkerers: -1 on the controller's first support
-- of the turn). Mirrors 'globalCostAdjust' on the support side.
unitCostAdjust
  :: (Game -> UnitDetails -> PlayerKey -> CardCodeFilter -> Int)
  -> CardBuilder Unit ()
unitCostAdjust f = modifyUnitExtras \e -> e {unitCostAdjustment = f}

-- | Support-side target tax that fires when an effect targets a
-- specific unit (Church of Sigmar: opponents pay +1 to target
-- the controller's units). Args: game, this support, the player
-- firing the effect, the targeted unit.
supportTax
  :: (Game -> SupportDetails -> PlayerKey -> UnitDetails -> Int)
  -> CardBuilder Support ()
supportTax f = modifySupportExtras \e -> e {supportTargetTax = f}

-- | Per-tick HP adjustment this support grants another unit. Used
-- by Horrific Mutation (defenders lose 1 HP while host attacks).
supportHPAura
  :: (Game -> SupportDetails -> UnitDetails -> Int)
  -> CardBuilder Support ()
supportHPAura f = modifySupportExtras \e -> e {supportAuraHP = f}

-- | "Units in a zone with no developments lose all power." (Hidden
-- Grove.) The Omens of Ruin "empty zone" building cycle.
unitsLoseAllPowerInEmptyZones :: CardBuilder Support ()
unitsLoseAllPowerInEmptyZones =
  modifySupportExtras \e -> e {imposesNoPowerOn = \g _s u -> devsInZone g u == 0}

-- | "Units in a zone with no developments cannot defend." (Boar Pen.)
unitsCannotDefendInEmptyZones :: CardBuilder Support ()
unitsCannotDefendInEmptyZones =
  modifySupportExtras \e -> e {imposesCannotDefendOn = \g _s u -> devsInZone g u == 0}

-- | "Units in a zone with no developments lose all triggered abilities."
-- (Eatine Harbour.) Modelled as full text-box blanking, which also
-- suppresses keywords — a slight over-reach noted on the card.
unitsBlankedInEmptyZones :: CardBuilder Support ()
unitsBlankedInEmptyZones =
  modifySupportExtras \e -> e {imposesBlankOn = \g _s u -> devsInZone g u == 0}

-- | "Units in a zone with no developments get N hit points." (Malekith's
-- Throne: -1.)
unitsHPInEmptyZones :: Int -> CardBuilder Support ()
unitsHPInEmptyZones n =
  modifySupportExtras \e -> e {supportAuraHP = \g _s u -> if devsInZone g u == 0 then n else 0}

-- | Mark the printed Rune of Fortitude effect on this support.
imposesRuneOfFortitudeTax :: CardBuilder Support ()
imposesRuneOfFortitudeTax =
  modifySupportExtras \e -> e {runeOfFortitudeTax = True}

-- | Toughness this support grants a unit (Gromril Armour grants the
-- attached unit Toughness 1). Args: game, this support, target unit.
supportToughnessAura
  :: (Game -> SupportDetails -> UnitDetails -> Int)
  -> CardBuilder Support ()
supportToughnessAura f =
  modifySupportExtras \e -> e {supportAuraToughness = f}

-- | Savage X this support grants a unit (Ziggurat of Quetli grants
-- Lizardmen in a Pyramid zone Savage 1). Args: game, this support,
-- target unit. Folded into 'totalSavage'.
supportSavageAura
  :: (Game -> SupportDetails -> UnitDetails -> Int)
  -> CardBuilder Support ()
supportSavageAura f =
  modifySupportExtras \e -> e {supportAuraSavage = f}

-- | "Whenever you search your deck, you may search an additional
-- card." (Scout Camp.) Args: game, this support, the searching
-- player.
searchBonus
  :: (Game -> SupportDetails -> PlayerKey -> Int)
  -> CardBuilder Support ()
searchBonus f = modifySupportExtras \e -> e {searchDepthBonus = f}

-- | "Whenever a tactic you play deals damage to one or more targets,
-- deal an additional damage to each target." (Hellcannon Reserves.)
tacticDamageBoost
  :: (Game -> SupportDetails -> PlayerKey -> Int)
  -> CardBuilder Support ()
tacticDamageBoost f = modifySupportExtras \e -> e {tacticDamageBonus = f}

-- | "Double all damage dealt to the defending opponent's capital"
-- while the condition holds (Basha's Bloodaxe). Args: game, this
-- support, the player whose capital is taking damage.
doublesCapitalDamage
  :: (Game -> SupportDetails -> PlayerKey -> Bool)
  -> CardBuilder Support ()
doublesCapitalDamage f =
  modifySupportExtras \e -> e {capitalDamageDoubler = f}

-- | "Treat attached unit as though its printed text box were blank
-- (except for Traits)." (Witch Hag's Curse.)
blanksAttachedUnit :: CardBuilder Support ()
blanksAttachedUnit = modifySupportExtras \e -> e {blanksHost = True}

-- | "Attached unit cannot attack." (Word of Pain.)
preventsHostAttack :: CardBuilder Support ()
preventsHostAttack = modifySupportExtras \e -> e {hostCannotAttack = True}

-- | "This card cannot be targeted by card effects." Unconditional
-- self-immunity for the artefact attachments (Dawnstar Sword, Eye of
-- Sheerian, Windcatcher Prism, …). Blocks every player.
cannotBeTargetedSelf :: CardBuilder Support ()
cannotBeTargetedSelf =
  modifySupportExtras \e -> e {selfUntargetable = \_ _ -> Just False}

-- | Conditional self-immunity. Returns @Just opponentOnly@ while the
-- predicate holds (Helm of Fortune: blocks opponents while the host is
-- questing). Args: game, this support.
cannotBeTargetedSelfWhen
  :: (Game -> InPlay Support -> Maybe Bool)
  -> CardBuilder Support ()
cannotBeTargetedSelfWhen f =
  modifySupportExtras \e -> e {selfUntargetable = f}

-- | "Attached unit cannot be targeted by ... card effects while
-- CONDITION." (Helm of Fortune: by opponents while questing.) The
-- predicate returns @Just opponentOnly@ for the support's own host
-- while the immunity holds; the host-side mirror of
-- 'cannotBeTargetedSelfWhen'.
grantsHostUntargetableWhen
  :: (Game -> InPlay Support -> InPlay Unit -> Maybe Bool)
  -> CardBuilder Support ()
grantsHostUntargetableWhen f =
  modifySupportExtras \e -> e {grantsHostUntargetable = f}

-- | "Cancel all damage assigned to the attached unit while CONDITION."
-- (Shield of Aeons: while its host is participating in combat.) Args:
-- game, this support, the candidate unit — return True only for the
-- support's own host while the condition holds.
grantsHostDamageImmunityWhen
  :: (Game -> InPlay Support -> InPlay Unit -> Bool)
  -> CardBuilder Support ()
grantsHostDamageImmunityWhen f =
  modifySupportExtras \e -> e {grantsHostDamageImmunity = f}

-- | "If attached unit would be destroyed, you may pay N resources to
-- leave it in play and remove all damage from it." (Hydra Blade.)
hostDestroyRansomOf :: Int -> CardBuilder Support ()
hostDestroyRansomOf n =
  modifySupportExtras \e -> e {hostDestroyRansom = Just n}

-- | Mark a synthetic attachment as physically being the given unit
-- card; the discard pile receives the unit def when the attachment
-- leaves play (Vigilant Elector).
revertsToUnit :: CardDef Unit -> CardBuilder Support ()
revertsToUnit cd = modifySupportExtras \e -> e {revertToUnit = Just cd}

-- | "The unit questing on this card adds its power to your kingdom zone
-- as well." (New Trade Route.)
questerAddsPowerToKingdom :: CardBuilder Quest ()
questerAddsPowerToKingdom =
  modifyQuestExtras \e -> e {questerAddsPowerToKingdom = True}

-- | "Any unit questing on this card can defend any of your zones."
-- (Protect the Empire.)
questerDefendsAnywhere :: CardBuilder Quest ()
questerDefendsAnywhere =
  modifyQuestExtras \e -> e {questerDefendsAnyZone = True}

-- | "While a Lizardmen unit is questing on this card, double all damage
-- assigned by the effects of Savage." (Guardians of the Gods.)
doublesSavageDamageQuest :: CardBuilder Quest ()
doublesSavageDamageQuest =
  modifyQuestExtras \e -> e {doublesSavageDamage = True}

-- | "You may spend resources from this card to pay for Attachment
-- cards." (Dat's Mine!.)
paysAttachmentCosts :: CardBuilder Quest ()
paysAttachmentCosts = modifyQuestExtras \e -> e {paysForAttachments = True}

-- | "Any unit questing on this card may attack as though it were in
-- your battlefield." (Sack Tor Aendris.)
questerAttacksAnywhere :: CardBuilder Quest ()
questerAttacksAnywhere =
  modifyQuestExtras \e -> e {questerAttacksAnyZone = True}

-- | Continuous power this quest grants the controller's units (Night
-- Raids while it holds 3+ resource tokens). Args: game, this quest,
-- target unit. The quest-side mirror of 'unitAura' / 'supportAura'.
questUnitAura
  :: (Game -> InPlay Quest -> InPlay Unit -> Int) -> CardBuilder Quest ()
questUnitAura f = modifyQuestExtras \e -> e {questUnitAuraPower = f}

-- | Non-gated constant block. The body fires every engine tick
-- regardless of which zone this unit is in. Use for "this unit gains
-- X while CONDITION" effects whose condition isn't zone-specific
-- (Durgnar the Bold, …).
--
-- > effects \self owner ->
-- >   when (capitalBurning owner) $ gainPower self 2
effects
  :: (UnitDetails -> Player -> EffectM ())
  -> CardBuilder Unit ()
effects body = modifyUnitExtras \e -> e
  { runtimeEffects = \g self ->
      let prev = e.runtimeEffects g self
          owner = playerOf self.controller g
       in prev <> execEffectM g (body self owner)
  }
