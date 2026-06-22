{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Empire core cards (core-026..050). Every printed ability is
-- implemented; one card takes a simplifying shortcut (Bright
-- Wizard Apprentice cancels the next opponent action rather than
-- the previous one) — the in-card comment names the compromise.
module Invasion.Card.Defs.Empire (module Invasion.Card.Defs.Empire) where

import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Data.Map.Strict qualified as Map
import Invasion.Capital
import Invasion.Entity (QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

peasantMilitia :: CardDef Unit
peasantMilitia = unitCard "core-026" "Peasant Militia" do
  race Empire
  cost 0
  loyalty 1
  power 0
  hitPoints 3
  trait Warrior

reiksguardKnights :: CardDef Unit
reiksguardKnights = unitCard "core-027" "Reiksguard Knights" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  trait Knight
  counterstrike 2
  body "Counterstrike 2 (this unit deals 2 combat damage immediately after defending)."

brightWizardApprentice :: CardDef Unit
brightWizardApprentice = unitCard "core-028" "Bright Wizard Apprentice" do
  race Empire
  cost 2
  loyalty 2
  power 1
  hitPoints 1
  trait Mage
  body "Quest. Action: Spend 2 resources to target the effects of an Action just triggered by another unit or support card. Cancel the effects of that Action (limit once per turn)."
  -- Approximation: arm a one-shot cancel against this player's
  -- opponent. The next 'TriggerCardAction' that opponent fires has
  -- its effect suppressed (cost is still paid). The "target an
  -- action just triggered" rewind isn't expressible without a real
  -- action stack; this forward-looking cancel is the closest
  -- working substitute. Once-per-turn enforced via
  -- 'ActionUsedThisTurn' marker.
  quest $ action "Counterspell" 2 \usage -> do
    g <- getGame
    let used =
          any (\m -> m.details == ActionUsedThisTurn)
            (Map.findWithDefault [] (UnitRef usage.self.key) g.modifiers)
    unless used do
      until EndOfTurn (PendingBuff usage.self.key ActionUsedThisTurn)
      push (ArmActionCancel usage.user.next)

nulnTinkerers :: CardDef Unit
nulnTinkerers = unitCard "core-029" "Nuln Tinkerers" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Engineer
  body "Kingdom. Lower the cost of the first support card you play each turn by 1."
  unitCostAdjust \g self pk filt ->
    let played =
          getOrZero pk (historyOfThisTurn g).supportsPlayedBy
        gateZone = self.zone == KingdomZone
     in if pk == self.controller
           && gateZone
           && filt.cfKind == Support
           && played == 0
          then -1
          else 0

-- | Lookup helper: scope's per-player counter, default 0.
getOrZero :: PlayerKey -> Map.Map PlayerKey Int -> Int
getOrZero pk = Map.findWithDefault 0 pk

historyOfThisTurn :: Game -> History
historyOfThisTurn g = case Map.lookup ThisTurn g.history of
  Just h -> h
  Nothing -> mempty

theGreatswords :: CardDef Unit
theGreatswords = unitCard "core-030" "The Greatswords" do
  race Empire
  cost 4
  loyalty 2
  power 1
  hitPoints 4
  traits [Warrior, Elite]
  body "Forced: After a unit enters this zone, The Greatswords gains {power} until the end of the turn."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay _pk uk
      | uk /= self.key -> do
          g <- getGame
          case findUnit uk g of
            Just u | u.zone == self.zone ->
              until EndOfTurn $ buffPower self.key 1
            _ -> pure ()
    _ -> pure ()

sigmarsBlessed :: CardDef Unit
sigmarsBlessed = unitCard "core-031" "Sigmar's Blessed" do
  race Empire
  cost 2
  loyalty 2
  power 1
  hitPoints 1
  trait Priest
  body "Forced: After this unit leaves play, return one target unit in any player's corresponding zone to its owner's hand."
  onSelfLeavesPlay \_owner self -> do
    g <- getGame
    let candidates = filter (\u -> u.zone == self.zone) g.units
    case candidates of
      [] -> pure ()
      _ ->
        withTarget self.controller AnyUnit \k ->
          withUnit k \u ->
            when (u.zone == self.zone) (returnUnitToHand k)

thyrusGorman :: CardDef Unit
thyrusGorman = unitCard "core-032" "Thyrus Gorman" do
  hero
  trait Mage
  race Empire
  cost 3
  loyalty 3
  power 3
  hitPoints 3
  body "Limit one Hero per zone. Forced: After your turn ends, this unit takes 1 damage."
  onMyTurnEnd \_owner self -> dealDamage self.key 1

huntsmen :: CardDef Unit
huntsmen = unitCard "core-033" "Huntsmen" do
  race Empire
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Ranger
  body "Quest zone only."
  questOnly

warriorPriests :: CardDef Unit
warriorPriests = unitCard "core-034" "Warrior Priests" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  traits [Warrior, Priest]
  body "Forced: The first point of damage assigned to this unit each turn is redirected to one target unit in any battlefield. (If there is no valid target, the damage is assigned to Warrior Priests.)"
  -- Faithful: the engine consults this pre-damage hook BEFORE
  -- applying damage. We claim 1 of the inbound on the first hit
  -- each turn provided a valid battlefield target exists, then
  -- prompt the controller for redirect target.
  preDamageRedirectHook \g self inbound ->
    let used =
          any (\m -> m.details == RedirectedThisTurn)
            (Map.findWithDefault [] (UnitRef self.key) g.modifiers)
        anyTarget =
          any (\u -> u.zone == BattlefieldZone && u.key /= self.key) g.units
     in if not used && anyTarget && inbound > 0
          then Just PreDamageRedirect
            { amount = 1
            , run = ActionEffect \usage -> do
                until EndOfTurn (PendingBuff usage.self.key RedirectedThisTurn)
                withTarget usage.user
                  (UnitMatching \_ _ u ->
                    u.zone == BattlefieldZone && u.key /= usage.self.key)
                  \k -> dealDamage k 1
            }
          else Nothing

freeCompany :: CardDef Unit
freeCompany = unitCard "core-035" "Free Company" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Warrior
  scout
  body "Scout (discard one card at random from an opponent's hand if this unit survives combat)."

pistoliers :: CardDef Unit
pistoliers = unitCard "core-036" "Pistoliers" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Ranger
  body "Action: Spend 1 resource to move this unit from its current zone to another of your zones."
  action "Reposition" 1 \usage ->
    withTarget usage.user MyAnyZone \zk ->
      moveUnit usage.self.key zk

johannesBroheim :: CardDef Unit
johannesBroheim = unitCard "core-037" "Johannes Broheim" do
  hero
  trait Cavalry
  race Empire
  cost 6
  loyalty 3
  power 1
  hitPoints 4
  counterstrike 2
  body "Limit one Hero per zone. Counterstrike 2. At the end of each phase, you may move this unit from its current zone to one of your other zones."
  onReceive $ Receive \msg _owner self -> case msg of
    EndPhase _ -> do
      g <- getGame
      when (g.currentPlayer == self.controller) do
        move <- askYesNo self.controller "Move Johannes Broheim to one of your other zones?"
        when move $
          withTarget self.controller MyAnyZone \zk ->
            moveUnit self.key zk
    _ -> pure ()

knightTraining :: CardDef Support
knightTraining = supportCard "core-038" "Knight Training" do
  race Empire
  cost 0
  loyalty 2
  traits [Attachment, Skill]
  body "Attach to a target unit in your battlefield. Attached unit gains {power}."
  attachedTo \_self unit -> gainPower unit 1
  -- NOTE: the engine doesn't yet restrict attachment targets by
  -- zone. Until it does, this can attach to any unit. The +1 power
  -- buff is correct once it's attached.

churchOfSigmar :: CardDef Support
churchOfSigmar = supportCard "core-039" "Church of Sigmar" do
  race Empire
  cost 2
  loyalty 2
  power 1
  trait Building
  body "Kingdom. Opponents cannot target your units with card effects unless they pay an additional 1 resource per effect."
  supportTax \_g self caster target ->
    if self.zone == KingdomZone
        && target.controller == self.controller
        && caster /= self.controller
      then 1
      else 0

cityGates :: CardDef Support
cityGates = supportCard "core-040" "City Gates" do
  race Empire
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Forced: After your turn begins, place the top card of your deck into this zone as a development."
  forced \self ->
    onTurnBegin self.controller $
      addDevelopment self.controller self.zone

shrineToTaal :: CardDef Support
shrineToTaal = supportCard "core-041" "Shrine to Taal" do
  race Empire
  cost 2
  loyalty 1
  trait Building
  body "Forced: After your turn begins, choose one target unit in this zone. That unit gains {power} for each of your developments in this zone until the end of the turn."
  onMyTurnBegin \owner self -> do
    let pk = self.controller
        z = self.zone
        Developments devs =
          case z of
            KingdomZone -> owner.capital.kingdom.developments
            QuestZone -> owner.capital.quest.developments
            BattlefieldZone -> owner.capital.battlefield.developments
    when (devs > 0) $
      withTarget pk
        (UnitMatching \_ _ u -> u.controller == pk && u.zone == z)
        \k -> until EndOfTurn $ buffPower k devs

templeOfShallya :: CardDef Support
templeOfShallya = supportCard "core-042" "Temple of Shallya" do
  race Empire
  cost 4
  loyalty 1
  power 2
  trait Building
  body "Kingdom. At the beginning of your kingdom phase, you may move one of your units from its current zone to one of your other zones."
  -- Printed timing: once, at the controller's kingdom phase begin,
  -- optional, and only while this card sits in the kingdom zone.
  onMyPhaseBegin KingdomPhase \_owner self ->
    when (self.zone == KingdomZone) do
      let pk = self.controller
      g <- getGame
      when (any (\u -> u.controller == pk) g.units) $
        may pk "Temple of Shallya: move one of your units to another zone?" $
          withTarget pk ownUnit \uk ->
            withTarget pk MyAnyZone \zk ->
              moveUnit uk zk

defendTheBorder :: CardDef Quest
defendTheBorder = questCard "core-043" "Defend the Border" do
  race Empire
  cost 0
  loyalty 2
  body "Quest. While Defend the Border has 3 or more resource tokens on it, redirect the first point of damage done to your capital each turn to another target unit or capital. Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  -- True redirect, evaluated live by the engine's capital-damage
  -- pipeline: while the quest holds 3+ tokens, the first point of
  -- capital damage each turn (on EITHER player's turn) is redirected
  -- to a unit or capital section of the controller's choice.
  redirectsFirstCapitalDamage \_g q -> q.tokens >= 3

willOfTheElectors :: CardDef Tactic
willOfTheElectors = tacticCard "core-044" "Will of the Electors" do
  race Empire
  cost 1
  loyalty 1
  body "Action: Move up to two target developments from one zone to another controlled by the same player."
  whenResolved \self -> do
    let pk = self.controller
    -- "Up to two": pick source and destination, then ask how many
    -- (1..min 2 available) to move.
    withTarget pk (CapitalMatching \me (owner, _) -> owner == me) \(_, fromZ) ->
      withTarget pk MyAnyZone \toZ ->
        when (fromZ /= toZ) do
          g <- getGame
          let me = playerOf pk g
              zoneOf = \case
                KingdomZone -> me.capital.kingdom
                QuestZone -> me.capital.quest
                BattlefieldZone -> me.capital.battlefield
              Developments avail = (zoneOf fromZ).developments
              cap = min 2 avail
          when (cap > 0) do
            n <- chooseAmount pk 1 cap "Move how many developments?"
            replicateM_ n (moveDevelopment pk fromZ toZ)

twinTailedComet :: CardDef Tactic
twinTailedComet = tacticCard "core-045" "Twin-Tailed Comet" do
  race Empire
  cost 2
  loyalty 4
  body "Action: Target a tactic just played. Copy the effects of that tactic without paying its cost. (You choose all targets of the copied tactic.)"
  -- The engine updates 'lastResolvedTactic' AFTER each tactic's
  -- body fires, so when Comet's body runs it reads the previously
  -- resolved tactic (exactly the "just played" referent the card
  -- text names) and re-fires it under the Comet controller, who
  -- re-picks every target via the tactic's own 'withTarget' calls.
  -- The 'code /= self' guard is a defensive no-op in case the
  -- caller ever resolves Comet without a prior tactic on record.
  whenResolved \self -> do
    g <- getGame
    case g.lastResolvedTactic of
      Just (code, target, x)
        | code /= self.cardDef.code ->
            push (TacticResolved self.controller code target x)
      _ -> pure ()

demoralise :: CardDef Tactic
demoralise = tacticCard "core-046" "Demoralise" do
  race Empire
  cost 0
  loyalty 1
  body "Action: One target unit loses {power}{power} until the end of the turn."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k ->
      until EndOfTurn $ buffPower k (-2)

franzsDecree :: CardDef Tactic
franzsDecree = tacticCard "core-047" "Franz's Decree" do
  race Empire
  cost 1
  loyalty 1
  body "Action: One target unit cannot attack or defend until the end of the turn."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> do
      until EndOfTurn $ disableAttack k
      until EndOfTurn $ disableDefend k

forcedMarch :: CardDef Tactic
forcedMarch = tacticCard "core-048" "Forced March" do
  race Empire
  cost 2
  loyalty 2
  body "Play during your turn. Action: Move one target unit from its zone to another zone controlled by the same player."
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \uk ->
      withUnit uk \u ->
        -- "Another zone controlled by the same player" — own or
        -- opponent, but the unit's controller's zones, not the
        -- caster's. So we pick from `u.controller`'s zones.
        withTarget u.controller MyAnyZone \zk -> moveUnit uk zk

judgementOfVerena :: CardDef Tactic
judgementOfVerena = tacticCard "core-049" "Judgement of Verena" do
  race Empire
  cost 4
  loyalty 2
  trait Spell
  body "Play during your turn. Action: Destroy all unit and support cards in each zone with no developments."
  whenResolved \_ -> do
    g <- getGame
    let zonesFor :: Player -> [(PlayerKey, ZoneKind, Zone)]
        zonesFor p =
          let cap = p.capital
           in [ (p.key, KingdomZone, cap.kingdom)
              , (p.key, QuestZone, cap.quest)
              , (p.key, BattlefieldZone, cap.battlefield)
              ]
        undeveloped =
          [ (pk, zk)
          | (pk, zk, z) <- zonesFor g.player1 <> zonesFor g.player2
          , z.developments == 0
          ]
        targetUnits =
          [ u.key | u <- g.units
                  , (u.controller, u.zone) `elem` undeveloped
          ]
        targetSupports =
          [ s.key | s <- g.supports
                  , s.attachedTo == Nothing
                  , (s.controller, s.zone) `elem` undeveloped
          ]
    for_ targetUnits destroyUnit
    for_ targetSupports destroySupport

sigmarsIntervention :: CardDef Tactic
sigmarsIntervention = tacticCard "core-050" "Sigmar's Intervention" do
  race Empire
  cost 2
  loyalty 2
  trait Spell
  body "Play when an opponent attacks your capital, before defenders are declared. Action: Redirect the attack to a different one of your zones. (An attack cannot be redirected to a burning zone.)"
  -- The "play during the AfterDeclareCombatTarget window" timing
  -- restriction relies on the player choosing not to fire this any
  -- other time; we surface the redirect itself unconditionally.
  playableWhen \g pk -> case g.combat of
    Just cs -> cs.defendingPlayer == pk
    Nothing -> False
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk MyAnyZone \zk -> redirectAttackZone zk

-- The Corruption cycle ------------------------------------------------

nordlandHalberdiers :: CardDef Unit
nordlandHalberdiers = unitCard "the-skavenblight-threat-003" "Nordland Halberdiers" do
  race Empire
  cost 4
  loyalty 2
  power 2
  hitPoints 2
  trait Warrior
  keyword PlayAnytime
  body "You may play this unit from your hand any time you could take an action."

infiltrateTactic :: CardDef Tactic
infiltrateTactic = tacticCard "the-skavenblight-threat-004" "Infiltrate" do
  race Empire
  cost 1
  loyalty 2
  body "Action: Target opponent cannot draw more than one card this turn."
  whenResolved \self ->
    push (SetDrawCap self.controller.next 1)

gustavTheBear :: CardDef Unit
gustavTheBear = unitCard "path-of-the-zealot-023" "Gustav the Bear" do
  hero
  trait Cavalry
  race Empire
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  body "Limit one Hero per zone. Cancel all damage to this unit while any opponent controls a corrupted unit."
  damageImmuneWhen \g self ->
    any (\u -> u.controller /= self.controller && u.corrupted) g.units

talabheimDetachment :: CardDef Unit
talabheimDetachment = unitCard "path-of-the-zealot-024" "Talabheim Detachment" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Action: Spend 1 resource to return this unit to its owner's hand."
  action "Withdraw" 1 \usage ->
    returnUnitToHand usage.self.key

gateOfSigmar :: CardDef Support
gateOfSigmar = supportCard "path-of-the-zealot-025" "Gate of Sigmar" do
  race Empire
  cost 4
  loyalty 2
  power 2
  trait Building
  body "Cancel one damage assigned to your capital each turn."
  capitalShieldEachTurn

reiksguardSwordsmen :: CardDef Unit
reiksguardSwordsmen = unitCard "tooth-and-claw-044" "Reiksguard Swordsmen" do
  race Empire
  cost 5
  loyalty 2
  power 2
  hitPoints 2
  trait Knight
  counterstrike 4
  body "Counterstrike 4 (this unit deals 4 combat damage immediately after defending)."

ironDiscipline :: CardDef Tactic
ironDiscipline = tacticCard "tooth-and-claw-045" "Iron Discipline" do
  race Empire
  cost 0
  loyalty 1
  body
    "Action: Target one unit. Until the end of the turn, cancel any other action that targets \
    \this unit unless the action's controller pays an additional 4 resources (per action)."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k ->
      until EndOfTurn $ imposeTargetTax k 4

vigilantElector :: CardDef Unit
vigilantElector = unitCard "the-deathmaster-s-dance-064" "Vigilant Elector" do
  race Empire
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Warrior
  body
    "Quest. Action: Spend 1 resource to attach this unit to a target unit. It then counts \
    \as an Attachment with the text \"Attached unit is destroyed at the end of its controller's turn.\""
  quest $ action "Mark for judgement" 1 \usage ->
    withTarget usage.user (unitWhere \u -> u.key /= usage.self.key) \host ->
      push (TransformUnitToAttachment usage.self.key host)

flagellants :: CardDef Unit
flagellants = unitCard "the-deathmaster-s-dance-065" "Flagellants" do
  race Empire
  cost 1
  loyalty 1
  power 0
  hitPoints 2
  trait Zealot
  body "Action: Sacrifice this unit to cancel the next 2 damage assigned to your capital this turn."
  actionWith "Martyrdom" 0 [SacrificeSelf] \usage ->
    push (ArmCapitalShield usage.user (Just 2) 0)

ulricsFury :: CardDef Tactic
ulricsFury = tacticCard "the-deathmaster-s-dance-066" "Ulric's Fury" do
  race Empire
  cost 2
  loyalty 2
  body "Action: Each of your defending units gains Counterstrike 1 until the end of the turn."
  whenResolved \self ->
    push (ArmDefenderCounterstrike self.controller 1)

vigilantPistoliers :: CardDef Unit
vigilantPistoliers = unitCard "the-warpstone-chronicles-084" "Vigilant Pistoliers" do
  race Empire
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Warrior
  body
    "If this unit is destroyed while in your battlefield, put it into play in your kingdom \
    \instead of into your discard pile."
  onDestroyedRelocate \_g self ->
    if self.zone == BattlefieldZone then Just KingdomZone else Nothing

runefangOfSolland :: CardDef Support
runefangOfSolland = supportCard "the-warpstone-chronicles-085" "Runefang of Solland" do
  unique
  race Empire
  cost 2
  loyalty 1
  traits [Attachment, Relic]
  body
    "Attach to a target {empire} unit. Attached unit gains {power}{power}. \
    \Action: Sacrifice attached unit to lower the cost of the next non-Epic tactic you play this turn to 0."
  attachmentPower 2
  action "Final stand" 0 \usage ->
    for_ usage.self.attachedTo \host -> do
      destroyUnit host
      push (ArmFreeTactic usage.user)

helblasterVolleyGun :: CardDef Support
helblasterVolleyGun = supportCard "the-warpstone-chronicles-086" "Helblaster Volley Gun" do
  race Empire
  cost 1
  loyalty 1
  traits [Attachment, Weapon]
  body
    "Attach to a target unit in your battlefield. Attached unit deals +X damage in combat, \
    \where X is the number of developments in your battlefield."
  supportCombat \g s u ->
    if s.attachedTo == Just u.key
      then
        let me = playerOf s.controller g
            Developments d = me.capital.battlefield.developments
         in d
      else 0

helblasterCrew :: CardDef Unit
helblasterCrew = unitCard "arcane-fire-104" "Helblaster Crew" do
  race Empire
  cost 4
  loyalty 2
  power 2
  hitPoints 2
  trait Engineer
  body
    "Action: When this unit enters play, move one target Attachment from one unit to another \
    \unit in the same zone."
  onEnterPlay \_owner self -> do
    g <- getGame
    let attached = [a | u <- g.units, a <- u.attachments]
    unless (null attached) $
      may self.controller "Helblaster Crew: move an attachment to another unit in the same zone?" $
        withTarget self.controller
          (SupportMatching \_ _ s -> isJust s.attachedTo)
          \aKey -> do
            gNow <- getGame
            let mhost =
                  listToMaybe
                    [u | u <- gNow.units, any ((== aKey) . (.key)) u.attachments]
            whenJust mhost \host -> do
              let dests =
                    [ u.key
                    | u <- gNow.units
                    , u.zone == host.zone
                    , u.key /= host.key
                    ]
              forcePickUnit self.controller dests
                "Choose the attachment's new host (same zone)."
                \dst -> push (MoveAttachment aKey dst)

sigmarsBrilliance :: CardDef Tactic
sigmarsBrilliance = tacticCard "arcane-fire-105" "Sigmar's Brilliance" do
  race Empire
  cost 10
  loyalty 3
  traits [Epic, Spell]
  body "Action: Rearrange your units and support cards, moving any number of them into new zones."
  whenResolved \self -> do
    let pk = self.controller
        movable g =
          any (\u -> u.controller == pk) g.units
            || any (\s -> s.controller == pk && isNothing s.attachedTo) g.supports
        loop budget = when (budget > (0 :: Int)) do
          g <- getGame
          when (movable g) $
            may pk "Sigmar's Brilliance: move a card to another zone?" do
              withTarget pk
                (ownUnit `Or` SupportMatching \me _ s -> s.controller == me && isNothing s.attachedTo)
                \case
                  TargetUnitOption k ->
                    withTarget pk MyAnyZone \zk -> moveUnit k zk
                  TargetSupportOption k ->
                    withTarget pk MyAnyZone \zk -> push (MoveSupport k zk)
                  _ -> pure ()
              loop (budget - 1)
    loop 20

protectTheEmpire :: CardDef Quest
protectTheEmpire = questCard "arcane-fire-106" "Protect the Empire" do
  race Empire
  cost 0
  loyalty 2
  trait QuestTrait
  body "Quest. Any unit questing on this card can defend any of your zones when they are attacked."
  questerDefendsAnywhere

-- Days of Blood --------------------------------------------------------

ludwigSchwarzheim :: CardDef Unit
ludwigSchwarzheim = unitCard "days-of-blood-007" "Ludwig Schwarzheim" do
  unique
  race Empire
  cost 4
  loyalty 3
  power 3
  hitPoints 3
  trait Knight
  body
    "Toughness X. X is the number of experiences attached to this unit. \
    \Action: When this unit attacks or defends, attach 1 experience to it."
  -- Toughness X here is experience-derived, distinct from the engine's
  -- development-counting 'Toughness Variable' keyword.
  selfToughness \_g u -> length u.experiences
  gainsExperienceOnAttackOrDefend

-- Oaths of Vengeance ---------------------------------------------------

maidOfSigmar :: CardDef Unit
maidOfSigmar = unitCard "oaths-of-vengeance-028" "Maid of Sigmar" do
  race Empire
  cost 2
  loyalty 2
  power 0
  hitPoints 2
  trait Priest
  battlefieldOnly
  body
    "Battlefield only. This unit gains {power} for each experience attached to it. \
    \Action: When this unit attacks or defends, attach 1 experience to it."
  selfPower \_g u -> length u.experiences
  gainsExperienceOnAttackOrDefend

-- Battle for the Old World ---------------------------------------------

reiksguardElite :: CardDef Unit
reiksguardElite = unitCard "battle-for-the-old-world-047" "Reiksguard Elite" do
  race Empire
  cost 3
  loyalty 2
  power 2
  hitPoints 1
  traits [Elite, Knight]
  battlefieldOnly
  body
    "Battlefield only. This unit gains +1 hit point for every experience attached to it. \
    \Action: When this unit attacks or defends, attach 1 experience to it."
  selfHP \_g u -> length u.experiences
  gainsExperienceOnAttackOrDefend

chapterhouseStables :: CardDef Support
chapterhouseStables = supportCard "battle-for-the-old-world-049" "Chapterhouse Stables" do
  race Empire
  cost 3
  loyalty 1
  power 1
  trait Building
  body "Lower the cost of the first Knight unit you play each turn by 1."
  globalCostAdjust \g self pk filt ->
    let knightsPlayed =
          length
            [ cf
            | cf <- Map.findWithDefault [] pk (historyOfThisTurn g).playedBy
            , cf.cfKind == Unit
            , Knight `elem` cf.cfTraits
            ]
     in if pk == self.controller
           && filt.cfKind == Unit
           && Knight `elem` filt.cfTraits
           && knightsPlayed == 0
          then -1
          else 0

-- The Ruinous Hordes ---------------------------------------------------

steelBehemoth :: CardDef Unit
steelBehemoth = unitCard "the-ruinous-hordes-085" "Steel Behemoth" do
  race Empire
  cost 6
  loyalty 4
  power 4
  hitPoints 4
  trait WarMachine
  battlefieldOnly
  body
    "Battlefield only. Toughness X. X is the number of all opponent's units \
    \participating in combat."
  selfToughness \g u -> case g.combat of
    Just cs ->
      length
        [ k
        | k <- cs.attackers <> cs.defenders
        , maybe False (\v -> v.controller == u.controller.next) (findUnit k g)
        ]
    Nothing -> 0

-- Faith and Steel ------------------------------------------------------

theEmperorsStatue :: CardDef Support
theEmperorsStatue = supportCard "faith-and-steel-103" "The Emperor's Statue" do
  race Empire
  cost 1
  loyalty 1
  power 1
  trait Building
  body "If you control a non-[Empire] card, sacrifice this card."
  sacrificeIfControlsOffFaction Empire

ostlandGreatswords :: CardDef Unit
ostlandGreatswords = unitCard "glory-of-days-past-065" "Ostland Greatswords" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Warrior
  raider 2
  body "Raider 2."

-- Bloodquest: Rising Dawn -----------------------------------------------

rageOfTheBear :: CardDef Tactic
rageOfTheBear = tacticCard "rising-dawn-008" "Rage of the Bear" do
  race Empire
  cost 1
  loyalty 2
  trait Spell
  body
    "Action: Heal 1 damage on target Mage unit. It deals +2 damage in combat until the end \
    \of the turn."
  playableWhen $ hasTarget (unitWhere \u -> Mage `elem` u.cardDef.traits)
  whenResolved \self ->
    withTarget self.controller (unitWhere \u -> Mage `elem` u.cardDef.traits) \k -> do
      healUnit k 1
      until EndOfTurn $ buffCombatDamage k 2

-- Bloodquest: Fragments of Power ----------------------------------------

averheimSoldiers :: CardDef Unit
averheimSoldiers = unitCard "fragments-of-power-027" "Averheim Soldiers" do
  race Empire
  cost 2
  loyalty 1
  power 0
  hitPoints 2
  trait Warrior
  body
    "This unit deals +2 damage in combat. Action: When this unit defends, it gets +1 hit \
    \point for each resource token on quests you control until the end of the turn."
  combatPower \_g _u -> 2
  onReceive $ Receive \msg _owner self -> case msg of
    DeclareDefenders ks | self.key `elem` ks -> do
      g <- getGame
      let toks = sum [q.tokens | q <- g.quests, q.controller == self.controller]
      when (toks > 0) $ until EndOfTurn $ buffHP self.key toks
    _ -> pure ()

-- Bloodquest: Shield of the Gods ----------------------------------------

steelStandard :: CardDef Unit
steelStandard = unitCard "shield-of-the-gods-107" "Steel Standard" do
  race Empire
  cost 2
  loyalty 1
  power 0
  hitPoints 2
  trait Knight
  body "Knight units in this zone gain Toughness 1."
  toughnessAura \_g self u ->
    if Knight `elem` u.cardDef.traits && u.zone == self.zone && u.controller == self.controller
      then 1
      else 0

-- The Capital Cycle ----------------------------------------------------

imperialDrummer :: CardDef Unit
imperialDrummer = unitCard "the-imperial-throne-103" "Imperial Drummer" do
  race Empire
  cost 1
  loyalty 2
  power 0
  hitPoints 1
  trait Musician
  body "Battlefield. Warrior units in this zone gain {power}."
  unitAura \_g self u ->
    if self.zone == BattlefieldZone
      && u.zone == BattlefieldZone
      && u.controller == self.controller
      && Warrior `elem` u.cardDef.traits
      then 1
      else 0

priestsOfSigmar :: CardDef Unit
priestsOfSigmar = unitCard "the-imperial-throne-106" "Priests of Sigmar" do
  race Empire
  cost 5
  loyalty 2
  power 2
  hitPoints 3
  traits [Cavalry, Priest]
  body "Toughness X. X is the highest loyalty on an {empire} card you control."
  selfToughness \g u -> highestLoyaltyControlled Empire g u.controller

stirlandDeathjacks :: CardDef Unit
stirlandDeathjacks = unitCard "the-imperial-throne-107" "Stirland Deathjacks" do
  race Empire
  cost 1
  loyalty 3
  power 1
  hitPoints 1
  trait Warrior
  body "Action: When this unit enters or leaves play, deal 1 damage to target corrupted unit."
  onEnterPlay \_owner self -> do
    g <- getGame
    when (any (.corrupted) g.units) $
      withTarget self.controller (UnitMatching \_ _ u -> u.corrupted) (`dealDamage` 1)
  onSelfLeavesPlay \_owner self -> do
    g <- getGame
    when (any (.corrupted) g.units) $
      withTarget self.controller (UnitMatching \_ _ u -> u.corrupted) (`dealDamage` 1)

wingedRidersOfKislev :: CardDef Unit
wingedRidersOfKislev = unitCard "the-imperial-throne-108" "Winged Riders of Kislev" do
  race Empire
  cost 5
  loyalty 3
  power 3
  hitPoints 3
  trait Cavalry
  body "Action: When this unit attacks, it gains {power} for each other attacking unit."
  onMyAttackDeclared \_owner self _zone attackers ->
    until EndOfTurn $ buffPower self.key (length (filter (/= self.key) attackers))

recruitingForWar :: CardDef Quest
recruitingForWar = questCard "the-imperial-throne-120" "Recruiting for War" do
  race Empire
  cost 0
  loyalty 3
  body
    "Quest. Action: When this card enters play, draw a card. Quest. Action: When you play \
    \an {empire} non-Attachment support card from your hand, draw a card if a unit is \
    \questing here."
  onEnterPlay \_owner self -> drawCard self.controller
  onQuestSupportPayoff Empire \self -> drawCard self.controller

doublingOfTheGuard :: CardDef Tactic
doublingOfTheGuard = tacticCard "the-imperial-throne-112" "Doubling of the Guard" do
  race Empire
  cost 1
  loyalty 2
  body "Action: Discard a card from your hand with X loyalty to draw X cards."
  playableWhen \g pk -> not (null (playerOf pk g).hand)
  whenResolved \self ->
    discardForLoyalty self.controller \x -> when (x > 0) $ drawCards self.controller x

-- Cataclysm cycle ------------------------------------------------------

ferventDisciple :: CardDef Unit
ferventDisciple = unitCard "cataclysm-020" "Fervent Disciple" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Priest
  body "If this zone is not burning, this unit deals +2 damage in combat."
  combatPower \g self ->
    if zoneBurning g self.controller self.zone then 0 else 2

luminarkOfHysh :: CardDef Support
luminarkOfHysh = supportCard "cataclysm-021" "Luminark of Hysh" do
  race Empire
  cost 3
  loyalty 1
  power 2
  trait WarMachine
  body "If this zone is burning, this card loses {power}{power}."
  zonePowerAura \g s zone ->
    if zone == s.zone && zoneBurning g s.controller s.zone then -2 else 0

garrisoned :: CardDef Support
garrisoned = supportCard "cataclysm-022" "Garrisoned" do
  race Empire
  cost 1
  loyalty 1
  power 0
  traits [Attachment, Condition]
  body
    "Attach to a target unit. If this zone is not burning, attached unit \
    \deals +2 damage in combat."
  supportCombat \g s u ->
    if s.attachedTo == Just u.key && not (zoneBurning g u.controller u.zone)
      then 2
      else 0

-- The Morrslieb cycle ---------------------------------------------------

chainLightning :: CardDef Tactic
chainLightning = tacticCard "the-chaos-moon-028" "Chain Lightning" do
  race Empire
  cost 2
  loyalty 3
  trait Spell
  body "Action: Destroy up to two target attacking units, each with a printed cost 3 or lower."
  whenResolved \self ->
    withUpTo self.controller 2
      (UnitMatching \_pk g u -> unitIsAttacking g u && costAtMost 3 u.cardDef)
      (traverse_ destroyUnit)

steamTank :: CardDef Unit
steamTank = unitCard "omens-of-ruin-006" "Steam Tank" do
  race Empire
  cost 4
  loyalty 3
  power 0
  hitPoints 7
  trait WarMachine
  body
    "This unit gains {power} for each resource token on it. Action: At the \
    \beginning of your turn, put a resource token on this unit."
  selfPower \_g self -> self.tokens
  onMyTurnBegin \_owner self -> push (AdjustUnitTokens self.key 1)

gloriousPreceptor :: CardDef Unit
gloriousPreceptor = unitCard "the-eclipse-of-hope-086" "Glorious Preceptor" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Knight
  body "Action: When this unit enters or leaves play, draw a card."
  onEnterPlay \_owner self -> drawCard self.controller
  onSelfLeavesPlay \_owner self -> drawCard self.controller

osterknachtElite :: CardDef Unit
osterknachtElite = unitCard "the-eclipse-of-hope-087" "Osterknacht Elite" do
  race Empire
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  traits [Knight, Elite]
  body
    "Action: When this unit enters play, return another target unit you control \
    \and target unit an opponent controls to their owner's hands."
  onEnterPlay \_owner self -> do
    withTarget self.controller
      (UnitMatching \pk _ u -> u.controller == pk && u.key /= self.key)
      returnUnitToHand
    withTarget self.controller enemyUnit returnUnitToHand

innerCircleKnight :: CardDef Unit
innerCircleKnight = unitCard "signs-in-the-stars-068" "Inner Circle Knight" do
  race Empire
  cost 2
  loyalty 1
  power 1
  hitPoints 3
  trait Knight

rallyPoint :: CardDef Support
rallyPoint = supportCard "the-twin-tailed-comet-048" "Rally Point" do
  race Empire
  cost 1
  loyalty 2
  trait Location
  body
    "Action: At the beginning of your turn, move target unit you control from \
    \its current zone to this zone."
  onMyTurnBegin \_owner self ->
    withTarget self.controller ownUnit \k -> moveUnit k self.zone

theSealedVaults :: CardDef Tactic
theSealedVaults = tacticCard "portent-of-doom-088" "The Sealed Vaults" do
  race Empire
  cost 0
  loyalty 2
  body "Action: Discard the top 3 cards of your deck. Gain 1 resource for each discarded card with a Quest ability."
  -- TODO: approximation — counts Quest-*type* cards only. The printed
  -- card counts any discarded card "with a Quest ability", which also
  -- covers units/supports carrying a "Quest." ability line (e.g.
  -- Strollaz's Banner). Widen this predicate beyond 'asQuest' once card
  -- defs expose whether they have a quest ability.
  whenResolved \self -> do
    let pk = self.controller
    searchTopOfDeck pk 3 \result -> do
      let top3 = take 3 result.cards
          quests = length [c | c <- top3, isJust (asQuest c.def)]
      push (DiscardCardsFromDeck pk (map (.key) top3))
      when (quests > 0) $ gainResources pk quests

veteranGreatsword :: CardDef Unit
veteranGreatsword = unitCard "fiery-dawn-107" "Veteran Greatsword" do
  race Empire
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Warrior
  body
    "Action: When this unit enters play, it gains {power} for each unit in this \
    \zone until the end of the turn."
  onEnterPlay \_owner self -> do
    g <- getGame
    let n = length [u | u <- g.units, u.zone == self.zone]
    until EndOfTurn $ buffPower self.key n

-- The Enemy cycle -------------------------------------------------------

brightWizardAcolyte :: CardDef Unit
brightWizardAcolyte = unitCard "the-fourth-waystone-083" "Bright Wizard Acolyte" do
  race Empire
  cost 2
  loyalty 1
  power 2
  hitPoints 2
  trait Mage
  body "Forced: When your turn ends, this unit takes 1 uncancellable damage."
  onMyTurnEnd \_owner self -> dealUncancellableDamage self.key 1

calledBack :: CardDef Tactic
calledBack = tacticCard "the-fall-of-karak-grimaz-026" "Called Back" do
  race Empire
  cost 2
  loyalty 2
  body "Action: Return target unit to its owner's hand."
  playableWhen \g pk -> hasTarget AnyUnit g pk
  whenResolved \self ->
    withTarget self.controller AnyUnit returnUnitToHand

blessingOfMyrmidia :: CardDef Tactic
blessingOfMyrmidia = tacticCard "bleeding-sun-107" "Blessing of Myrmidia" do
  race Empire
  cost 1
  loyalty 2
  body "Action: Each of your attacking or defending Knight units gains {power} until the end of the turn."
  whenResolved \self ->
    buffEachUntilEoT self.controller
      (UnitMatching \pk g u ->
        u.controller == pk
          && Knight `elem` u.cardDef.traits
          && (unitIsAttacking g u || unitIsDefending g u))
      1

derricksburgForge :: CardDef Support
derricksburgForge = supportCard "the-burning-of-derricksburg-005" "Derricksburg Forge" do
  race Empire
  cost 2
  loyalty 1
  power 1
  trait Building
  body
    "Kingdom. Action: When this card enters play, gain 1 resource. Quest. \
    \Action: When this card enters play, draw a card."
  kingdom $ onEnterPlay \_owner self -> gainResources self.controller 1
  quest $ onEnterPlay \_owner self -> drawCard self.controller

-- The Enemy cycle (batch 2) ---------------------------------------------

miningTunnels :: CardDef Support
miningTunnels = supportCard "the-burning-of-derricksburg-002" "Mining Tunnels" do
  race Empire
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Action: When you play a development from your hand, draw a card."
  onYouPlayDevelopment \_owner self -> drawCard self.controller

griffonStandard :: CardDef Support
griffonStandard = supportCard "the-burning-of-derricksburg-006" "Griffon Standard" do
  race Empire
  cost 2
  loyalty 3
  trait Attachment
  body "Attach to an [Empire] unit in your battlefield. Attached unit gains {power} for each [Empire] unit in this zone."
  supportAura \g s u ->
    if Just u.key == s.attachedTo
      then length [v | v <- g.units, v.controller == u.controller, v.zone == u.zone, Empire `elem` v.cardDef.races]
      else 0

blackKnightsOfMorr :: CardDef Unit
blackKnightsOfMorr = unitCard "the-silent-forge-043" "Black Knights of Morr" do
  race Empire
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  trait Knight
  body "Action: When a unit enters this zone, target unit cannot defend until the end of the turn."
  onUnitEnterMyZone \_owner self _uk ->
    withTarget self.controller AnyUnit \k -> until EndOfTurn $ disableDefend k

higherGround :: CardDef Support
higherGround = supportCard "redemption-of-a-mage-065" "Higher Ground" do
  race Empire
  cost 1
  loyalty 2
  trait Environment
  body "Battlefield. Action: When a Knight unit enters this zone, that unit gains {power} until the end of the turn."
  onUnitEnterMyZone \_owner _self uk -> do
    g <- getGame
    whenJust (findUnit uk g) \u ->
      when (Knight `elem` u.cardDef.traits) $ until EndOfTurn $ buffPower uk 1

douseTheFlames :: CardDef Tactic
douseTheFlames = tacticCard "the-fourth-waystone-084" "Douse the Flames" do
  race Empire
  cost 0
  loyalty 2
  body "Action: Move 1 damage from one target unit to another target unit."
  playableWhen \g pk -> hasTarget (unitWhere isDamaged) g pk
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk (unitWhere isDamaged) \src ->
      withTarget pk (unitWhere (\u -> u.key /= src)) \dst ->
        moveDamage src dst 1

-- Assault on Ulthuan ---------------------------------------------------

goldWizardAcolyte :: CardDef Unit
goldWizardAcolyte = unitCard "assault-on-ulthuan-046" "Gold Wizard Acolyte" do
  race Empire
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Mage
  body "Battlefield. Action: When you declare this unit as an attacker, one target unit cannot defend against this attack."
  battlefield $ onMyAttackDeclared \_owner self _zone _attackers ->
    withTarget self.controller enemyUnit \k -> until EndOfTurn $ disableDefend k

surrender :: CardDef Tactic
surrender = tacticCard "assault-on-ulthuan-048" "Surrender!" do
  race Empire
  cost 1
  loyalty 2
  body "Action: Return target defending unit to its owner's hand."
  playableWhen $ hasTarget defendingUnit
  whenResolved \self ->
    withTarget self.controller defendingUnit returnUnitToHand

-- March of the Damned --------------------------------------------------

warMachineEmplacement :: CardDef Support
warMachineEmplacement = supportCard "march-of-the-damned-008" "War Machine Emplacement" do
  race Empire
  cost 1
  loyalty 2
  power 0
  traits [Siege, WarMachine]
  body
    "Battlefield. Action: Spend 2 resources to deal 2 damage to target attacking unit \
    \(limit once per turn)."
  battlefield $ actionWith "Bombard" 2 [] \usage -> do
    g <- getGame
    let used =
          any (\m -> m.details == ActionUsedThisTurn)
            (Map.findWithDefault [] (UnitRef usage.self.key) g.modifiers)
    unless used do
      until EndOfTurn (PendingBuff usage.self.key ActionUsedThisTurn)
      withTarget usage.user attackingUnit \k -> dealDamage k 2

gardenOfMorr :: CardDef Support
gardenOfMorr = supportCard "march-of-the-damned-009" "Garden of Morr" do
  race Empire
  cost 3
  loyalty 2
  power 1
  trait Building
  body
    "This card gains {power} for each resource token on it. Action: When one or more units \
    \are destroyed, put a resource token on this card."
  zonePowerAura \_g s zone -> if s.zone == zone then s.tokens else 0
  -- Approximation: banks one token per 'DestroyUnit' message. The
  -- printed text grants a single token per destruction *event*, so a
  -- simultaneous multi-unit wipe over-counts slightly; the common
  -- one-at-a-time case is exact.
  onReceive $ Receive \msg _owner self -> case msg of
    DestroyUnit _ -> adjustSupportTokens self.key 1
    _ -> pure ()

manaanTakeYou :: CardDef Tactic
manaanTakeYou = tacticCard "march-of-the-damned-010" "Manaan Take You!" do
  race Empire
  cost 3
  loyalty 2
  trait Spell
  body "Action: Destroy target unit or support card in a zone with no developments."
  playableWhen $ hasTarget (Or unitInEmptyZone supportInEmptyZone)
  whenResolved \self ->
    withTarget self.controller (Or unitInEmptyZone supportInEmptyZone) \case
      TargetUnitOption k -> destroyUnit k
      TargetSupportOption k -> destroySupport k
      _ -> pure ()
  where
    unitInEmptyZone = UnitMatching \_pk g u -> devsInZone g u == 0
    supportInEmptyZone = SupportMatching \_pk g s -> zoneDevsFor g s.controller s.zone == 0

-- | Facedown developments in a named zone of a player's capital.
zoneDevsFor :: Game -> PlayerKey -> ZoneKind -> Int
zoneDevsFor g pk z =
  let p = playerOf pk g
      Developments d = case z of
        KingdomZone -> p.capital.kingdom.developments
        QuestZone -> p.capital.quest.developments
        BattlefieldZone -> p.capital.battlefield.developments
   in d

-- Legends (deluxe expansion) -------------------------------------------

callForReserves :: CardDef Tactic
callForReserves = tacticCard "legends-021" "Call for Reserves" do
  race Empire
  cost 2
  loyalty 3
  body
    "Action: Return target unit you control to its owner's hand. Then, put \
    \into play from your hand a unit with printed cost 3 or lower."
  playableWhen $ hasTarget ownUnit
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk ownUnit \k -> do
      returnUnitToHand k
      me <- playerOf pk <$> getGame
      let reserves =
            [ c
            | c <- me.hand
            , Just cd <- [asUnit c.def]
            , costAtMost 3 cd
            ]
      chooseFromCards pk 0 1 reserves
        "Put a unit with printed cost 3 or lower into play." \chosen ->
          for_ chosen \c ->
            withTarget pk MyAnyZone \zk -> putUnitIntoPlay pk FromHand c.key zk
