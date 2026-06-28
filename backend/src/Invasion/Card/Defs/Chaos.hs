{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Chaos core cards (core-081..105). Heavy on corruption interactions
-- so the engine's existing 'CorruptUnit' / 'CleanseUnit' messages do a
-- lot of work here.
module Invasion.Card.Defs.Chaos (module Invasion.Card.Defs.Chaos) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Invasion.Capital
import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Invasion.Entity (LegendDetails (..), QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

servantsOfKhorne :: CardDef Unit
servantsOfKhorne = unitCard "core-081" "Servants of Khorne" do
  race Chaos
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Battlefield only."
  battlefieldOnly

savageMarauders :: CardDef Unit
savageMarauders = unitCard "core-082" "Savage Marauders" do
  race Chaos
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior

festeringNurglings :: CardDef Unit
festeringNurglings = unitCard "core-083" "Festering Nurglings" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Daemon
  body "Forced: After this unit enters play, corrupt one target unit in any player's corresponding zone."
  onEnterPlay \_owner self ->
    withTarget self.controller
      (UnitMatching \_ _ u -> u.zone == self.zone)
      \k -> push (CorruptUnit k)

nurgleSorcerer :: CardDef Unit
nurgleSorcerer = unitCard "core-084" "Nurgle Sorcerer" do
  race Chaos
  cost 3
  loyalty 3
  power 1
  hitPoints 2
  trait Sorceror
  body "Quest. Action: Spend 3 resources to deal 1 damage to one target unit. Deal an additional damage to the target if that unit is corrupted."
  quest $ action "Plague them" 3 \usage ->
    withTarget usage.user AnyUnit \k ->
      withUnit k \u ->
        dealDamage k (if u.corrupted then 2 else 1)

chaosKnights :: CardDef Unit
chaosKnights = unitCard "core-085" "Chaos Knights" do
  race Chaos
  cost 5
  loyalty 2
  power 3
  hitPoints 4
  traits [Knight, Cavalry]

cultistOfSlaanesh :: CardDef Unit
cultistOfSlaanesh = unitCard "core-086" "Cultist of Slaanesh" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Zealot
  body "Quest. Action: Spend 4 resources to destroy one target development."
  quest $ action "Defile" 4 \usage ->
    withTarget usage.user AnyDevelopmentZone \(owner, zk) ->
      destroyDevelopment owner zk

valkiaTheBloody :: CardDef Unit
valkiaTheBloody = unitCard "core-087" "Valkia the Bloody" do
  hero
  trait Daemon
  race Chaos
  cost 4
  loyalty 3
  power 2
  hitPoints 4
  body "Limit one Hero per zone. Quest. Action: Spend 2 resources to move any number of damage on this unit to a target corrupted unit."
  quest $ action "Spite" 2 \usage ->
    withTarget usage.user
      (UnitMatching \_ _ u -> u.corrupted)
      \dst -> moveAllDamage usage.self.key dst

melekhTheChanger :: CardDef Unit
melekhTheChanger = unitCard "core-088" "Melekh the Changer" do
  hero
  trait Mage
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  body "Limit one Hero per zone. This unit gains {power} for each corrupted card controlled by an opponent."
  effects \self owner -> do
    let opp = owner.key.next
    units <- (.units) <$> getGame
    let n = length [u | u <- units, u.controller == opp, u.corrupted]
    when (n > 0) (gainPower self n)

fledglingChaosSpawn :: CardDef Unit
fledglingChaosSpawn = unitCard "core-089" "Fledgling Chaos Spawn" do
  race Chaos
  cost 0
  loyalty 1
  power 0
  hitPoints 1
  trait Daemon
  body "Battlefield. Forced: After this unit is destroyed, deal 1 damage to one target unit in any player's battlefield."
  onSelfDestroyed \_owner self ->
    withTarget self.controller
      (UnitMatching \_ _ u -> u.zone == BattlefieldZone)
      \k -> dealDamage k 1

savageGors :: CardDef Unit
savageGors = unitCard "core-090" "Savage Gors" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Battlefield. This unit deals 2 additional damage while attacking if you have 2 or more developments in your battlefield."
  effects \self owner ->
    let devs = case owner.capital.battlefield.developments of
          Developments n -> n
     in when (self.zone == BattlefieldZone && self.attacking && devs >= 2) $
          gainPower self 2

darkZealot :: CardDef Unit
darkZealot = unitCard "core-091" "Dark Zealot" do
  race Chaos
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Zealot

bloodthirster :: CardDef Unit
bloodthirster = unitCard "core-092" "Bloodthirster" do
  race Chaos
  cost 8
  loyalty 5
  power 5
  hitPoints 8
  trait Daemon
  keyword DamageCannotBeCancelled
  body "Damage cannot be cancelled. Forced: After your turn begins, each player must sacrifice a unit in this corresponding zone."
  onMyTurnBegin \_owner self -> do
    g <- getGame
    -- One sacrifice per player, in the same zone as this unit.
    for_ [self.controller, self.controller.next] \pk ->
      withTarget pk
        (UnitMatching \_ _ u -> u.controller == pk && u.zone == self.zone)
        destroyUnit

cloudOfFlies :: CardDef Support
cloudOfFlies = supportCard "core-093" "Cloud of Flies" do
  race Chaos
  cost 0
  loyalty 1
  traits [Attachment, Spell]
  body "Attach to a target unit you control. At the beginning of your turn, you may deal 1 uncancellable damage to this unit and to one target unit."
  -- "You may": the whole package (damage to the host AND to a target)
  -- is optional, gated behind one yes/no.
  onAttachedHostTurnBegin \_owner self host ->
    may self.controller
      "Cloud of Flies: deal 1 uncancellable damage to the attached unit and one target unit?"
      do
        dealUncancellableDamage host.key 1
        withTarget self.controller AnyUnit \k -> dealUncancellableDamage k 1

horrificMutation :: CardDef Support
horrificMutation = supportCard "core-094" "Horrific Mutation" do
  race Chaos
  cost 1
  loyalty 1
  traits [Attachment, Mutation]
  body "Attach to a target unit you control. While attached unit is attacking, defending units get -1 hit points."
  supportHPAura \g self target -> case self.attachedTo of
    Just hostKey | hostKey `elem` maybe [] (.attackers) g.combat ->
      if target.key `elem` maybe [] (.defenders) g.combat then -1 else 0
    _ -> 0

sadisticMutation :: CardDef Support
sadisticMutation = supportCard "core-095" "Sadistic Mutation" do
  race Chaos
  cost 2
  loyalty 2
  traits [Attachment, Mutation]
  body "Attach to a target unit you control. Forced: After the attached unit deals damage in combat, deal 1 damage to one target unit or capital."
  -- Fires whenever combat resolves with the host on the attacker
  -- side. We don't check "the host actually dealt damage" beyond
  -- being a non-corrupted attacker — covers the common case.
  onReceive $ Receive \msg _owner self -> case msg of
    CombatResolved -> case self.attachedTo of
      Just hostKey -> do
        g <- getGame
        case g.combat of
          Just cs | hostKey `elem` cs.attackers ->
            withTarget self.controller (AnyUnit `Or` AnyCapital) \case
              TargetUnitOption u -> dealDamage u 1
              TargetZoneOption owner z -> dealZoneDamage owner z 1
          _ -> pure ()
      Nothing -> pure ()
    _ -> pure ()

warpstoneMeteor :: CardDef Support
warpstoneMeteor = supportCard "core-096" "Warpstone Meteor" do
  race Chaos
  cost 3
  loyalty 2
  power 2
  trait Warpstone
  body "Forced: After your turn begins, each player must corrupt one of his units in this corresponding zone or deal 1 damage to his capital. (Players decide where their own damage is assigned.)"
  -- Each player picks: corrupt one of their own units in this zone
  -- or take 1 indirect damage. With no eligible unit, the damage
  -- is mandatory.
  onMyTurnBegin \_owner self -> do
    g <- getGame
    for_ [self.controller, self.controller.next] \pk -> do
      let candidates =
            [ u.key
            | u <- g.units
            , u.controller == pk
            , u.zone == self.zone
            , not u.corrupted
            ]
      case candidates of
        [] -> indirectDamage pk 1
        _ -> do
          corruptIt <- askYesNo pk "Corrupt one of your units in this zone instead of taking 1 capital damage?"
          if corruptIt
            then withTarget pk
              (UnitMatching \_ _ u ->
                u.controller == pk && u.zone == self.zone && not u.corrupted)
              \k -> push (CorruptUnit k)
            else indirectDamage pk 1

journeyToTheGate :: CardDef Quest
journeyToTheGate = questCard "core-097" "Journey to the Gate" do
  race Chaos
  cost 2
  loyalty 2
  body "Quest. Action: Sacrifice the unit on this quest to force each opponent to discard his hand. Use this ability on your turn, and only if Journey to the Gate has 3 or more resource tokens on it. Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  action "Journey" 0 \usage ->
    withQuest usage.self.key \q ->
      when (q.tokens >= 3) $
        for_ q.questingUnit \quester -> do
          destroyUnit quester
          discardHand usage.user.next

shrineToNurgle :: CardDef Support
shrineToNurgle = supportCard "core-098" "Shrine to Nurgle" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Kingdom. Forced: After an opponent's unit is damaged during combat, corrupt that unit."
  onReceive $ Receive \msg _owner self -> case msg of
    DealDamageToUnit uk n
      | n > 0, self.zone == KingdomZone -> do
          g <- getGame
          when (isJust g.combat) $
            case findUnit uk g of
              Just u | u.controller /= self.controller ->
                push (CorruptUnit uk)
              _ -> pure ()
    _ -> pure ()

seducedByDarkness :: CardDef Tactic
seducedByDarkness = tacticCard "core-099" "Seduced by Darkness" do
  race Chaos
  cost 0
  loyalty 1
  body "Action: Corrupt one target unit."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> push (CorruptUnit k)

willOfTzeentch :: CardDef Tactic
willOfTzeentch = tacticCard "core-100" "Will of Tzeentch" do
  race Chaos
  cost 3
  loyalty 3
  body "Play during your turn. Action: Each player discards his hand and draws three cards."
  whenResolved \self -> do
    eachPlayer \pk -> do
      discardHand pk
      drawCards pk 3
    -- Silence unused-self warning.
    let _ = self
    pure ()

nurglesPestilence :: CardDef Tactic
nurglesPestilence = tacticCard "core-101" "Nurgle's Pestilence" do
  race Chaos
  cost 3
  loyalty 2
  trait Spell
  body "Action: Each unit in play takes 1 damage. Corrupted units take an additional damage."
  whenResolved \_ -> do
    g <- getGame
    for_ g.units \u -> dealDamage u.key (if u.corrupted then 2 else 1)

flamesOfTzeentch :: CardDef Tactic
flamesOfTzeentch = tacticCard "core-102" "Flames of Tzeentch" do
  race Chaos
  costVariable
  loyalty 3
  trait Spell
  body "Play during your turn. Action: Deal X damage to one target unit."
  whenResolved \self ->
    when (self.xValue > 0) $
      withTarget self.controller AnyUnit \k -> dealDamage k self.xValue

bloodForTheBloodGod :: CardDef Tactic
bloodForTheBloodGod = tacticCard "core-103" "Blood for the Blood God" do
  race Chaos
  cost 2
  loyalty 2
  body "Action: Choose a target unit in any battlefield. Deal damage to that unit equal to its power."
  whenResolved \self ->
    withTarget self.controller
      (UnitMatching \_ _ u -> u.zone == BattlefieldZone)
      \k ->
        withUnit k \u -> dealDamage k u.effectivePower

cullingTheWeak :: CardDef Tactic
cullingTheWeak = tacticCard "core-104" "Culling the Weak" do
  race Chaos
  cost 2
  loyalty 1
  body "Action: Sacrifice a unit to have all units in your battlefield gain {power} until the end of the turn."
  playableWhen \g pk -> any (\u -> u.controller == pk) g.units
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk ownUnit \k -> do
      destroyUnit k
      g <- getGame
      let mine =
            [ u | u <- g.units
                , u.controller == pk
                , u.zone == BattlefieldZone
            ]
      for_ mine \u -> until EndOfTurn $ buffPower u.key 1

slaaneshsDomination :: CardDef Tactic
slaaneshsDomination = tacticCard "core-105" "Slaanesh's Domination" do
  race Chaos
  cost 2
  loyalty 2
  body "Action: Reveal up to three cards at random from one target opponent's hand. You may play any tactics thus revealed as though they were in your hand for no cost."
  -- Engine-side handler does the random shuffle (no 'MonadRandom'
  -- in TriggerM) and the per-tactic opt-in prompt loop. Cards stay
  -- in the opponent's hand; only the effects fire on the
  -- controller's behalf.
  whenResolved \self ->
    push (SlaaneshDominate self.controller self.controller.next 3)

-- The Corruption cycle -------------------------------------------------

chosenOfTzeentch :: CardDef Unit
chosenOfTzeentch = unitCard "the-skavenblight-threat-010" "Chosen of Tzeentch" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Sorceror
  body "Quest. Action: Spend 1 resource to deal 1 damage to one target damaged unit (limit once per turn)."
  quest $ action "Feed on wounds" 1 \usage -> do
    g <- getGame
    let used =
          any (\m -> m.details == ActionUsedThisTurn)
            (Map.findWithDefault [] (UnitRef usage.self.key) g.modifiers)
    unless used do
      until EndOfTurn (PendingBuff usage.self.key ActionUsedThisTurn)
      withTarget usage.user (unitWhere isDamaged) \k -> dealDamage k 1

boltOfChange :: CardDef Tactic
boltOfChange = tacticCard "the-skavenblight-threat-011" "Bolt of Change" do
  race Chaos
  cost 1
  loyalty 2
  trait Spell
  body
    "Action: Until the end of the turn, one target development becomes a unit with 2 hit \
    \points and {power}{power}. It also counts as a development."
  playableWhen $ hasTarget AnyDevelopmentZone
  whenResolved \self ->
    withTarget self.controller AnyDevelopmentZone \(owner, zk) ->
      push (AnimateDevelopment owner zk 2 2)

buleLordOfPus :: CardDef Unit
buleLordOfPus = unitCard "path-of-the-zealot-030" "Bule, Lord of Pus" do
  hero
  race Chaos
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  body "Limit one Hero per zone. Forced: At the beginning of your turn, corrupt one target unit."
  onMyTurnBegin \_owner self -> do
    g <- getGame
    let candidates = [u.key | u <- g.units, not u.corrupted]
    forcePickUnit self.controller candidates
      "Bule, Lord of Pus: corrupt one target unit."
      corrupt

blueHorrors :: CardDef Unit
blueHorrors = unitCard "tooth-and-claw-051" "Blue Horrors" do
  race Chaos
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Daemon
  body
    "Action: When this unit leaves play, you may put another unit named Blue Horrors into \
    \play from your hand. (That unit may be played into any zone.)"
  onSelfLeavesPlay \_owner self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    let copies =
          [ c
          | c <- me.hand
          , Just cd <- [asUnit c.def]
          , cd.code == self.cardDef.code
          ]
    for_ (take 1 copies) \c ->
      may pk "Put another Blue Horrors into play from your hand?" $
        withTarget pk MyAnyZone \zk ->
          putUnitIntoPlay pk FromHand c.key zk

brutalOffering :: CardDef Tactic
brutalOffering = tacticCard "tooth-and-claw-052" "Brutal Offering" do
  race Chaos
  cost 2
  loyalty 1
  body
    "Action: Sacrifice a unit. If you do, deal X damage to each unit in all battlefields, \
    \where X is the sacrificed unit's power."
  playableWhen \g pk -> any (\u -> u.controller == pk) g.units
  whenResolved \self ->
    sacrificeOwnUnit self.controller "Sacrifice a unit." \k -> do
      g <- getGame
      -- The sacrifice is still queued, so the unit is readable for
      -- its power and excluded from the blast by key.
      whenJust (findUnit k g) \sacrificed -> do
        let x = sacrificed.effectivePower
            targets =
              [ u.key
              | u <- g.units
              , u.zone == BattlefieldZone
              , u.key /= k
              ]
        when (x > 0) $ for_ targets \t -> dealDamage t x

greatUncleanOne :: CardDef Unit
greatUncleanOne = unitCard "the-deathmaster-s-dance-073" "Great Unclean One" do
  race Chaos
  cost 6
  loyalty 4
  power 4
  hitPoints 6
  trait Daemon
  body "Battlefield. Action: Sacrifice a corrupt unit to give this unit {power} until the end of the turn."
  battlefield $ action "Consume the corrupt" 0 \usage -> do
    g <- getGame
    let corrupt =
          [ u.key
          | u <- g.units
          , u.controller == usage.user
          , u.corrupted
          ]
    forcePickUnit usage.user corrupt "Sacrifice a corrupt unit." \k -> do
      destroyUnit k
      until EndOfTurn $ buffPower usage.self.key 1

hellcannonReserves :: CardDef Support
hellcannonReserves = supportCard "the-deathmaster-s-dance-074" "Hellcannon Reserves" do
  race Chaos
  cost 4
  loyalty 2
  power 1
  trait Siege
  body
    "Kingdom. Whenever a tactic you play deals damage to one or more targets, deal an \
    \additional damage to each target."
  tacticDamageBoost \_g s pk ->
    if pk == s.controller && s.zone == KingdomZone then 1 else 0

offeringOfBlood :: CardDef Tactic
offeringOfBlood = tacticCard "the-deathmaster-s-dance-075" "Offering of Blood" do
  race Chaos
  cost 0
  loyalty 2
  body "Action: Sacrifice a unit. If you do, deal 1 damage to each section of each opponent's capital."
  playableWhen \g pk -> any (\u -> u.controller == pk) g.units
  whenResolved \self ->
    sacrificeOwnUnit self.controller "Sacrifice a unit." \_k ->
      for_ [KingdomZone, QuestZone, BattlefieldZone] \zk ->
        dealZoneDamage self.controller.next zk 1

alluringDaemonettes :: CardDef Unit
alluringDaemonettes = unitCard "the-warpstone-chronicles-093" "Alluring Daemonettes" do
  race Chaos
  cost 5
  loyalty 1
  power 2
  hitPoints 3
  trait Daemon
  body "Action: When this unit attacks, target unit must defend if able."
  onMyAttackDeclared \_owner self zone _attackers ->
    may self.controller "Alluring Daemonettes: force a unit to defend?" $
      withTarget self.controller
        (UnitMatching \_ _ u ->
          u.controller == self.controller.next && u.zone == zone)
        \k -> until EndOfTurn $ mustDefend k

beastOfChaos :: CardDef Unit
beastOfChaos = unitCard "arcane-fire-113" "Beast of Chaos" do
  race Chaos
  cost 4
  loyalty 3
  power 3
  hitPoints 1
  trait Creature
  body "Battlefield only."
  battlefieldOnly

cacophonicScream :: CardDef Tactic
cacophonicScream = tacticCard "arcane-fire-114" "Cacophonic Scream" do
  race Chaos
  cost 10
  loyalty 3
  traits [Epic, Spell]
  body "Play at the beginning of your turn. Action: Deal 8 damage to one section of target capital (you choose which section)."
  playableWhen \g pk ->
    g.currentPlayer == pk
      && case g.actionWindow of
        Just aw -> aw.trigger == BeginningOfTurnActionWindow
        Nothing -> False
  whenResolved \self ->
    withTarget self.controller AnyCapital \(owner, zk) ->
      dealZoneDamage owner zk 8

blessingsOfTzeentch :: CardDef Tactic
blessingsOfTzeentch = tacticCard "arcane-fire-115" "Blessings of Tzeentch" do
  race Chaos
  cost 3
  loyalty 1
  trait Spell
  body
    "Action: Sacrifice a unit. If you do, you may search the top five cards of your deck \
    \for any number of units and put one of them into play at random (you choose which \
    \zone). Then, shuffle the other cards back into your deck."
  playableWhen \g pk -> any (\u -> u.controller == pk) g.units
  whenResolved \self ->
    sacrificeOwnUnit self.controller "Sacrifice a unit." \_k ->
      push (PutRandomUnitIntoPlayFromDeckTop self.controller 5)

-- Cataclysm cycle ------------------------------------------------------

lordOfKhorne :: CardDef Unit
lordOfKhorne = unitCard "cataclysm-033" "Lord of Khorne" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Warrior
  body "This unit deals +1 damage in combat for each burning zone."
  combatPower \g _u -> burningZoneCount g

bloodcrusher :: CardDef Unit
bloodcrusher = unitCard "cataclysm-034" "Bloodcrusher" do
  race Chaos
  cost 5
  loyalty 3
  power 3
  hitPoints 5
  trait Daemon
  body "Lower the cost to play this unit by 1 for each burning zone."
  selfCostAdjust \g _pk -> negate (burningZoneCount g)

riftOfChaos :: CardDef Support
riftOfChaos = supportCard "cataclysm-037" "Rift of Chaos" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  trait Rift
  body "This card gains {power} for each burning zone."
  zonePowerAura \g s zone ->
    if s.zone == zone then burningZoneCount g else 0

-- Fragments of Power ---------------------------------------------------

swornOfKhorne :: CardDef Unit
swornOfKhorne = unitCard "fragments-of-power-031" "Sworn of Khorne" do
  race Chaos
  cost 2
  loyalty 1
  power 3
  hitPoints 1
  trait Warrior
  keyword BattlefieldOnly
  body "Battlefield only. This unit cannot attack unless the defending zone has at least 1 corrupted unit."
  canAttack \g defender zone _u ->
    any
      ( \v ->
          v.controller == defender
            && v.zone == zone
            && v.corrupted
      )
      g.units

-- Faith and Steel ------------------------------------------------------

skulltaker :: CardDef Unit
skulltaker = unitCard "faith-and-steel-113" "Skulltaker" do
  unique
  race Chaos
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  trait Daemon
  body
    "This unit gains {power} for each experience attached to it. \
    \Action: When an opponent's unit leaves play, spend 1 resource to attach it facedown to this unit as an experience."
  selfPower \_g u -> length u.experiences
  onOpponentUnitLeavePlay \_owner self _uk _zone code ->
    mayPay self.controller 1
      "Spend 1 resource to attach the departing unit as an experience on Skulltaker?" $
        attachExperience self.key code

-- Days of Blood --------------------------------------------------------

recklessAttack :: CardDef Tactic
recklessAttack = tacticCard "days-of-blood-018" "Reckless Attack" do
  race Chaos
  cost 1
  loyalty 2
  keyword Limited
  body
    "Limited. Action: When your opponent declares at least 1 defender against your attack, \
    \put target unit in your discard pile into play in your battlefield declared as an attacker. \
    \At the end of the phase, sacrifice all units that attacked this phase."
  playableWhen \g pk ->
    isMyAttackWithDefenders g pk
      && any isUnitCard (playerOf pk g).discard
  whenResolved \self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    chooseFromCards pk 1 1 (filter isUnitCard me.discard)
      "Choose a unit in your discard pile to put into your battlefield." \chosen ->
        for_ chosen \c -> do
          putUnitIntoPlay pk FromDiscard c.key BattlefieldZone
          push ScheduleAttackerSacrifice
  where
    isUnitCard c = case c.def of
      UnitCardDef _ -> True
      _ -> False
    isMyAttackWithDefenders g pk = case g.combat of
      Just cs -> cs.attackingPlayer == pk && not (null cs.defenders)
      _ -> False

-- Legends --------------------------------------------------------------

archaon :: CardDef Legend
archaon = legendCard "legends-029" "Archaon" do
  race Chaos
  cost 7
  loyalty 5
  legendPower 3 3 3
  hitPoints 5
  body "Action: When Archaon attacks, spend X resources to deal X damage to target unit."
  onMyAttackDeclared \owner self _zone _attackers -> do
    let Resources avail = owner.resources
    when (avail > 0) $
      withTarget self.controller AnyUnit \tgt -> do
        x <- chooseAmount self.controller 0 avail
          "Archaon: spend X resources to deal X damage."
        when (x > 0) $ do
          payResources self.controller x
          dealDamage tgt x

sigvaldTheMagnificent :: CardDef Legend
sigvaldTheMagnificent = legendCard "the-ruinous-hordes-081" "Sigvald the Magnificent" do
  race Chaos
  cost 3
  loyalty 2
  legendPower 1 1 2
  hitPoints 3
  body
    "Forced: When this legend enters play, you must burn 3 zones instead of \
    \2 in order to win for the rest of the game. Action: When this legend \
    \attacks, discard the top card of your deck. This legend deals +X damage \
    \in combat until the end of the phase. X is the discarded card's cost."
  onEnterPlay \_owner self -> push (RequireBurnThreeToWin self.controller)
  onMyAttackDeclared \owner self _zone _attackers ->
    case owner.deck of
      [] -> pure ()
      (top : _) -> do
        millFromDeck self.controller 1
        until EndOfTurn $ buffPower self.key (someCardCost top.def)

skarbrand :: CardDef Legend
skarbrand = legendCard "portent-of-doom-082" "Skarbrand" do
  race Chaos
  cost 5
  loyalty 4
  legendPower 2 2 2
  hitPoints 5
  body
    "Forced: When a unit leaves play, each player must deal 1 damage to his \
    \capital or 1 damage to a legend he controls."
  onReceive $ Receive \msg _owner _self -> case msg of
    UnitLeftPlay _ ->
      for_ [Player1, Player2] \pk -> do
        g <- getGame
        case legendOf pk g of
          Just leg -> do
            hitLegend <- askYesNo pk
              "Skarbrand: deal 1 damage to your legend? (otherwise to your capital)"
            if hitLegend
              then push (DealDamageToLegend leg.key 1)
              else withTarget pk MyAnyZone \z -> push (DealDamageToZone pk z 1)
          Nothing ->
            withTarget pk MyAnyZone \z -> push (DealDamageToZone pk z 1)
    _ -> pure ()

bloodletter :: CardDef Unit
bloodletter = unitCard "legends-031" "Bloodletter" do
  race Chaos
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  trait Daemon
  body "Double all damage assigned to units as it is being assigned."
  damageMultiplier 2

warhounds :: CardDef Unit
warhounds = unitCard "legends-032" "Warhounds" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Creature
  body
    "Action: When this unit enters play, reveal a {chaos} legend or unit from your hand. \
    \If you do, deal 2 damage to target unit in any corresponding zone."
  onEnterPlay \_owner self ->
    revealFromHand self.controller chaosLegendOrUnit
      "Reveal a Chaos legend or unit to deal 2 damage." \_revealed ->
        withTarget self.controller (unitWhere \u -> u.zone == self.zone) \k ->
          dealDamage k 2
  where
    chaosLegendOrUnit c = case c.def of
      UnitCardDef cd -> Chaos `elem` cd.races
      LegendCardDef cd -> Chaos `elem` cd.races
      _ -> False

marauderChieftain :: CardDef Unit
marauderChieftain = unitCard "legends-033" "Marauder Chieftain" do
  race Chaos
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Warrior
  body
    "Action: At the beginning of your turn, deal 1 damage to target capital \
    \(you choose which zone)."
  onMyTurnBegin \_owner self ->
    withTarget self.controller AnyCapital \(owner, zk) ->
      dealZoneDamage owner zk 1

seductiveDelusion :: CardDef Tactic
seductiveDelusion = tacticCard "legends-035" "Seductive Delusion" do
  race Chaos
  cost 2
  loyalty 2
  trait Spell
  body
    "Action: Choose a target unit. Corrupt that unit. Then, that unit deals \
    \damage equal to its power to another target unit."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \k1 -> do
      corrupt k1
      g <- getGame
      whenJust (findUnit k1 g) \u1 ->
        withTarget pk (unitWhere \u -> u.key /= k1) \k2 ->
          dealDamage k2 u1.effectivePower

-- Path of the Zealot ---------------------------------------------------

bloodsworn :: CardDef Unit
bloodsworn = unitCard "path-of-the-zealot-031" "Bloodsworn" do
  race Chaos
  cost 4
  loyalty 1
  power 2
  hitPoints 3
  trait Warrior
  body "Forced: When an opponent's unit enters a discard pile from play, heal all damage on Bloodsworn."
  -- Heal all damage. A large constant beats threading the current HP
  -- into the message; 'HealUnit' clamps to 0.
  onOpponentUnitLeavePlay \_owner self _uk _zone _code ->
    healUnit self.key 999

wolvesOfTheNorth :: CardDef Quest
wolvesOfTheNorth = questCard "path-of-the-zealot-032" "Wolves of the North" do
  unique
  race Chaos
  cost 0
  loyalty 2
  trait QuestTrait
  body
    "Action: During your quest phase, the unit questing on this card can initiate a single attack against a single zone controlled by an opponent."
  actionEnemyZone "Out-of-phase attack" 0 \u (_, z) ->
    withQuest u.self.key \q ->
      for_ q.questingUnit \attackerKey ->
        push $ BeginCombat u.user z [attackerKey]

-- The Fourth Waystone --------------------------------------------------

viciousMarauder :: CardDef Unit
viciousMarauder = unitCard "the-fourth-waystone-091" "Vicious Marauder" do
  race Chaos
  cost 3
  loyalty 1
  power 2
  hitPoints 2
  trait Warrior
  keyword BattlefieldOnly
  body "Battlefield only. This unit must attack during your battlefield phase, if able."
  -- Approximation: when the battlefield action window opens for the
  -- controller's turn, auto-initiate an attack on the opposing
  -- battlefield with every friendly unit in that zone (the marauder
  -- itself plus any others) — strictly, the rules force only the
  -- marauder, but auto-batching with others keeps the line of play
  -- coherent until action prompts exist.
  onActionWindow BattlefieldActionWindow \_owner self -> do
    g <- getGame
    when (g.currentPlayer == self.controller && self.zone == BattlefieldZone) do
      let attackers =
            [ u.key
            | u <- g.units
            , u.controller == self.controller
            , u.zone == BattlefieldZone
            , not u.corrupted
            ]
      unless (null attackers) $
        push $ BeginCombat self.controller BattlefieldZone attackers

-- The Chaos Moon -------------------------------------------------------

doombull :: CardDef Unit
doombull = unitCard "the-chaos-moon-032" "Doombull" do
  race Chaos
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Action: When this unit leaves play, deal 4 damage to target unit in any corresponding zone."
  onSelfDestroyed \_owner self ->
    withTarget self.controller (unitWhere \u -> u.zone == self.zone) \k ->
      dealDamage k 4

-- The Warpstone Chronicles ---------------------------------------------

berserkFury :: CardDef Tactic
berserkFury = tacticCard "the-warpstone-chronicles-094" "Berserk Fury" do
  race Chaos
  cost 2
  loyalty 3
  body
    "Action: One target Unit gains 3 Power until the end of the turn. At the end of the turn, that unit takes 2 damage."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> do
      until EndOfTurn $ buffPower k 3
      queueEoTDamage k 2

daemonsword :: CardDef Support
daemonsword = supportCard "the-warpstone-chronicles-095" "Daemonsword" do
  unique
  race Chaos
  cost 2
  loyalty 1
  traits [Attachment, Relic]
  body
    "Attach to a target {chaos} unit. Corrupt that unit. \
    \Attached unit gains 3 Power and gets +2 Hit Points."
  attachmentPower 3
  attachmentHp 2
  onEnterPlay \_owner self -> for_ self.attachedTo corrupt

-- The Eclipse of Hope --------------------------------------------------

brandedByKhorne :: CardDef Support
brandedByKhorne = supportCard "the-eclipse-of-hope-093" "Branded by Khorne" do
  race Chaos
  cost 0
  loyalty 2
  trait Attachment
  body "Attach to a target unit. If attached unit is damaged, destroy that unit."
  onHostDamaged \_owner _self hostKey _n -> destroyUnit hostKey

unleashingTheSpell :: CardDef Tactic
unleashingTheSpell = tacticCard "the-eclipse-of-hope-094" "Unleashing the Spell" do
  race Chaos
  cost 3
  loyalty 3
  trait Spell
  body "Action: Put the top card of your deck into play facedown as a development. Then, sacrifice X developments to deal X damage to target unit or capital."
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk MyDevZone \zone -> placeTopAsDevelopments pk zone 1
    me <- playerOf pk <$> getGame
    -- X is capped at developments physically present now; the freshly
    -- placed one resolves after this action, so it cannot be sacrificed
    -- within the same resolution.
    x <- chooseAmount pk 0 (developmentsControlled me) "Sacrifice how many developments?"
    when (x > 0) do
      replicateM_ x $ withTarget pk MyDevZone \zone -> destroyDevelopment pk zone
      withTarget pk (AnyUnit `Or` AnyCapital) \case
        TargetUnitOption u -> dealDamage u x
        TargetZoneOption owner z -> dealZoneDamage owner z x

-- Omens of Ruin --------------------------------------------------------

markOfChaos :: CardDef Support
markOfChaos = supportCard "omens-of-ruin-013" "Mark of Chaos" do
  race Chaos
  cost 1
  loyalty 2
  traits [Attachment, Spell]
  body
    "Attach to a target unit. Attached unit gains {power}{power}. \
    \Forced: At the beginning of your turn, attached unit takes 1 uncancellable damage."
  attachmentPower 2
  -- The +2 power half waits on dynamic modifiers; for now wire the
  -- turn-start damage tick on the host's controller's turn.
  onAttachedHostTurnBegin \_owner _self host ->
    dealUncancellableDamage host.key 1

-- The Ruinous Hordes ---------------------------------------------------

northernWastes :: CardDef Support
northernWastes = supportCard "the-ruinous-hordes-083" "Northern Wastes" do
  race Chaos
  cost 1
  loyalty 1
  power 1
  trait Wasteland
  body "If you control a faceup non-{chaos} unit or support card, sacrifice this card."
  -- Self-check on every message tick: if controller has any faceup
  -- non-Chaos unit or support, sacrifice. (Attachments inherit their
  -- host's faceup status; we don't track facedown explicitly so every
  -- in-play card is treated as faceup for this check.)
  onAnyMessage \_owner self -> do
    g <- getGame
    let nonChaosUnit u =
          u.controller == self.controller
            && Chaos `notElem` u.cardDef.races
        nonChaosSupport s =
          s.key /= self.key
            && s.controller == self.controller
            && Chaos `notElem` s.cardDef.races
    when
      ( any nonChaosUnit g.units
          || any nonChaosSupport g.supports
      )
      (destroySupport self.key)

dominionOfChaos :: CardDef Quest
dominionOfChaos = questCard "the-ruinous-hordes-082" "Dominion of Chaos" do
  race Chaos
  cost 0
  loyalty 3
  trait Mission
  keyword PlayInOpponentArea
  body
    "Play in any opponent's zone under your control. \
    \When you assign combat damage to this zone, you may place any number of that combat damage on this quest instead. \
    \Forced: When the 3rd damage token is placed here, sacrifice this quest to corrupt up to 3 target units."
  onMyQuestTokensAdjusted \_owner self _delta ->
    withQuest self.key \q -> when (q.tokens >= 3) $
      withUpTo self.controller 3 (unitWhere (not . (.corrupted))) \chosen -> do
        traverse_ corrupt chosen
        destroyQuest self.key

-- The Inevitable City --------------------------------------------------

ironThroneroom :: CardDef Support
ironThroneroom = supportCard "the-inevitable-city-013" "Iron Throneroom" do
  unique
  race Chaos
  cost 3
  loyalty 5
  power 2
  trait CapitalCenter
  body
    "This card enters play with 4 resource tokens on it. \
    \Action: At the beginning of your turn, remove a resource token from this card. \
    \Then, if there are no resource tokens on this card, put up to 3 {chaos} units into play from your hand or discard pile."
  onEnterPlay \_owner self -> adjustSupportTokens self.key 4
  onMyTurnBegin \_owner self -> when (self.tokens > 0) do
    adjustSupportTokens self.key (-1)
    when (self.tokens == 1) do
      let pk = self.controller
      me <- playerOf pk <$> getGame
      let isChaosUnit c = case c.def of
            UnitCardDef cd -> Chaos `elem` cd.races
            _ -> False
          handChaos = filter isChaosUnit me.hand
          discardChaos = filter isChaosUnit me.discard
          candidates = handChaos <> discardChaos
          inHandKeys = map (.key) handChaos
      chooseFromCards pk 0 3 candidates
        "Choose up to 3 Chaos units from your hand or discard pile to put into play." \chosen ->
          for_ chosen \c ->
            putUnitIntoPlay pk
              (if c.key `elem` inHandKeys then FromHand else FromDiscard)
              c.key KingdomZone

raidingCamps :: CardDef Quest
raidingCamps = questCard "the-inevitable-city-020" "Raiding Camps" do
  race Chaos
  cost 0
  loyalty 3
  body
    "Quest. Action: When this card enters play, draw a card. \
    \Quest. Action: When you play a {chaos} non-Attachment support card from your hand, \
    \destroy target support card in a zone with no units if a unit is questing here."
  onEnterPlay \_owner self ->
    drawCard self.controller
  -- "destroy target support card in a zone with no units": pick any
  -- in-play support whose controller's zone holds no units.
  onQuestSupportPayoff Chaos \self ->
    withTarget self.controller
      ( SupportMatching \_pk g s ->
          null [u | u <- g.units, u.controller == s.controller, u.zone == s.zone]
      )
      destroySupport

-- The Accursed Dead ----------------------------------------------------

riftOfBattle :: CardDef Support
riftOfBattle = supportCard "the-accursed-dead-052" "Rift of Battle" do
  race Chaos
  cost 1
  loyalty 2
  trait Rift
  body "Units in all corresponding zones deal +1 damage in combat."
  supportCombat \_g _s _u -> 1

ghorgon :: CardDef Unit
ghorgon = unitCard "days-of-blood-017" "Ghorgon" do
  race Chaos
  cost 2
  loyalty 2
  power 3
  hitPoints 3
  trait Creature
  battlefieldOnly
  body
    "Battlefield only. Forced: When this unit attacks, discard the top card of your deck. \
    \If the discarded card is not a unit, sacrifice this unit."
  onMyAttackDeclared \owner self _zone _attackers ->
    case owner.deck of
      [] -> pure ()
      (top : _) -> do
        millFromDeck self.controller 1
        unless (isUnitCard top) $ destroyUnit self.key
  where
    isUnitCard c = case c.def of
      UnitCardDef _ -> True
      _ -> False

-- Battle for the Old World ---------------------------------------------

norseClansman :: CardDef Unit
norseClansman = unitCard "battle-for-the-old-world-053" "Norse Clansman" do
  race Chaos
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Berserker
  body
    "Forced: When this unit is opposed in combat, discard the top card of your deck. \
    \If the discarded card is a unit, deal 1 uncancellable damage to target attacking \
    \or defending unit."
  -- "Opposed in combat" is modelled at the defender-declaration step:
  -- the unit is opposed once attackers and defenders both exist and it
  -- is on one side of the combat.
  onReceive $ Receive \msg owner self -> case msg of
    DeclareDefenders ks -> do
      g <- getGame
      let opposed = case g.combat of
            Just cs ->
              (self.key `elem` cs.attackers && not (null ks))
                || self.key `elem` ks
            Nothing -> False
      when opposed case owner.deck of
        [] -> pure ()
        (top : _) -> do
          millFromDeck self.controller 1
          when (isUnitCard top) $
            withTarget self.controller inCombatUnit \k ->
              dealUncancellableDamage k 1
    _ -> pure ()
  where
    isUnitCard c = case c.def of
      UnitCardDef _ -> True
      _ -> False
    inCombatUnit = UnitMatching \_pk g u -> case g.combat of
      Just cs -> u.key `elem` cs.attackers || u.key `elem` cs.defenders
      Nothing -> False

-- Glory of Days Past ---------------------------------------------------

chaosDragon :: CardDef Unit
chaosDragon = unitCard "glory-of-days-past-073" "Chaos Dragon" do
  race Chaos
  cost 7
  loyalty 3
  power 5
  hitPoints 5
  traits [Creature, Dragon]
  battlefieldOnly
  body
    "Battlefield only. Action: When this unit attacks, discard the top card of your deck. \
    \If the discarded card is a unit, corrupt target unit in the defending zone."
  onMyAttackDeclared \owner self zone _attackers ->
    case owner.deck of
      [] -> pure ()
      (top : _) -> do
        millFromDeck self.controller 1
        when (isChaosDragonUnit top) $
          withTarget self.controller
            (UnitMatching \_pk _g u -> u.zone == zone && u.controller /= self.controller)
            corrupt
  where
    isChaosDragonUnit c = case c.def of
      UnitCardDef _ -> True
      _ -> False

painfulMutation :: CardDef Support
painfulMutation = supportCard "glory-of-days-past-075" "Painful Mutation" do
  race Chaos
  cost 1
  loyalty 2
  traits [Attachment, Mutation]
  body
    "Attach to a target unit in any battlefield zone. Attached unit gains {power}{power}. \
    \Forced: At the beginning of your turn, deal X indirect damage to attached unit's \
    \controller. X is attached unit's total {power}."
  attachmentPower 2
  onMyTurnBegin \_owner self ->
    for_ self.attachedTo \hostKey -> do
      g <- getGame
      whenJust (findUnit hostKey g) \host ->
        when (host.effectivePower > 0) $
          indirectDamage host.controller host.effectivePower

norseMarauders :: CardDef Unit
norseMarauders = unitCard "days-of-blood-016" "Norse Marauders" do
  race Chaos
  cost 4
  loyalty 1
  power 2
  hitPoints 3
  trait Warrior
  raider 3
  body "Raider 3."

-- Bloodquest: Rising Dawn -----------------------------------------------

boonOfTzeentch :: CardDef Tactic
boonOfTzeentch = tacticCard "rising-dawn-013" "Boon of Tzeentch" do
  race Chaos
  cost 2
  loyalty 3
  trait Spell
  body
    "Action: Discard the top card of your deck. Gain resources equal to the printed cost of \
    \the discarded card."
  whenResolved \self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    case me.deck of
      [] -> pure ()
      (top : _) -> do
        millFromDeck pk 1
        let c = someCardCost top.def
        when (c > 0) $ gainResources pk c

-- Bloodquest: Fragments of Power ----------------------------------------

stolenSkin :: CardDef Support
stolenSkin = supportCard "fragments-of-power-032" "Stolen Skin" do
  race Chaos
  cost 0
  loyalty 2
  trait Attachment
  body
    "Attach to a target [Chaos] unit. Attached unit gains Toughness 1. If attached unit \
    \survives combat, heal all damage on it."
  supportToughnessAura \_g self u -> if self.attachedTo == Just u.key then 1 else 0
  onReceive $ Receive \msg _owner self -> case msg of
    CombatResolved ->
      for_ self.attachedTo \hostKey -> do
        g <- getGame
        case g.combat of
          Just cs
            | hostKey `elem` (cs.attackers <> cs.defenders) ->
                whenJust (findUnit hostKey g) \host ->
                  let Damage d = host.damage in when (d > 0) $ healUnit hostKey d
          _ -> pure ()
    _ -> pure ()

-- Bloodquest: The Accursed Dead -----------------------------------------

strickenWarrior :: CardDef Unit
strickenWarrior = unitCard "the-accursed-dead-051" "Stricken Warrior" do
  race Chaos
  cost 2
  loyalty 1
  power 0
  hitPoints 4
  trait Warrior
  body
    "Forced: When this unit is opposed in combat, deal 1 damage to each other participating \
    \unit."
  onReceive $ Receive \msg _owner self -> case msg of
    DeclareDefenders ks -> do
      g <- getGame
      case g.combat of
        Just cs
          | (self.key `elem` cs.attackers && not (null ks)) || self.key `elem` ks ->
              for_ (filter (/= self.key) (cs.attackers <> ks)) \k -> dealDamage k 1
        _ -> pure ()
    _ -> pure ()

-- Bloodquest: Shield of the Gods ----------------------------------------

necrodomosProphecy :: CardDef Tactic
necrodomosProphecy = tacticCard "shield-of-the-gods-112" "Necrodomo's Prophecy" do
  race Chaos
  cost 0
  loyalty 5
  body "Action: Sacrifice a unit to search your deck for a [Chaos] card, reveal it, and shuffle your deck. Then, put that card on top of your deck."
  playableWhen \g pk -> any (\u -> u.controller == pk) g.units
  whenResolved \self -> do
    let pk = self.controller
    sacrificeOwnUnit pk "Sacrifice a unit to search your deck for a Chaos card." \_ ->
      searchWholeDeck pk \result -> do
        let chaos = [c | c <- result.cards, isRace c.def Chaos]
        chooseFromCards pk 0 1 chaos
          "Choose a Chaos card to reveal and put on top of your deck." \chosen -> do
            push (RevealCards pk chosen)
            shuffleDeck pk
            arrangeDeckCards pk (map (.key) chosen) []

swordsOfChaos :: CardDef Unit
swordsOfChaos = unitCard "portent-of-doom-094" "Swords of Chaos" do
  race Chaos
  cost 4
  loyalty 3
  power 2
  hitPoints 3
  battlefieldOnly
  toughness 2
  body
    "Battlefield only. Toughness 2. Bodyguard. This unit can attack or \
    \defend (from any zone) whenever a [Chaos] legend you control attacks \
    \or defends."
  bodyguardForLegend Chaos

-- The Capital Cycle ----------------------------------------------------

savageForsaken :: CardDef Unit
savageForsaken = unitCard "the-inevitable-city-008" "Savage Forsaken" do
  race Chaos
  cost 3
  loyalty 1
  power 0
  hitPoints 2
  traits [Warrior, Elite]
  body
    "This unit deals +X damage in combat while attacking. X is the highest \
    \loyalty on a {chaos} card you control."
  combatPower \g u ->
    if unitIsAttacking g u then highestLoyaltyControlled Chaos g u.controller else 0

theBleedingWall :: CardDef Support
theBleedingWall = supportCard "the-inevitable-city-011" "The Bleeding Wall" do
  race Chaos
  cost 2
  loyalty 2
  power 1
  trait Location
  body
    "Action: When a {chaos} unit you control is corrupted, put a resource token on this \
    \card. Action: Remove 2 resource tokens from this card to destroy target corrupted unit."
  onUnitCorrupted \_owner self uk -> do
    g <- getGame
    whenJust (findUnit uk g) \u ->
      when (u.controller == self.controller && Chaos `elem` u.cardDef.races) $
        adjustSupportTokens self.key 1
  action "Bleed the corrupted" 0 \usage -> do
    g <- getGame
    whenJust (findSupport usage.self.key g) \s ->
      when (s.tokens >= 2 && any (.corrupted) g.units) do
        adjustSupportTokens usage.self.key (-2)
        withTarget usage.user (UnitMatching \_ _ u -> u.corrupted) destroyUnit

daemonPrince :: CardDef Unit
daemonPrince = unitCard "the-imperial-throne-114" "Daemon Prince" do
  race Chaos
  cost 0
  loyalty 3
  power 4
  hitPoints 4
  trait Daemon
  battlefieldOnly
  body
    "Battlefield only. This unit cannot be declared as an attacker or defender \
    \unless it has exactly 3 resource tokens on it. Action: Sacrifice a unit to put \
    \a resource token on this unit."
  canAttack \_g _pk _zone u -> u.tokens == 3
  canDefend \_g _pk _zone u -> u.tokens == 3
  action "Empower" 0 \usage ->
    sacrificeOwnUnit usage.user "Sacrifice a unit to empower the Daemon Prince." \_k ->
      adjustUnitTokens usage.self.key 1

soporificMusk :: CardDef Tactic
soporificMusk = tacticCard "city-of-winter-085" "Soporific Musk" do
  race Chaos
  cost 1
  loyalty 2
  body
    "Action: Target corrupted unit loses all power until the end of the turn. \
    \Then, you may put this card on top of your deck."
  whenResolved \self -> do
    withTarget self.controller (unitWhere (.corrupted)) \k ->
      until EndOfTurn $ losesAllPower k
    mayReturnToTopOfDeck self.controller self.cardDef.code

pinkHorror :: CardDef Unit
pinkHorror = unitCard "the-inevitable-city-007" "Pink Horror" do
  race Chaos
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Daemon
  body
    "Action: When this unit enters play, put a card named Pink Horror or Blue \
    \Horrors into play from your hand."
  onEnterPlay \_owner self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    let candidates =
          [ c
          | c <- me.hand
          , Just cd <- [asUnit c.def]
          , cd.title `elem` ["Pink Horror", "Blue Horrors"]
          ]
    unless (null candidates) $
      may pk "Put a Pink Horror or Blue Horrors into play from your hand?" $
        chooseFromCards pk 1 1 candidates "Choose a unit to put into play." \case
          (c : _) -> withTarget pk MyAnyZone \zk -> putUnitIntoPlay pk FromHand c.key zk
          _ -> pure ()

screamersOfTzeentch :: CardDef Unit
screamersOfTzeentch = unitCard "the-inevitable-city-010" "Screamers of Tzeentch" do
  race Chaos
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Creature
  feared 1
  body
    "Feared 1 (while this unit is attacking, blank the text box of 1 target unit \
    \except for Traits). Forced: At the beginning of your turn, corrupt this unit."
  -- Feared 1: while attacking, blank one target unit's text box.
  onMyAttackDeclared \_owner self _z _atk ->
    withTarget self.controller AnyUnit \k ->
      until EndOfTurn $ blankUnit k
  onMyTurnBegin \_owner self -> corrupt self.key

wallOfMaggots :: CardDef Support
wallOfMaggots = supportCard "the-inevitable-city-012" "Wall of Maggots" do
  race Chaos
  cost 1
  loyalty 1
  trait Fortification
  body
    "Action: When this zone is attacked, corrupt target attacking unit (this does \
    \not prevent the unit from attacking)."
  onMyZoneAttacked \_owner _self cs ->
    unless (null cs.attackers) $
      withTarget cs.defendingPlayer
        (UnitMatching \_me _g u -> u.key `elem` cs.attackers)
        corrupt

beastmanShaman :: CardDef Unit
beastmanShaman = unitCard "the-iron-rock-054" "Beastman Shaman" do
  race Chaos
  cost 3
  loyalty 3
  power 2
  hitPoints 1
  trait Sorceror
  body "Action: When a {chaos} unit you control is corrupted, draw a card."
  onUnitCorrupted \_owner self uk -> do
    g <- getGame
    whenJust (findUnit uk g) \u ->
      when (u.controller == self.controller && Chaos `elem` u.cardDef.races) $
        drawCard self.controller

stormOfChange :: CardDef Tactic
stormOfChange = tacticCard "the-inevitable-city-014" "Storm of Change" do
  race Chaos
  cost 3
  loyalty 2
  trait Spell
  body
    "Action: Discard a card from your hand with X loyalty to deal X damage to each \
    \corrupted unit."
  playableWhen \g pk -> not (null (playerOf pk g).hand)
  whenResolved \self ->
    discardForLoyalty self.controller \x -> when (x > 0) do
      g <- getGame
      for_ [u | u <- g.units, u.corrupted] \u -> dealDamage u.key x

doomBearer :: CardDef Unit
doomBearer = unitCard "the-inevitable-city-005" "Doom Bearer" do
  race Chaos
  cost 2
  loyalty 1
  power 0
  hitPoints 2
  trait StandardBearer
  body "Action: When a unit enters this zone, corrupt target unit you control."
  onUnitEnterMyZone \_owner self _uk ->
    withTarget self.controller ownUnit (push . CorruptUnit)

-- The Morrslieb cycle (additional) -------------------------------------

desecratedTemple :: CardDef Support
desecratedTemple = supportCard "the-chaos-moon-033" "Desecrated Temple" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Action: When a [Chaos] unit you control leaves play, destroy target development."
  forced \self -> onUnitOfLeavesPlay self.controller \du ->
    when (Chaos `elem` du.cardDef.races) $
      withTarget self.controller AnyDevelopmentZone \(o, z) -> destroyDevelopment o z

plagueBomb :: CardDef Tactic
plagueBomb = tacticCard "the-chaos-moon-034" "Plague Bomb" do
  race Chaos
  cost 3
  loyalty 3
  traits [Spell, Disease]
  body
    "Action: Deal 1 damage to target unit. Deal 2 damage to another target unit. \
    \Deal 3 damage to a third target unit."
  -- TODO: approximation — does not enforce that the three targets are
  -- distinct ("another"/"a third" target unit). The same unit can be
  -- chosen for more than one prompt until the target picker supports
  -- excluding already-chosen units.
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \k -> dealDamage k 1
    withTarget pk AnyUnit \k -> dealDamage k 2
    withTarget pk AnyUnit \k -> dealDamage k 3

ungorRaiders :: CardDef Unit
ungorRaiders = unitCard "omens-of-ruin-012" "Ungor Raiders" do
  race Chaos
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Action: When this unit leaves play, destroy target development."
  onSelfDestroyed \_owner self ->
    withTarget self.controller AnyDevelopmentZone \(o, z) -> destroyDevelopment o z

centigor :: CardDef Unit
centigor = unitCard "the-twin-tailed-comet-052" "Centigor" do
  race Chaos
  cost 4
  loyalty 2
  power 3
  hitPoints 3
  trait Warrior
  body "Forced: At the beginning of your turn, sacrifice a development or this unit deals 1 damage to each section of your capital."
  onMyTurnBegin \_owner self -> do
    g <- getGame
    let pk = self.controller
        p = playerOf pk g
        devZones =
          [ zk
          | (zk, Developments n) <-
              [ (KingdomZone, p.capital.kingdom.developments)
              , (QuestZone, p.capital.quest.developments)
              , (BattlefieldZone, p.capital.battlefield.developments)
              ]
          , n > 0
          ]
        dealAll = for_ [KingdomZone, QuestZone, BattlefieldZone] \zk -> dealZoneDamage pk zk 1
    case devZones of
      [] -> dealAll
      _ -> do
        sac <- askYesNo pk "Sacrifice a development instead of dealing 1 damage to each capital section?"
        if sac
          then withTarget pk (CapitalMatching \_ (owner, zk) -> owner == pk && zk `elem` devZones) \(owner, zk) ->
            destroyDevelopment owner zk
          else dealAll

sorcererOfTzeentch :: CardDef Unit
sorcererOfTzeentch = unitCard "the-twin-tailed-comet-053" "Sorcerer of Tzeentch" do
  race Chaos
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Sorceror
  body
    "Action: When you play a development from your hand, put a resource token on \
    \this unit. Then, deal X damage to target unit. X is the number of resource \
    \tokens on this unit."
  onYouPlayDevelopment \_owner self -> do
    push (AdjustUnitTokens self.key 1)
    let n = self.tokens + 1
    withTarget self.controller AnyUnit \k -> dealDamage k n

-- The Enemy cycle -------------------------------------------------------

wingedFury :: CardDef Unit
wingedFury = unitCard "the-burning-of-derricksburg-013" "Winged Fury" do
  race Chaos
  cost 5
  loyalty 2
  power 2
  hitPoints 4
  trait Daemon
  body "This unit gains {power} for each corrupted unit controlled by an opponent."
  selfPower \g self ->
    length [u | u <- g.units, u.controller /= self.controller, u.corrupted]

heraldOfChange :: CardDef Unit
heraldOfChange = unitCard "redemption-of-a-mage-072" "Herald of Change" do
  race Chaos
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Cultist
  body "This unit gains +2 hit points while defending."
  selfHP \_g self -> if self.defending then 2 else 0

disgracedChampion :: CardDef Unit
disgracedChampion = unitCard "redemption-of-a-mage-073" "Disgraced Champion" do
  race Chaos
  cost 3
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Action: When this unit is destroyed, deal 2 damage to target unit."
  onSelfDestroyed \_owner self ->
    withTarget self.controller AnyUnit \k -> dealDamage k 2

schemingCultist :: CardDef Unit
schemingCultist = unitCard "the-burning-of-derricksburg-011" "Scheming Cultist" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Cultist
  body "Action: Spend 1 resource to deal 1 damage to target corrupted unit."
  action "Deal 1 to a corrupted unit" 1 \u ->
    withTarget u.user (unitWhere (.corrupted)) \k -> dealDamage k 1

beastOfRot :: CardDef Unit
beastOfRot = unitCard "bleeding-sun-114" "Beast of Rot" do
  race Chaos
  cost 5
  loyalty 2
  power 2
  hitPoints 4
  trait Creature
  body "Lower the cost to play this unit by 1 for each Disease card in play. This unit gains {power} for each Disease card in play."
  selfCostAdjust \g _pk -> negate (diseaseInPlay g)
  selfPower \g _self -> diseaseInPlay g
  where
    diseaseInPlay g =
      length [s | s <- allInPlaySupports g, Disease `elem` s.cardDef.traits]

necroticSpasms :: CardDef Support
necroticSpasms = supportCard "bleeding-sun-115" "Necrotic Spasms" do
  race Chaos
  cost 1
  loyalty 2
  traits [Attachment, Disease]
  body "Attach to a target unit. Action: At the beginning of its controller's turn, attached unit takes 1 uncancellable damage."
  onAttachedHostTurnBegin \_owner _self host ->
    dealUncancellableDamage host.key 1

taintedWell :: CardDef Support
taintedWell = supportCard "the-fourth-waystone-092" "Tainted Well" do
  race Chaos
  cost 4
  loyalty 2
  power 2
  trait Building
  body "Action: When a unit is corrupted, deal 1 damage to that unit."
  onUnitCorrupted \_owner _self uk -> dealDamage uk 1

eslian :: CardDef Unit
eslian = unitCard "the-silent-forge-052" "Esli'an" do
  hero
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Cultist
  body "Limit one Hero per zone. Action: When this unit attacks, corrupt target unit in the defending zone."
  onMyAttackDeclared \_owner self zone _attackers ->
    withTarget self.controller
      (UnitMatching \pk _g u -> u.controller /= pk && u.zone == zone)
      corrupt

spreadingDarkness :: CardDef Tactic
spreadingDarkness = tacticCard "the-burning-of-derricksburg-014" "Spreading Darkness" do
  race Chaos
  cost 3
  loyalty 3
  body "Action: Each attacking unit gains {power} for each corrupted unit in the defending zone."
  playableWhen \g pk -> inCombat g pk
  whenResolved \_self -> do
    g <- getGame
    for_ g.combat \cs -> do
      let n = length [u | u <- g.units, u.corrupted, u.zone == cs.targetZone, u.controller == cs.defendingPlayer]
      for_ cs.attackers \k -> until EndOfTurn $ buffPower k n

horrificFavour :: CardDef Support
horrificFavour = supportCard "realm-of-the-phoenix-king-034" "Horrific Favour" do
  race Chaos
  cost 1
  loyalty 2
  trait Condition
  body
    "Action: When a Daemon unit enters play under your control, return a \
    \Disease card from your discard pile to your hand."
  onFriendlyUnitEnterPlay \_owner self enteredKey -> do
    let pk = self.controller
    g <- getGame
    case findUnit enteredKey g of
      Just u | hasTrait Daemon u -> do
        me <- playerOf pk <$> getGame
        let diseases = [c | c <- me.discard, Disease `elem` someCardTraits c.def]
        chooseFromCards pk 0 1 diseases
          "Choose a Disease card to return to your hand." \chosen ->
            for_ chosen \c -> returnFromDiscardToHand pk [c.key]
      _ -> pure ()

grandfathersCall :: CardDef Tactic
grandfathersCall = tacticCard "the-fall-of-karak-grimaz-035" "Grandfather's Call" do
  race Chaos
  cost 1
  loyalty 1
  body
    "Action: Sacrifice a unit to search the top five cards of your deck for any \
    \number of Disease cards, reveal them, and add them to your hand. Shuffle \
    \the remaining cards into your deck."
  playableWhen \g pk -> any (\u -> u.controller == pk) g.units
  whenResolved \self ->
    sacrificeOwnUnit self.controller "Sacrifice a unit." \_ -> do
      let pk = self.controller
      searchTopOfDeck pk 5 \result -> do
        let diseases = [c | c <- result.cards, Disease `elem` someCardTraits c.def]
        chooseFromCards pk 0 (length diseases) diseases "Choose Disease cards to add to your hand." \chosen ->
          unless (null chosen) $ push (TakeCardsFromDeckToHand pk (map (.key) chosen))
        shuffleDeck pk

embersToInferno :: CardDef Tactic
embersToInferno = tacticCard "the-silent-forge-053" "Embers to Inferno" do
  race Chaos
  cost 1
  loyalty 2
  trait Spell
  body "Action: Destroy target unit or support card in any burning zone."
  playableWhen \g pk -> hasTarget (Or unitInBurning supportInBurning) g pk
  whenResolved \self ->
    withTarget self.controller (Or unitInBurning supportInBurning) \case
      TargetUnitOption k -> destroyUnit k
      TargetSupportOption k -> destroySupport k
      _ -> pure ()
  where
    unitInBurning = UnitMatching \_pk g u -> zoneBurning g u.controller u.zone
    supportInBurning = SupportMatching \_pk g s -> zoneBurning g s.controller s.zone

-- Assault on Ulthuan ---------------------------------------------------

maledictorOfTzeentch :: CardDef Unit
maledictorOfTzeentch = unitCard "assault-on-ulthuan-052" "Maledictor of Tzeentch" do
  race Chaos
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  trait Mage
  body "Forced: At the beginning of your turn, this unit and one other target unit each take 1 damage."
  onMyTurnBegin \_owner self -> do
    dealDamage self.key 1
    withTarget self.controller (unitWhere (\u -> u.key /= self.key)) \k ->
      dealDamage k 1

bloodFrenzy :: CardDef Support
bloodFrenzy = supportCard "assault-on-ulthuan-053" "Blood Frenzy" do
  race Chaos
  cost 1
  loyalty 3
  trait Attachment
  body
    "Attach to a target unit in your battlefield. Attached unit gains {power} for each \
    \resource token on this card. Forced: Each time an opponent's unit enters a discard \
    \pile from play, put a resource token on this attachment."
  -- Approximation: 'onOpponentUnitLeavePlay' fires for any departure
  -- (destruction, sacrifice, AND bounce-to-hand), so a bounced enemy
  -- unit also banks a token; the printed text counts only units that
  -- enter a discard pile.
  attachedTo \self unit ->
    when (self.tokens > 0) $ gainPower unit self.tokens
  onOpponentUnitLeavePlay \_owner self _uk _zone _code ->
    adjustSupportTokens self.key 1

tzeentchsFirestorm :: CardDef Tactic
tzeentchsFirestorm = tacticCard "assault-on-ulthuan-054" "Tzeentch's Firestorm" do
  race Chaos
  cost 4
  loyalty 2
  trait Spell
  body "Action: Deal 2 damage to 2 different target units."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \k1 -> do
      dealDamage k1 2
      withTarget pk (unitWhere (\u -> u.key /= k1)) \k2 -> dealDamage k2 2

-- March of the Damned --------------------------------------------------

-- | The "play with decks revealed" clause is already satisfied: the
-- engine ships full deck contents to both clients. The "play your top
-- deck card as though in hand" permission is keyed off this card's
-- presence in play ('Engine.controlsLordOfChange'), so the def itself
-- needs only its statline and text.
lordOfChange :: CardDef Unit
lordOfChange = unitCard "march-of-the-damned-021" "Lord of Change" do
  race Chaos
  cost 6
  loyalty 3
  power 3
  hitPoints 4
  trait Daemon
  body
    "Players play with the top card of their decks revealed. You may play \
    \the top card of your deck as though it were in your hand."

brayShaman :: CardDef Unit
brayShaman = unitCard "march-of-the-damned-022" "Bray Shaman" do
  race Chaos
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  trait Warrior
  body
    "Action: Corrupt this unit to have target attacking unit you control gain \
    \{power}{power}. Sacrifice that unit at the end of the turn."
  actionWith "Frenzy" 0 [CorruptSelf] \usage ->
    withTarget usage.user
      (UnitMatching \me _g u -> u.controller == me && u.attacking)
      \k -> do
        until EndOfTurn $ buffPower k 2
        queueEoTSacrifice k

bloodboilFever :: CardDef Support
bloodboilFever = supportCard "march-of-the-damned-023" "Bloodboil Fever" do
  race Chaos
  cost 1
  loyalty 2
  power 0
  traits [Attachment, Disease]
  body
    "Attach to a target unit. Action: At the beginning of its controller's turn, put a \
    \resource token on this card. Then, deal X damage to the attached unit. X is the number \
    \of resource tokens on this card."
  onAttachedHostTurnBegin \_owner self host -> do
    adjustSupportTokens self.key 1
    dealDamage host.key (self.tokens + 1)

cloyingQuagmire :: CardDef Tactic
cloyingQuagmire = tacticCard "march-of-the-damned-024" "Cloying Quagmire" do
  race Chaos
  cost 2
  loyalty 2
  trait Spell
  body "Action: Choose a target unit. Corrupt that unit and deal 2 damage to it."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> do
      corrupt k
      dealDamage k 2

bloodSummoning :: CardDef Tactic
bloodSummoning = tacticCard "march-of-the-damned-025" "Blood Summoning" do
  race Chaos
  cost 0
  loyalty 3
  trait Spell
  body
    "Action: Corrupt X units you control to lower the cost of the next [Chaos] \
    \unit you play this turn by X."
  whenResolved \self -> do
    let pk = self.controller
    g <- getGame
    let candidates = [u.key | u <- g.units, u.controller == pk, not u.corrupted]
    chooseUpTo pk (length candidates) candidates \chosen -> do
      traverse_ corrupt chosen
      let x = length chosen
      when (x > 0) $ push (ScheduleNextUnitDiscount pk x)

-- Hidden Kingdoms (deluxe expansion) -----------------------------------

seekerChariot :: CardDef Unit
seekerChariot = unitCard "hidden-kingdoms-047" "Seeker Chariot" do
  race Chaos
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  traits [Cavalry, Daemon]
  battlefieldOnly
  body
    "Battlefield only. While this unit is not opposed in combat, it deals +2 \
    \damage in combat."
  combatPower \g u -> if isOpposed g u then 0 else 2

callTheBrayherd :: CardDef Tactic
callTheBrayherd = tacticCard "fiery-dawn-114" "Call the Brayherd" do
  race Chaos
  cost 4
  loyalty 3
  trait Spell
  body
    "Play during your turn. Action: Reveal the top five cards of your deck. \
    \Put all revealed units with a printed cost of 3 or lower into your \
    \battlefield. Then, shuffle your deck."
  playableWhen \g pk -> g.currentPlayer == pk
  whenResolved \self -> do
    let pk = self.controller
    revealTopOfDeck pk 5 \r -> do
      for_ r.cards \c ->
        case asUnit c.def of
          Just _ | someCardCost c.def <= 3 ->
            putUnitIntoPlay pk FromDeck c.key BattlefieldZone
          _ -> pure ()
      shuffleDeck pk

daemonicOffering :: CardDef Support
daemonicOffering = supportCard "hidden-kingdoms-049" "Daemonic Offering" do
  race Chaos
  cost 0
  loyalty 0
  trait Tribute
  body
    "Non-Chaos only. Action: Sacrifice this card to ignore the loyalty cost \
    \of the next [Chaos] card you play this turn."
  actionWith "Tribute" 0 [SacrificeSelf] \usage ->
    grantLoyaltyWaiver usage.user Chaos
