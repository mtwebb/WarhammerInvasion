{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Dwarf core cards (core-001..025). Static data matches @cards.json@
-- and every printed ability has a functional implementation against
-- the engine's effect / trigger / modifier primitives.
module Invasion.Card.Defs.Dwarf (module Invasion.Card.Defs.Dwarf) where

import Data.Map.Strict qualified as Map
import Data.Text qualified as T
import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Invasion.Capital
import Invasion.Entity (QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

defenderOfTheHold :: CardDef Unit
defenderOfTheHold = unitCard "core-001" "Defender of the Hold" do
  race Dwarf
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Battlefield only."
  battlefieldOnly

zhufbarEngineers :: CardDef Unit
zhufbarEngineers = unitCard "core-002" "Zhufbar Engineers" do
  race Dwarf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Engineer
  body "Forced: After this unit leaves play, each opponent must sacrifice a unit in this corresponding zone."
  onSelfLeavesPlay \_owner self ->
    -- Forced: the opponent MUST sacrifice (their choice of which
    -- unit); a declinable target prompt would let them skip it.
    mustSacrificeInZone self.controller.next self.zone
      "Zhufbar Engineers: sacrifice a unit in this zone."

hammererOfKarakAzul :: CardDef Unit
hammererOfKarakAzul = unitCard "core-003" "Hammerer of Karak Azul" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  traits [Warrior, Elite]
  toughness 1
  body "Toughness 1 (whenever this unit is assigned damage, cancel 1 of that damage)."

trollSlayers :: CardDef Unit
trollSlayers = unitCard "core-004" "Troll Slayers" do
  race Dwarf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Slayer
  body "Battlefield. This unit gains {power}{power} while you have at least two developments in this zone."
  battlefield $ constant \self ->
    withZoneOf self \z ->
      when (z.developments >= 2) $ gainPower self 2

runesmith :: CardDef Unit
runesmith = unitCard "core-005" "Runesmith" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Priest
  body "Quest. Action: Spend 2 resources to have a target unit gain {power} until the end of the turn."
  quest $ action "Buff a unit" 2 \usage ->
    withTarget usage.user AnyUnit \t -> until EndOfTurn $ buffPower t 1

durgnarTheBold :: CardDef Unit
durgnarTheBold = unitCard "core-006" "Durgnar the Bold" do
  hero
  trait Warrior
  race Dwarf
  cost 3
  loyalty 3
  power 2
  hitPoints 2
  body "Limit one Hero per zone. This unit gains {power}{power} while one section of your capital is burning."
  effects \self owner ->
    when (capitalBurning owner) $ gainPower self 2

kingKazador :: CardDef Unit
kingKazador = unitCard "core-007" "King Kazador" do
  hero
  trait Warrior
  race Dwarf
  cost 6
  loyalty 5
  power 3
  hitPoints 6
  toughness 2
  body "Limit one Hero per zone. Toughness 2. Opponents cannot target this unit with card effects unless they pay an additional 3 resources per effect."
  targetTax \_g caster self ->
    if caster /= self.controller then 3 else 0

dwarfCannonCrew :: CardDef Unit
dwarfCannonCrew = unitCard "core-008" "Dwarf Cannon Crew" do
  race Dwarf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Engineer
  body "Forced: After this unit enters play, search the top five cards of your deck for a support card with cost 2 or lower and put it into this zone, if able. Then shuffle your deck."
  onEnterPlay \_owner self -> do
    let pk = self.controller
    searchTopOfDeck pk 5 \result -> do
      let matches = filterSupportsIn result.cards (costAtMost 2)
      chooseFromCards pk 0 1 matches
        "Choose a support to put into play (or skip)." \chosen ->
          for_ chosen \c -> playSupportFromDeck pk c.key self.zone
      shuffleDeck pk

dwarfMasons :: CardDef Unit
dwarfMasons = unitCard "core-009" "Dwarf Masons" do
  race Dwarf
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Engineer
  body "Forced: After this unit enters play, put the top card of your deck facedown into this zone as a development."
  onEnterPlay \_owner self ->
    addDevelopment self.controller self.zone

dwarfRanger :: CardDef Unit
dwarfRanger = unitCard "core-010" "Dwarf Ranger" do
  race Dwarf
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Ranger
  scout
  body "Scout. Quest. Forced: After one of your other {dwarf} units leaves play, deal 1 damage to one target unit or capital."
  quest $ forced \self ->
    onUnitOfLeavesPlay self.controller \unit ->
      when (unit.key /= self.key && unit `isRace` Dwarf) $
        withTarget self.controller (AnyUnit `Or` AnyCapital) \case
          TargetUnitOption u -> dealDamage u 1
          TargetZoneOption owner z -> dealZoneDamage owner z 1

mountainBrigade :: CardDef Unit
mountainBrigade = unitCard "core-011" "Mountain Brigade" do
  race Dwarf
  cost 4
  loyalty 2
  power 2
  hitPoints 6
  trait Warrior

ironbreakersOfAnkhor :: CardDef Unit
ironbreakersOfAnkhor = unitCard "core-012" "Ironbreakers of Ankhor" do
  race Dwarf
  cost 5
  loyalty 2
  power 2
  hitPoints 3
  traits [Warrior, Elite]
  toughnessX
  body "Toughness X (whenever this unit is assigned damage, cancel X of that damage). X is the number of development cards in this zone."

runeOfFortitude :: CardDef Support
runeOfFortitude = supportCard "core-013" "Rune of Fortitude" do
  race Dwarf
  cost 2
  loyalty 1
  trait Rune
  body "Each unit attacking this zone loses {power} unless its controller pays 1 resource per unit."
  imposesRuneOfFortitudeTax

keystoneForge :: CardDef Support
keystoneForge = supportCard "core-014" "Keystone Forge" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Kingdom. Forced: After your turn begins, heal 1 damage to your capital."
  kingdom $ forced \self ->
    onTurnBegin self.controller $
      healCapital self.controller 1

organGun :: CardDef Support
organGun = supportCard "core-015" "Organ Gun" do
  race Dwarf
  cost 0
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target unit. Attached unit gains {power}{power} while defending."
  attachedTo \_self unit ->
    when unit.defending $ gainPower unit 2

masterRuneOfDismay :: CardDef Support
masterRuneOfDismay = supportCard "core-016" "Master Rune of Dismay" do
  race Dwarf
  cost 4
  loyalty 3
  power 2
  trait Rune
  body "Kingdom. Opponent's units cost 1 additional resource to play."
  globalCostAdjust \_g s playing _filter ->
    if playing /= s.controller && s.zone == KingdomZone then 1 else 0

aGloriousDeath :: CardDef Quest
aGloriousDeath = questCard "core-017" "A Glorious Death" do
  race Dwarf
  cost 0
  loyalty 2
  body "Quest. Action: Sacrifice the unit on this quest to destroy up to two target attacking units. Use this ability only if A Glorious Death has 3 or more resource tokens on it. Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  action "Glorious sacrifice" 0 \usage ->
    withQuest usage.self.key \q -> when (q.tokens >= 3) $
      for_ q.questingUnit \quester -> do
        destroyUnit quester
        withCombat \cs ->
          when (cs.attackingPlayer /= usage.user) $
            chooseUpTo usage.user 2 cs.attackers (traverse_ destroyUnit)

grudgeThrower :: CardDef Support
grudgeThrower = supportCard "core-018" "Grudge Thrower" do
  race Dwarf
  cost 1
  loyalty 2
  trait Siege
  body "Battlefield. Action: Spend 1 resource and sacrifice a unit to have each attacking or defending unit gain {power} until the end of the turn."
  battlefield $ actionWith "Volley" 1 [SacrificeUnit] \_usage ->
    withCombat \cs ->
      for_ (cs.attackers <> cs.defenders) \k ->
        until EndOfTurn $ buffPower k 1

buryingTheGrudge :: CardDef Tactic
buryingTheGrudge = tacticCard "core-019" "Burying the Grudge" do
  race Dwarf
  cost 0
  loyalty 2
  body "Action: Gain 1 resource for each unit that entered a discard pile this turn."
  playableWhen \g _ ->
    maybe 0 (.unitsDiscarded) (Map.lookup ThisTurn g.history) > 0
  whenResolved \self ->
    withHistory ThisTurn \h ->
      gainResources self.controller h.unitsDiscarded

stubbornRefusal :: CardDef Tactic
stubbornRefusal = tacticCard "core-020" "Stubborn Refusal" do
  race Dwarf
  cost 2
  loyalty 1
  body "Action: Move all damage from one target unit to another target unit in any player's corresponding zone."
  playableWhen \g _pk ->
    any (\u -> isDamaged u && hasPeerInZone g u) g.units
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \src ->
      withUnit src \srcUnit -> do
        peers <- filter (\u -> u.key /= src) <$> unitsInZone srcUnit.zone
        chooseUpTo pk 1 (map (.key) peers) \chosen ->
          for_ chosen \dst -> moveAllDamage src dst

strikingTheGrudge :: CardDef Tactic
strikingTheGrudge = tacticCard "core-021" "Striking the Grudge" do
  race Dwarf
  cost 1
  loyalty 3
  body "Action: One target attacking or defending unit gains {power}{power} until the end of the turn."
  playableWhen $ hasTarget (Or attackingUnit defendingUnit)
  whenResolved \self ->
    withTarget self.controller (Or attackingUnit defendingUnit) \case
      TargetUnitOption k -> until EndOfTurn $ buffPower k 2
      _ -> pure ()

grudgeThrowerAssault :: CardDef Tactic
grudgeThrowerAssault = tacticCard "core-022" "Grudge Thrower Assault" do
  race Dwarf
  cost 2
  loyalty 3
  body "Play during combat, after damage has been assigned. Action: Destroy one target attacking unit."
  -- "After damage has been assigned": only the assign-response and
  -- post-apply windows qualify. Without the window gate this could
  -- kill an attacker before its damage was pooled, which is strictly
  -- stronger than the printed timing.
  playableWhen \g pk ->
    hasEnemyAttacker g pk
      && case g.actionWindow of
        Just aw ->
          aw.trigger
            `elem` [AfterAssignCombatDamage, AfterApplyCombatDamage]
        Nothing -> False
  whenResolved \self ->
    withTarget self.controller attackingUnit \k -> destroyUnit k

demolition :: CardDef Tactic
demolition = tacticCard "core-023" "Demolition!" do
  race Dwarf
  cost 2
  loyalty 1
  body "Action: Destroy one target support card or development."
  -- One unified picker over both prongs: any in-play support card
  -- (friendly or enemy, free-standing or attached) or any development
  -- (either player's). The printed text has no "enemy" restriction
  -- and the player must be free to pick a development even when
  -- supports exist.
  playableWhen \g pk ->
    hasTarget (AnySupportCard `Or` AnyDevelopmentZone) g pk
  whenResolved \self ->
    withTarget self.controller (AnySupportCard `Or` AnyDevelopmentZone) \case
      TargetSupportOption k -> destroySupport k
      TargetZoneOption owner zk -> destroyDevelopment owner zk
      _ -> pure ()

wakeTheMountain :: CardDef Tactic
wakeTheMountain = tacticCard "core-024" "Wake the Mountain" do
  race Dwarf
  cost 3
  loyalty 2
  body "Action: Put the top three cards of your deck into your battlefield or kingdom facedown as developments. (All three developments must go in the same zone.)"
  playableWhen \g pk -> hasDeckSize 3 g pk && canDevelop g pk
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk MyDevZone \zone ->
      replicateM_ 3 (addDevelopment pk zone)

masterRuneOfValaya :: CardDef Tactic
masterRuneOfValaya = tacticCard "core-025" "Master Rune of Valaya" do
  race Dwarf
  cost 2
  loyalty 1
  traits [Spell, Rune]
  body "Action: Cancel all damage assigned during the battlefield phase this turn."
  -- Clears every pending combat assignment (units and zone spillover)
  -- but leaves the combat itself in flight: the apply step then
  -- commits nothing, and Scout / end-of-combat hooks still run as
  -- normal.
  whenResolved \_ ->
    push CancelAllAssignedDamage

-- The Corruption cycle ------------------------------------------------

gurnisElite :: CardDef Unit
gurnisElite = unitCard "the-skavenblight-threat-001" "Gurni's Elite" do
  race Dwarf
  cost 3
  loyalty 3
  power 3
  hitPoints 1
  traits [Warrior, Elite]
  body "Battlefield only."
  battlefieldOnly

standYourGround :: CardDef Tactic
standYourGround = tacticCard "the-skavenblight-threat-002" "Stand Your Ground" do
  race Dwarf
  cost 1
  loyalty 2
  body
    "Action: Put into play one target {dwarf} unit that entered your discard pile this turn. \
    \(You choose the zone in which the unit enters play.)"
  playableWhen \g pk -> not (null (fallenDwarves g pk))
  whenResolved \self -> do
    let pk = self.controller
    g <- getGame
    chooseFromCards pk 1 1 (fallenDwarves g pk)
      "Choose a Dwarf unit that fell this turn to put back into play." \chosen ->
        for_ chosen \c ->
          withTarget pk MyAnyZone \zk ->
            putUnitIntoPlay pk FromDiscard c.key zk
  where
    fallenDwarves g pk =
      let fellThisTurn =
            Map.findWithDefault [] pk
              (Map.findWithDefault mempty ThisTurn g.history).discardedUnitsBy
       in [ c
          | c <- (playerOf pk g).discard
          , c.key `elem` fellThisTurn
          , maybe False (`isRace` Dwarf) (asUnit c.def)
          ]

dwarfMiner :: CardDef Unit
dwarfMiner = unitCard "path-of-the-zealot-021" "Dwarf Miner" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Engineer
  body "Forced: After this unit enters play, heal up to 2 damage from this zone's section of your capital."
  onEnterPlay \_owner self ->
    push (HealZone self.controller self.zone 2)

gromrilArmour :: CardDef Support
gromrilArmour = supportCard "path-of-the-zealot-022" "Gromril Armour" do
  race Dwarf
  cost 0
  loyalty 2
  trait Attachment
  body "Attach to a unit. Attached unit gains Toughness 1."
  supportToughnessAura \_g s u ->
    if s.attachedTo == Just u.key then 1 else 0

gurniThorgrimson :: CardDef Unit
gurniThorgrimson = unitCard "tooth-and-claw-041" "Gurni Thorgrimson" do
  hero
  race Dwarf
  cost 5
  loyalty 3
  power 2
  hitPoints 4
  body "Limit one Hero per zone. This unit gains {power} for each card attached to it."
  selfPower \_g u -> length u.attachments

anvilOfDoom :: CardDef Support
anvilOfDoom = supportCard "tooth-and-claw-042" "Anvil of Doom" do
  race Dwarf
  cost 3
  loyalty 2
  power 1
  trait Building
  body "Kingdom. Units in your battlefield with one or more Attachment cards attached gain {power}."
  supportAura \_g s u ->
    if s.zone == KingdomZone
      && u.controller == s.controller
      && u.zone == BattlefieldZone
      && not (null u.attachments)
      then 1
      else 0

blessingOfValaya :: CardDef Tactic
blessingOfValaya = tacticCard "tooth-and-claw-043" "Blessing of Valaya" do
  race Dwarf
  cost 2
  loyalty 1
  traits [Spell, Rune]
  body "Action: The next 2 damage dealt to one target unit are redirected to another target unit."
  playableWhen \g _pk -> length g.units >= 2
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \src ->
      withTarget pk (unitWhere \u -> u.key /= src) \dst ->
        -- No printed expiry: the shield lasts until consumed.
        until Permanent $ redirectNextDamage src 2 dst

mountainLegion :: CardDef Unit
mountainLegion = unitCard "the-deathmaster-s-dance-061" "Mountain Legion" do
  race Dwarf
  cost 1
  loyalty 3
  power 1
  hitPoints 1
  trait Warrior
  toughness 1
  body "Toughness 1 (whenever this unit is assigned damage, cancel 1 of that damage)."

emptyTheHold :: CardDef Tactic
emptyTheHold = tacticCard "the-deathmaster-s-dance-062" "Empty the Hold" do
  race Dwarf
  cost 4
  loyalty 1
  body
    "Action: Search the top five cards of your deck. You may put into play (in any zone) \
    \one unit found amongst those cards with printed cost 3 or lower. Then, shuffle your deck."
  playableWhen \g pk -> hasDeckSize 1 g pk
  whenResolved \self -> do
    let pk = self.controller
    searchTopOfDeck pk 5 \result -> do
      let matches =
            [ c
            | c <- result.cards
            , Just cd <- [asUnit c.def]
            , costAtMost 3 cd
            ]
      chooseFromCards pk 0 1 matches
        "Choose a unit with printed cost 3 or lower to put into play." \chosen ->
          for_ chosen \c ->
            withTarget pk MyAnyZone \zk ->
              push (PutUnitIntoPlayFromDeck pk c.key zk)
      shuffleDeck pk

reclaimTheHold :: CardDef Quest
reclaimTheHold = questCard "the-deathmaster-s-dance-063" "Reclaim the Hold" do
  race Dwarf
  cost 0
  loyalty 2
  body
    "Quest. After a card you control leaves play, you may discard 1 resource token from this card \
    \to place that card into its current zone as a development instead of discarding it. \
    \Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  -- Covers departing UNITS (the overwhelmingly common case). Support
  -- departures don't carry their zone on the leave-play message, so
  -- they can't be reclaimed yet.
  onReceive $ Receive \msg _owner self -> case msg of
    UnitLeftPlay du
      | du.controller == self.controller -> do
          g <- getGame
          whenJust (findQuest self.key g) \q -> do
            let inDiscard =
                  any ((== du.key) . (.key)) (playerOf self.controller g).discard
            when (q.tokens >= 1 && inDiscard) $
              may self.controller
                ("Reclaim the Hold: place " <> T.pack du.cardDef.title
                  <> " as a development instead of discarding it?")
                do
                  addQuestToken self.key (-1)
                  push (ConvertDepartedToDevelopment self.controller du.key du.zone)
    _ -> pure ()

dragonslayer :: CardDef Unit
dragonslayer = unitCard "the-warpstone-chronicles-081" "Dragonslayer" do
  race Dwarf
  cost 4
  loyalty 2
  power 2
  hitPoints 1
  trait Slayer
  toughness 2
  body
    "Toughness 2. When you attack during your battlefield phase, this unit may attack \
    \from your quest zone."
  attacksFromZones [BattlefieldZone, QuestZone]

greatBookOfGrudges :: CardDef Support
greatBookOfGrudges = supportCard "the-warpstone-chronicles-082" "Great Book of Grudges" do
  unique
  race Dwarf
  cost 2
  loyalty 1
  traits [Attachment, Relic]
  body
    "Attach to a target {dwarf} unit. Attached unit gains {power}{power}. \
    \When attached unit enters a discard pile from play, each opponent takes 6 indirect damage. \
    \(Players assign their own indirect damage.)"
  attachmentPower 2
  -- Fires on the host's destruction (the only path into a discard
  -- pile from play for an in-play unit; bounce goes to hand).
  onReceive $ Receive \msg _owner self -> case msg of
    DestroyUnit uk
      | Just hostKey <- self.attachedTo
      , uk == hostKey ->
          indirectDamage self.controller.next 6
    _ -> pure ()

flameCannon :: CardDef Support
flameCannon = supportCard "the-warpstone-chronicles-083" "Flame Cannon" do
  race Dwarf
  cost 0
  loyalty 1
  traits [Attachment, Weapon]
  body
    "Attach to a target unit. While attached unit is attacking or defending, it gains \
    \{power} for each enemy unit participating in the battle."
  attachedTo \_self unit -> do
    g <- getGame
    let enemies = case g.combat of
          Just cs
            | unit.attacking -> length cs.defenders
            | unit.defending -> length cs.attackers
          _ -> 0
    when ((unit.attacking || unit.defending) && enemies > 0) $
      gainPower unit enemies

longbeards :: CardDef Unit
longbeards = unitCard "arcane-fire-101" "Longbeards" do
  race Dwarf
  cost 4
  loyalty 2
  power 3
  hitPoints 4
  traits [Warrior, Elite]
  body "Battlefield only."
  battlefieldOnly

runeOfHearthAndHome :: CardDef Tactic
runeOfHearthAndHome = tacticCard "arcane-fire-102" "Rune of Hearth and Home" do
  race Dwarf
  cost 10
  loyalty 5
  traits [Epic, Spell]
  body "Epic Spell. Play during your turn. Action: Heal all damage to your capital."
  playableWhen \g pk -> g.currentPlayer == pk
  whenResolved \self -> healCapital self.controller 999

hewnFromTheMountain :: CardDef Tactic
hewnFromTheMountain = tacticCard "arcane-fire-103" "Hewn From the Mountain" do
  race Dwarf
  cost 2
  loyalty 1
  body "Action: Each of your defending units gains {power}{power} until the end of the turn."
  whenResolved \self ->
    withCombat \cs ->
      when (cs.defendingPlayer == self.controller) $
        for_ cs.defenders \k ->
          until EndOfTurn $ buffPower k 2

-- Oaths of Vengeance ---------------------------------------------------

wealthOfTheHold :: CardDef Support
wealthOfTheHold = supportCard "oaths-of-vengeance-027" "Wealth of the Hold" do
  race Dwarf
  cost 2
  loyalty 2
  power 1
  trait Vault
  body "Action: At the beginning of each opponent's turn, gain 1 resource."
  onAnyTurnBegin \_owner self turnOwner ->
    when (turnOwner /= self.controller) $ gainResources self.controller 1

-- Glory of Days Past ---------------------------------------------------

karakHirnMine :: CardDef Support
karakHirnMine = supportCard "glory-of-days-past-064" "Karak Hirn Mine" do
  race Dwarf
  cost 1
  loyalty 1
  power 1
  trait Building
  body "If you control a faceup non-[Dwarf] unit or support card, sacrifice this card."
  sacrificeWhenBoardChanges \g self ->
    controlsNonRaceUnitOrSupport g self.controller Dwarf

-- The Ruinous Hordes ---------------------------------------------------

kingAlrik :: CardDef Unit
kingAlrik = unitCard "the-ruinous-hordes-088" "King Alrik" do
  unique
  race Dwarf
  cost 3
  loyalty 3
  power 0
  hitPoints 3
  traits [Noble, Warrior]
  battlefieldOnly
  body "Battlefield only. This unit gains {power} for each resource you have in your pool."
  selfPower \g u -> let Resources r = (playerOf u.controller g).resources in r

hornHoldDefender :: CardDef Unit
hornHoldDefender = unitCard "the-ruinous-hordes-089" "Horn Hold Defender" do
  race Dwarf
  cost 4
  loyalty 2
  power 2
  hitPoints 2
  trait Warrior
  body
    "While you have at least 3 resources in your pool, this unit gains Toughness 2. \
    \Action: When this unit attacks or defends, gain 1 resource."
  selfToughness \g u ->
    let Resources r = (playerOf u.controller g).resources in if r >= 3 then 2 else 0
  onMyAttackOrDefend \_owner self -> gainResources self.controller 1

-- Faith and Steel ------------------------------------------------------

doomSeeker :: CardDef Unit
doomSeeker = unitCard "faith-and-steel-106" "Doom-Seeker" do
  race Dwarf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Slayer
  body
    "Action: When this unit survives an attack on an opponent's zone, sacrifice this unit \
    \to destroy target support card in that zone."
  onCombatResolveAsAttacker \_owner self cs -> do
    g <- getGame
    when (isJust (findUnit self.key g)) $
      may self.controller "Doom-Seeker: sacrifice to destroy a support in that zone?" do
        destroyUnit self.key
        withTarget self.controller
          (SupportMatching \_pk _g s -> s.zone == cs.targetZone && s.controller /= self.controller)
          destroySupport

fearlessInBattle :: CardDef Tactic
fearlessInBattle = tacticCard "faith-and-steel-107" "Fearless in Battle" do
  race Dwarf
  cost 1
  loyalty 3
  trait Slayer
  body
    "Action: Until the end of the phase, each Slayer unit you control deals +1 damage in \
    \combat and gains Toughness 1."
  whenResolved \self -> do
    slayers <- unitsMatching self.controller
      (UnitMatching \pk _g u -> u.controller == pk && Slayer `elem` u.cardDef.traits)
    for_ slayers \u -> do
      until EndOfTurn $ buffCombatDamage u.key 1
      until EndOfTurn $ buffToughness u.key 1

veteranThunderers :: CardDef Unit
veteranThunderers = unitCard "days-of-blood-005" "Veteran Thunderers" do
  race Dwarf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Warrior
  raider 2
  body "Raider 2."

-- Bloodquest: Shield of the Gods ----------------------------------------

dwarfAdventurer :: CardDef Unit
dwarfAdventurer = unitCard "shield-of-the-gods-103" "Dwarf Adventurer" do
  race Dwarf
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  body "Quest. While this unit is questing, opponents' units cost 1 additional resource to play."
  unitCostAdjust \_g self pk filt ->
    if self.zone == QuestZone && pk /= self.controller && filt.cfKind == Unit
      then 1
      else 0

hospitableCave :: CardDef Support
hospitableCave = supportCard "shield-of-the-gods-104" "Hospitable Cave" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  body "Quest. Each of your questing units gain Toughness 3."
  supportToughnessAura \_g self u ->
    if self.zone == QuestZone && u.controller == self.controller && u.zone == QuestZone
      then 3
      else 0

-- The Capital Cycle ----------------------------------------------------

runeblades :: CardDef Unit
runeblades = unitCard "karaz-a-karak-064" "Runeblades" do
  race Dwarf
  cost 4
  loyalty 2
  power 0
  hitPoints 3
  traits [Warrior, Elite]
  body
    "This unit deals +X damage in combat while attacking. X is the highest \
    \loyalty of a {dwarf} card you control."
  combatPower \g u ->
    if unitIsAttacking g u then highestLoyaltyControlled Dwarf g u.controller else 0

mountainSentry :: CardDef Unit
mountainSentry = unitCard "karaz-a-karak-061" "Mountain Sentry" do
  race Dwarf
  cost 1
  loyalty 2
  power 0
  hitPoints 1
  trait Musician
  body "Ranger units in this zone get +2 hit points."
  hpAura \_g self u ->
    if u.zone == self.zone && u.controller == self.controller && Ranger `elem` u.cardDef.traits
      then 2
      else 0

queenHelga :: CardDef Unit
queenHelga = unitCard "karaz-a-karak-062" "Queen Helga" do
  race Dwarf
  cost 5
  loyalty 3
  power 1
  hitPoints 3
  traits [Hero, Noble]
  limitOneHeroPerZone
  toughness 2
  body
    "Limit one Hero per zone. Toughness 2. Action: When a Hero unit enters play under \
    \your control, put a {dwarf} unit with printed cost 3 or lower into play in the same \
    \zone from your hand."
  onFriendlyUnitEnterPlay \_owner self uk -> do
    g <- getGame
    whenJust (findUnit uk g) \entered ->
      when (Hero `elem` entered.cardDef.traits) do
        let pk = self.controller
        me <- playerOf pk <$> getGame
        let isCand c = case c.def of
              UnitCardDef cd -> Dwarf `elem` cd.races && someCardCost c.def <= 3
              _ -> False
            cands = filter isCand me.hand
        chooseFromCards pk 0 1 cands
          "Queen Helga: put a Dwarf unit (cost 3 or lower) into play in that zone." \chosen ->
          for_ chosen \c -> putUnitIntoPlay pk FromHand c.key entered.zone

guildOfEngineers :: CardDef Unit
guildOfEngineers = unitCard "the-iron-rock-041" "Guild of Engineers" do
  race Dwarf
  cost 2
  loyalty 3
  power 1
  hitPoints 1
  trait Engineer
  body
    "Kingdom. Action: When you play an Engineer unit from your hand, gain 1 resource. \
    \Quest. Action: When you play an Engineer unit from your hand, draw a card."
  -- Approximation: 'onFriendlyUnitEnterPlay' fires for any Engineer
  -- unit entering under your control; the printed "from your hand"
  -- restriction isn't carried on 'UnitEnteredPlay', so put-into-play
  -- effects also count.
  -- TODO: tighten to "from your hand" once UnitEnteredPlay carries the
  -- entry origin (e.g. a PlayUnitOrigin field), so put-into-play /
  -- relocation entries stop triggering the resource/draw payoff.
  kingdom $ onFriendlyUnitEnterPlay \_owner self uk -> do
    g <- getGame
    whenJust (findUnit uk g) \u ->
      when (Engineer `elem` u.cardDef.traits) $ gainResources self.controller 1
  quest $ onFriendlyUnitEnterPlay \_owner self uk -> do
    g <- getGame
    whenJust (findUnit uk g) \u ->
      when (Engineer `elem` u.cardDef.traits) $ drawCard self.controller

buildingForWar :: CardDef Quest
buildingForWar = questCard "karaz-a-karak-080" "Building for War" do
  race Dwarf
  cost 0
  loyalty 3
  body
    "Quest. Action: When this card enters play, draw a card. Quest. Action: When you play \
    \a {dwarf} non-Attachment support card from your hand, gain 1 resource if a unit is \
    \questing here."
  onEnterPlay \_owner self -> drawCard self.controller
  onQuestSupportPayoff Dwarf \self -> gainResources self.controller 1

leaveNoTrace :: CardDef Tactic
leaveNoTrace = tacticCard "karaz-a-karak-070" "Leave No Trace" do
  race Dwarf
  cost 1
  loyalty 3
  body
    "Action: Discard a card from your hand with X loyalty to deal X damage to target \
    \defending unit."
  playableWhen \g pk -> not (null (playerOf pk g).hand) && hasTarget defendingUnit g pk
  whenResolved \self ->
    withTarget self.controller defendingUnit \k ->
      discardForLoyalty self.controller \x -> when (x > 0) $ dealDamage k x

-- The Morrslieb cycle ---------------------------------------------------

bugmansRangers :: CardDef Unit
bugmansRangers = unitCard "the-chaos-moon-022" "Bugman's Rangers" do
  race Dwarf
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Ranger
  body "This unit gains {power} for each Attachment support card attached to it."
  selfPower \_g self -> length self.attachments

bugmansTankard :: CardDef Support
bugmansTankard = supportCard "the-chaos-moon-023" "Bugman's Tankard" do
  race Dwarf
  cost 1
  loyalty 2
  trait Attachment
  body
    "Attach to a target [Dwarf] unit you control. Attached unit gains +1 hit \
    \points for each development in this zone."
  supportHPAura \g s u ->
    if Just u.key == s.attachedTo then devsInZone g u else 0

mountainGuard :: CardDef Unit
mountainGuard = unitCard "the-eclipse-of-hope-081" "Mountain Guard" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 3
  trait Warrior

daemonslayer :: CardDef Unit
daemonslayer = unitCard "the-twin-tailed-comet-041" "Daemonslayer" do
  race Dwarf
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  trait Slayer
  body
    "Toughness X. X is the number of resource tokens on this unit. Action: \
    \When you play a development from your hand, put a resource token on this card."
  selfToughness \_g self -> self.tokens
  onYouPlayDevelopment \_owner self -> push (AdjustUnitTokens self.key 1)

karakAzulForge :: CardDef Support
karakAzulForge = supportCard "signs-in-the-stars-062" "Karak Azul Forge" do
  race Dwarf
  cost 4
  loyalty 2
  power 2
  trait Building
  body
    "Each unit you control with at least one Attachment support card attached \
    \to it gains Toughness 1."
  supportToughnessAura \_g s u ->
    if u.controller == s.controller && not (null u.attachments) then 1 else 0

theSlayerOath :: CardDef Tactic
theSlayerOath = tacticCard "the-twin-tailed-comet-043" "The Slayer Oath" do
  race Dwarf
  cost 2
  loyalty 2
  body
    "Action: Target unit you control gains a {power} for each unit in your \
    \discard pile until the end of the turn."
  playableWhen \g pk -> any (\u -> u.controller == pk) g.units
  whenResolved \self -> do
    let pk = self.controller
    n <- (\g -> length [c | c <- (playerOf pk g).discard, isJust (asUnit c.def)]) <$> getGame
    withTarget pk ownUnit \k -> until EndOfTurn $ buffPower k n

-- The Enemy cycle -------------------------------------------------------

-- | "While attacking or defending, gains [Power] if there are more
-- opposing units in combat." Shared by Stonebearer and Son of Grungi.
-- Modeled as a combat-damage bonus that fires while the unit is in the
-- combat and the opposing side outnumbers its own.
moreOpposingInCombat :: Game -> UnitDetails -> Bool
moreOpposingInCombat g u = case g.combat of
  Just cs
    | u.key `elem` cs.attackers -> length cs.defenders > length cs.attackers
    | u.key `elem` cs.defenders -> length cs.attackers > length cs.defenders
  _ -> False

stonebearer :: CardDef Unit
stonebearer = unitCard "bleeding-sun-104" "Stonebearer" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "This unit gains {power} while attacking or defending if there are more opposing units in combat."
  combatPower \g self -> if moreOpposingInCombat g self then 1 else 0

sonOfGrungi :: CardDef Unit
sonOfGrungi = unitCard "the-silent-forge-041" "Son of Grungi" do
  race Dwarf
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  trait Warrior
  toughness 1
  body
    "Toughness 1. This unit gains {power}{power} while attacking or defending \
    \if there are more opposing units in combat."
  combatPower \g self -> if moreOpposingInCombat g self then 2 else 0

bodyguardOfBelegar :: CardDef Unit
bodyguardOfBelegar = unitCard "the-burning-of-derricksburg-001" "Bodyguard of Belegar" do
  race Dwarf
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior
  toughnessX
  body "Toughness X. X is the number of other [Dwarf] units in this zone."
  selfToughness \g self ->
    length
      [ u
      | u <- g.units
      , u.controller == self.controller
      , u.zone == self.zone
      , u.key /= self.key
      , Dwarf `elem` u.cardDef.races
      ]

grudgebearer :: CardDef Unit
grudgebearer = unitCard "the-fall-of-karak-grimaz-024" "Grudgebearer" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body
    "This unit gains {power} for each resource token on it. Forced: When one of \
    \your other [Dwarf] units leaves play, put a resource token on this card."
  selfPower \_g self -> self.tokens
  forced \self -> onUnitOfLeavesPlay self.controller \du ->
    when (du.key /= self.key && Dwarf `elem` du.cardDef.races) $
      push (AdjustUnitTokens self.key 1)

stuntySmasha :: CardDef Unit
stuntySmasha = unitCard "the-fall-of-karak-grimaz-030" "Stunty Smasha" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Action: Sacrifice this unit to destroy target development."
  action "Sacrifice to destroy a development" 0 \u -> do
    destroyUnit u.self.key
    withTarget u.user AnyDevelopmentZone \(owner, z) -> destroyDevelopment owner z

ancestralTomb :: CardDef Support
ancestralTomb = supportCard "the-fall-of-karak-grimaz-023" "Ancestral Tomb" do
  race Dwarf
  cost 2
  loyalty 2
  power 1
  trait Building
  body "Action: When this card enters play, put the top two cards of your deck into this zone as developments."
  onEnterPlay \_owner self -> do
    addDevelopment self.controller self.zone
    addDevelopment self.controller self.zone

grombrindalsElite :: CardDef Unit
grombrindalsElite = unitCard "redemption-of-a-mage-061" "Grombrindal's Elite" do
  race Dwarf
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  traits [Warrior, Elite]
  body "Lower the cost to play this unit by 3 if a zone is burning. This unit gains {power} if a zone is burning."
  selfCostAdjust \g _pk -> if burningZoneCount g > 0 then -3 else 0
  selfPower \g _self -> if burningZoneCount g > 0 then 1 else 0

honorInDeath :: CardDef Tactic
honorInDeath = tacticCard "bleeding-sun-102" "Honor in Death" do
  race Dwarf
  cost 1
  loyalty 2
  body "Action: Sacrifice a unit to destroy target attacking unit."
  playableWhen \g pk -> hasTarget attackingUnit g pk && any (\u -> u.controller == pk) g.units
  whenResolved \self ->
    sacrificeOwnUnit self.controller "Sacrifice a unit." \_ ->
      withTarget self.controller attackingUnit destroyUnit

masterRuneOfSpite :: CardDef Tactic
masterRuneOfSpite = tacticCard "the-fall-of-karak-grimaz-021" "Master Rune of Spite" do
  race Dwarf
  cost 3
  loyalty 3
  trait Rune
  body "Action: Each unit deals damage to itself equal to its power."
  whenResolved \_self -> do
    g <- getGame
    for_ g.units \u -> when (u.effectivePower > 0) $ dealDamage u.key u.effectivePower

runesmithApprentice :: CardDef Unit
runesmithApprentice = unitCard "the-fall-of-karak-grimaz-022" "Runesmith Apprentice" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body
    "Action: When this unit enters play, search the top five cards of your deck \
    \for any number of Rune cards, reveal them, and put them into your hand. \
    \Then, shuffle the remaining cards into your deck."
  onEnterPlay \_owner self -> do
    let pk = self.controller
    searchTopOfDeck pk 5 \result -> do
      let runes = [c | c <- result.cards, Rune `elem` someCardTraits c.def]
      chooseFromCards pk 0 (length runes) runes "Choose Rune cards to add to your hand." \chosen ->
        unless (null chosen) $ push (TakeCardsFromDeckToHand pk (map (.key) chosen))
      shuffleDeck pk

-- Assault on Ulthuan ---------------------------------------------------

slayersOfKarakKadrin :: CardDef Unit
slayersOfKarakKadrin = unitCard "assault-on-ulthuan-043" "Slayers of Karak Kadrin" do
  race Dwarf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  traits [Warrior, Slayer]
  body "Battlefield. Action: Sacrifice this unit to destroy one target attacking unit."
  battlefield $ action "Sacrifice to destroy attacker" 0 \usage ->
    withTarget usage.user attackingUnit \k -> do
      destroyUnit k
      destroyUnit usage.self.key

mountainBarracks :: CardDef Support
mountainBarracks = supportCard "assault-on-ulthuan-044" "Mountain Barracks" do
  race Dwarf
  cost 2
  loyalty 1
  power 1
  trait Building
  body "Your [Dwarf] units in this zone gain Toughness 1 (whenever this unit is assigned damage, cancel 1 of that damage)."
  supportToughnessAura \_g s u ->
    if u.controller == s.controller && u.zone == s.zone && Dwarf `elem` u.cardDef.races
      then 1
      else 0

-- March of the Damned --------------------------------------------------

serpentSlayer :: CardDef Unit
serpentSlayer = unitCard "march-of-the-damned-001" "Serpent Slayer" do
  race Dwarf
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  trait Slayer
  body
    "While you have at least three developments in this zone, this unit deals +X \
    \damage in combat. X is the number of developments in this zone."
  combatPower \g u ->
    let d = devsInZone g u in if d >= 3 then d else 0

longDrongsPirates :: CardDef Unit
longDrongsPirates = unitCard "march-of-the-damned-002" "Long Drong's Pirates" do
  race Dwarf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Slayer
  body "If there are at least two other Slayer units in this zone, this unit gains {power}{power}."
  selfPower \g u ->
    let otherSlayers =
          length
            [ v
            | v <- g.units
            , v.key /= u.key
            , v.zone == u.zone
            , v.controller == u.controller
            , Slayer `elem` v.cardDef.traits
            ]
     in if otherSlayers >= 2 then 2 else 0

oathstone :: CardDef Support
oathstone = supportCard "march-of-the-damned-003" "Oathstone" do
  race Dwarf
  cost 1
  loyalty 2
  power 0
  traits [Attachment, Rune]
  body "Attach to a target unit you control. If attached unit is destroyed, draw four cards."
  onReceive $ Receive \msg _owner self -> case msg of
    DestroyUnit uk
      | self.attachedTo == Just uk -> drawCards self.controller 4
    _ -> pure ()

toughAsNails :: CardDef Tactic
toughAsNails = tacticCard "march-of-the-damned-004" "Tough as Nails" do
  race Dwarf
  cost 1
  loyalty 2
  body
    "Action: Target Slayer unit gains Toughness X until the end of the turn. X is the \
    \damage on the unit."
  playableWhen $ hasTarget slayerUnit
  whenResolved \self ->
    withTarget self.controller slayerUnit \k ->
      withUnit k \u ->
        let Damage d = u.damage
         in when (d > 0) $ until EndOfTurn $ buffToughness k d
  where
    slayerUnit = UnitMatching \_pk _g u -> Slayer `elem` u.cardDef.traits

mercilessAssault :: CardDef Tactic
mercilessAssault = tacticCard "march-of-the-damned-005" "Merciless Assault" do
  race Dwarf
  cost 2
  loyalty 2
  body "Action: Target attacking unit you control gains {power}{power}{power} until the end of the turn."
  playableWhen $ hasTarget ownAttacker
  whenResolved \self ->
    withTarget self.controller ownAttacker \k ->
      until EndOfTurn $ buffPower k 3
  where
    ownAttacker = UnitMatching \me g u -> u.controller == me && unitIsAttacking g u

-- Legends (deluxe expansion) -------------------------------------------

veteranSlayer :: CardDef Unit
veteranSlayer = unitCard "legends-003" "Veteran Slayer" do
  race Dwarf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Slayer
  body
    "Action: Sacrifice this unit to have your units gain Toughness 2 until \
    \the end of the turn."
  actionWith "Last Stand" 0 [SacrificeSelf] \usage -> do
    mine <- unitsMatching usage.user ownUnit
    for_ mine \u -> until EndOfTurn $ buffToughness u.key 2

dwarfThunderer :: CardDef Unit
dwarfThunderer = unitCard "legends-005" "Dwarf Thunderer" do
  race Dwarf
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  traits [Warrior, Elite]
  body "If you control a legend, this unit gains Toughness 2."
  selfToughness \g self ->
    if isJust (legendOf self.controller g) then 2 else 0

runeOfCleaving :: CardDef Support
runeOfCleaving = supportCard "legends-006" "Rune of Cleaving" do
  race Dwarf
  cost 2
  loyalty 2
  traits [Attachment, Rune]
  body
    "Attach to a target unit you control. Attached unit gains {power}{power}. \
    \If attached unit leaves play, you may spend 2 resources to return this \
    \card to its owner's hand."
  attachmentPower 2

blastingCharge :: CardDef Tactic
blastingCharge = tacticCard "legends-007" "Blasting Charge" do
  race Dwarf
  cost 3
  loyalty 2
  body
    "Action: Choose a zone. Destroy up to 3 developments in that zone. Then, \
    \each player takes 3 indirect damage (players allocate their own indirect \
    \damage)."
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyDevelopmentZone \(owner, zk) -> do
      n <- chooseAmount pk 0 3 "Destroy how many developments?"
      replicateM_ n (destroyDevelopment owner zk)
    eachPlayer \p -> indirectDamage p 3
