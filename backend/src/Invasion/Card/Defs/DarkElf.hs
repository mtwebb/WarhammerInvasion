{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Dark Elf core cards (core-106..110). Only 5 cards — the rest of
-- the Dark Elf range arrives in later sets.
module Invasion.Card.Defs.DarkElf (module Invasion.Card.Defs.DarkElf) where

import Data.Map.Strict qualified as Map
import Invasion.Capital
import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.Card.Triggers
import Invasion.Card.Types
import Invasion.CardDef
import Invasion.Entity (QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (push)

discipleOfKhaine :: CardDef Unit
discipleOfKhaine = unitCard "core-106" "Disciple of Khaine" do
  race DarkElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  traits [Warrior, Priest]
  body "Action: Spend 2 resources to redirect one combat damage just assigned to this unit to another target unit."
  -- Action is selectable any time it's the controller's priority
  -- (the "just assigned" window isn't expressible without a real
  -- action stack). We guard the redirect on actually having a
  -- cancellable pending-damage assignment on self in the current
  -- combat — outside combat (or when nothing's been assigned to
  -- self) the action is a no-op rather than a free "deal 1 to a
  -- target" exploit.
  action "Redirect blow" 2 \usage -> do
    g <- getGame
    let pendingOnSelf = case g.combat of
          Just cs ->
            sum
              [ pd.cancellable
              | pd <- cs.pendingAssignments
              , PDUnit k <- [pd.target]
              , k == usage.self.key
              ]
          Nothing -> 0
    when (pendingOnSelf > 0) $
      withTarget usage.user AnyUnit \target -> do
        push (CancelAssignedDamageOnUnit usage.self.key 1)
        dealDamage target 1

vileSorceress :: CardDef Unit
vileSorceress = unitCard "core-107" "Vile Sorceress" do
  race DarkElf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Sorceror
  body "Quest. Forced: After your turn begins, one target unit gets -1 hit points until the end of the turn."
  onMyTurnBegin \_owner self ->
    when (self.zone == QuestZone) $
      withTarget self.controller AnyUnit \k ->
        until EndOfTurn $ debuffHP k 1

coldOneRiders :: CardDef Unit
coldOneRiders = unitCard "core-108" "Cold One Riders" do
  race DarkElf
  cost 4
  loyalty 1
  power 2
  hitPoints 3
  traits [Cavalry, Elite]
  counterstrike 1
  body "Counterstrike 1 (this unit deals 1 combat damage immediately after defending)."

cauldronOfBlood :: CardDef Support
cauldronOfBlood = supportCard "core-109" "Cauldron of Blood" do
  race DarkElf
  cost 4
  loyalty 1
  power 2
  trait Siege
  body "Kingdom. Forced: When this zone is attacked, deal 1 damage to one target attacking unit."
  onMyZoneAttacked \_owner self cs ->
    case cs.attackers of
      [] -> pure ()
      _ ->
        withTarget self.controller
          (UnitMatching \_ _ u -> u.key `elem` cs.attackers)
          (`dealDamage` 1)

hate :: CardDef Tactic
hate = tacticCard "core-110" "Hate" do
  race DarkElf
  cost 0
  loyalty 1
  body "Action: Take 1 resource from each opponent and add it to your available resources."
  whenResolved \self -> do
    let pk = self.controller
        opp = pk.next
    -- Spend 1 from opponent (clamps at 0), gain 1 to self per opp.
    push (SpendResources opp 1)
    push (GainResources pk 1)

-- The Corruption cycle ------------------------------------------------

malusDarkblade :: CardDef Unit
malusDarkblade = unitCard "the-skavenblight-threat-012" "Malus Darkblade" do
  hero
  race DarkElf
  cost 5
  loyalty 4
  power 3
  hitPoints 3
  body
    "Limit one Hero per zone. At the end of your battlefield phase, deal 1 damage to each \
    \of your opponent's units that could have defended but did not."
  onMyPhaseEnd BattlefieldPhase \_owner self -> do
    g <- getGame
    let h = Map.findWithDefault mempty ThisPhase g.history
        myAttacks =
          [ (rec.defender, rec.zone, rec.defenderKeys)
          | rec <- h.combats
          , rec.attacker == self.controller
          ]
        defended = concat [ks | (_, _, ks) <- myAttacks]
        couldNot u =
          u.corrupted
            || (not u.blanked && u.cardDef.extras.cannotDefend)
            || any
              (\m -> m.details == CannotDefend)
              (Map.findWithDefault [] (UnitRef u.key) g.modifiers)
        shirkers =
          [ u.key
          | u <- g.units
          , u.controller == self.controller.next
          , any (\(d, z, _) -> d == u.controller && z == u.zone) myAttacks
          , u.key `notElem` defended
          , not (couldNot u)
          ]
    for_ shirkers \k -> dealDamage k 1

morathisPegasus :: CardDef Unit
morathisPegasus = unitCard "the-skavenblight-threat-013" "Morathi's Pegasus" do
  race DarkElf
  cost 3
  loyalty 4
  power 1
  hitPoints 3
  trait Cavalry
  toughness 3
  body
    "Toughness 3 (whenever this unit is assigned damage, cancel 3 of that damage). \
    \Action: Spend 3 resources to have this unit lose all Toughness until the end of the \
    \turn. Only an opponent may trigger this ability."
  actionOpponent "Ground the pegasus" 3 \usage ->
    until EndOfTurn $ loseAllToughness usage.self.key

weNeedYourBlood :: CardDef Tactic
weNeedYourBlood = tacticCard "the-skavenblight-threat-014" "We Need Your Blood" do
  race DarkElf
  cost 1
  loyalty 1
  body
    "Action: One target unit gets -1 hit points and another target unit gets +1 hit points \
    \until the end of the turn."
  playableWhen \g _pk -> length g.units >= 2
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \loser -> do
      until EndOfTurn $ debuffHP loser 1
      withTarget pk (unitWhere \u -> u.key /= loser) \winner ->
        until EndOfTurn $ buffHP winner 1

darkRiders :: CardDef Unit
darkRiders = unitCard "path-of-the-zealot-033" "Dark Riders" do
  race DarkElf
  cost 4
  loyalty 1
  power 1
  hitPoints 4
  trait Cavalry
  body "This unit gains {power} while any unit in play is corrupted."
  effects \self _owner -> do
    g <- getGame
    when (any (.corrupted) g.units) $ gainPower self 1

callTheBlood :: CardDef Tactic
callTheBlood = tacticCard "path-of-the-zealot-034" "Call the Blood" do
  race DarkElf
  cost 1
  loyalty 2
  trait Spell
  body "Action: Destroy one target damaged unit."
  playableWhen $ hasTarget (unitWhere isDamaged)
  whenResolved \self ->
    withTarget self.controller (unitWhere isDamaged) destroyUnit

coldOneChariot :: CardDef Unit
coldOneChariot = unitCard "tooth-and-claw-053" "Cold One Chariot" do
  race DarkElf
  cost 3
  loyalty 2
  power 2
  hitPointsX
  trait Cavalry
  body "X is the number of developments in this zone."
  selfHP \g u -> devsInZone g u

graspingDarkness :: CardDef Tactic
graspingDarkness = tacticCard "tooth-and-claw-054" "Grasping Darkness" do
  race DarkElf
  cost 3
  loyalty 2
  trait Spell
  body
    "Action: Until the end of the turn, take control of target unit with printed cost 2 or \
    \lower. Move the unit to your corresponding zone."
  playableWhen $ hasTarget cheapEnemy
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk cheapEnemy \k ->
      withUnit k \u -> do
        push (TakeControlOfUnit pk k)
        push (ScheduleControlReturn k u.controller)
  where
    cheapEnemy = UnitMatching \me _ u ->
      u.controller /= me && costAtMost 2 u.cardDef

warHydra :: CardDef Unit
warHydra = unitCard "the-deathmaster-s-dance-076" "War Hydra" do
  race DarkElf
  cost 5
  loyalty 1
  power 2
  hitPoints 1
  trait Creature
  body
    "Place 5 resource tokens on this unit when it enters play. Action: Remove a resource \
    \token from this unit to cancel 1 damage assigned to it. Then, add 1 resource to your pool."
  onEnterPlay \_owner self -> push (AdjustUnitTokens self.key 5)
  action "Regrow a head" 0 \usage -> do
    g <- getGame
    whenJust (findUnit usage.self.key g) \u -> do
      let pending = case g.combat of
            Just cs ->
              sum
                [ pd.cancellable
                | pd <- cs.pendingAssignments
                , pd.target == PDUnit u.key
                ]
            Nothing -> 0
      when (u.tokens > 0 && pending > 0) do
        push (AdjustUnitTokens u.key (-1))
        push (CancelAssignedDamageOnUnit u.key 1)
        gainResources usage.user 1

reaperBoltThrower :: CardDef Support
reaperBoltThrower = supportCard "the-deathmaster-s-dance-077" "Reaper Bolt Thrower" do
  race DarkElf
  cost 2
  loyalty 2
  trait Siege
  body
    "Battlefield. Action: Spend 2 resources to deal 2 indirect damage to each opponent. \
    \(Players assign their own indirect damage.)"
  battlefield $ action "Reap" 2 \usage ->
    indirectDamage usage.user.next 2

caughtTheScent :: CardDef Tactic
caughtTheScent = tacticCard "the-deathmaster-s-dance-078" "Caught the Scent" do
  race DarkElf
  cost 2
  loyalty 3
  body
    "Play during your turn. Action: Look at one target opponent's hand. You may choose and \
    \discard one card from that hand."
  playableWhen \g pk -> g.currentPlayer == pk
  whenResolved \self -> do
    let pk = self.controller
        opp = pk.next
    oppPlayer <- playerOf opp <$> getGame
    chooseFromCards pk 0 1 oppPlayer.hand
      "Caught the Scent: the opponent's hand — discard one card (or none)." \chosen ->
        unless (null chosen) $
          push (DiscardCardsFromHand opp (map (.key) chosen))

naggarothSpearmen :: CardDef Unit
naggarothSpearmen = unitCard "the-warpstone-chronicles-096" "Naggaroth Spearmen" do
  race DarkElf
  cost 3
  loyalty 3
  power 1
  hitPoints 2
  trait Warrior
  body
    "Battlefield. Action: Spend X resources to have this unit deal +X damage in combat \
    \until the end of the turn. X is the number of developments in this zone."
  battlefield $ action "Wall of spears" 0 \usage -> do
    g <- getGame
    whenJust (findUnit usage.self.key g) \u -> do
      let x = devsInZone g u
          Resources r = (playerOf usage.user g).resources
      when (x > 0 && r >= x) do
        payResources usage.user x
        until EndOfTurn $ buffCombatDamage u.key x

hydraBlade :: CardDef Support
hydraBlade = supportCard "the-warpstone-chronicles-097" "Hydra Blade" do
  unique
  race DarkElf
  cost 2
  loyalty 1
  traits [Attachment, Relic]
  body
    "Attach to a target {darkelf} unit. Corrupt that unit. Attached unit gains \
    \{power}{power}. If attached unit would be destroyed, you may pay 2 resources to \
    \(instead of destroying it) leave it in play and remove all damage from it."
  attachmentPower 2
  onEnterPlay \_owner self -> for_ self.attachedTo corrupt
  hostDestroyRansomOf 2

slaverRaid :: CardDef Quest
slaverRaid = questCard "the-warpstone-chronicles-098" "Slaver Raid" do
  race DarkElf
  cost 1
  loyalty 3
  trait QuestTrait
  body
    "Quest. Action: Discard 3 resource tokens from this card to put a unit from an \
    \opponent's discard pile into play, corrupt, in your quest zone. \
    \Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  spendTokens "Raid for slaves" 3 \usage -> do
    let pk = usage.user
        opp = pk.next
    oppPlayer <- playerOf opp <$> getGame
    let units = [c | c <- oppPlayer.discard, isJust (asUnit c.def)]
    chooseFromCards pk 0 1 units
      "Choose a unit from the opponent's discard pile to enslave." \chosen ->
        for_ chosen \c ->
          push (StealUnitFromDiscard pk opp c.key QuestZone True)

slaveDriver :: CardDef Unit
slaveDriver = unitCard "arcane-fire-116" "Slave Driver" do
  race DarkElf
  cost 2
  loyalty 4
  power 1
  hitPoints 1
  trait Warrior
  body
    "Kingdom. Action: Spend 2 resources to choose one target unit with printed cost 2 or \
    \lower. That unit cannot attack or defend this turn."
  kingdom $ action "Crack the whip" 2 \usage ->
    withTarget usage.user (unitWhere \u -> costAtMost 2 u.cardDef) \k -> do
      until EndOfTurn $ disableAttack k
      until EndOfTurn $ disableDefend k

yourWillIsMine :: CardDef Tactic
yourWillIsMine = tacticCard "arcane-fire-117" "Your Will Is Mine" do
  race DarkElf
  cost 10
  loyalty 3
  traits [Epic, Spell]
  body
    "Play only during your turn. Action: Choose a zone. Take control of each opponent's \
    \units in that zone. (Those units move to your corresponding zone.)"
  playableWhen \g pk -> g.currentPlayer == pk
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk MyAnyZone \zk -> do
      g <- getGame
      for_ [u.key | u <- g.units, u.controller /= pk, u.zone == zk] \k ->
        push (TakeControlOfUnit pk k)

witchHagsCurse :: CardDef Support
witchHagsCurse = supportCard "arcane-fire-118" "Witch Hag's Curse" do
  race DarkElf
  cost 1
  loyalty 2
  traits [Attachment, Hex]
  body "Attach to a target unit. Treat attached unit as though its printed text box were blank (except for Traits)."
  blanksAttachedUnit

-- Days of Blood --------------------------------------------------------

chillSeaWatchtower :: CardDef Support
chillSeaWatchtower = supportCard "days-of-blood-004" "Chill Sea Watchtower" do
  race DarkElf
  cost 1
  loyalty 1
  power 1
  trait Building
  body "If you control a non-[Dark Elf] card, sacrifice this card."
  sacrificeIfControlsOffFaction DarkElf

-- Oaths of Vengeance ---------------------------------------------------

vaedraBloodsworn :: CardDef Unit
vaedraBloodsworn = unitCard "oaths-of-vengeance-035" "Vaedra Bloodsworn" do
  unique
  race DarkElf
  cost 3
  loyalty 2
  power 0
  hitPoints 3
  traits [Warrior]
  body
    "Action: When this unit attacks or defends, discard the top card of target opponent's \
    \deck. This unit gains {power} equal to the cost of the discarded card until the end of \
    \the phase."
  onMyAttackOrDefend \_owner self -> drainTopCard self
  where
    drainTopCard :: TriggerM m => UnitDetails -> m ()
    drainTopCard self = do
      let opp = self.controller.next
      oppPlayer <- playerOf opp <$> getGame
      case oppPlayer.deck of
        [] -> pure ()
        (top : _) -> do
          millFromDeck opp 1
          let c = someCardCost top.def
          when (c > 0) $ until EndOfTurn $ buffPower self.key c

-- Glory of Days Past ---------------------------------------------------

markedForDeath :: CardDef Support
markedForDeath = supportCard "glory-of-days-past-078" "Marked for Death" do
  race DarkElf
  cost 0
  loyalty 2
  trait Attachment
  body
    "Attach to a target unit. When attached unit leaves play, attached unit's controller \
    \must discard X cards from the top of his deck. X is the attached unit's cost."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitLeftPlay du
      | Just du.key == self.attachedTo ->
          let x = case du.cardDef.cost of
                Fixed n -> n
                Variable -> 0
           in when (x > 0) $ millFromDeck du.controller x
    _ -> pure ()

hagGraefKnights :: CardDef Unit
hagGraefKnights = unitCard "oaths-of-vengeance-036" "Hag Graef Knights" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Cavalry
  raider 2
  body "Raider 2."

coldOneChampion :: CardDef Unit
coldOneChampion = unitCard "the-ruinous-hordes-096" "Cold One Champion" do
  race DarkElf
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  trait Cavalry
  raider 2
  scout
  body "Raider 2. Scout."

-- Bloodquest: Rising Dawn -----------------------------------------------

towerOfOblivion :: CardDef Support
towerOfOblivion = supportCard "rising-dawn-015" "Tower of Oblivion" do
  race DarkElf
  cost 2
  loyalty 2
  power 1
  body
    "Quest. Action: Discard the top card of your deck to have target unit lose {power} until \
    \the end of the turn. Then, put 1 resource token on this card. (Limit once per turn)."
  quest $ action "Tower of Oblivion" 0 \usage -> do
    g <- getGame
    let used =
          any (\m -> m.details == ActionUsedThisTurn)
            (Map.findWithDefault [] (UnitRef usage.self.key) g.modifiers)
    unless used $
      withTarget usage.user AnyUnit \k -> do
        until EndOfTurn (PendingBuff usage.self.key ActionUsedThisTurn)
        millFromDeck usage.user 1
        until EndOfTurn $ buffPower k (-1)
        adjustSupportTokens usage.self.key 1

-- Bloodquest: The Accursed Dead -----------------------------------------

treasureThieves :: CardDef Unit
treasureThieves = unitCard "the-accursed-dead-053" "Treasure Thieves" do
  race DarkElf
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  trait Sorceror
  body
    "Action: When this unit enters play, discard the top card of your deck to discard the top \
    \card of target opponent's deck. Gain resources equal to the difference in printed cost \
    \between the discarded cards."
  onEnterPlay \owner self -> do
    let pk = self.controller
        opp = pk.next
    oppP <- playerOf opp <$> getGame
    case (owner.deck, oppP.deck) of
      (mine : _, theirs : _) -> do
        millFromDeck pk 1
        millFromDeck opp 1
        let diff = abs (someCardCost mine.def - someCardCost theirs.def)
        when (diff > 0) $ gainResources pk diff
      _ -> pure ()

-- Bloodquest: Vessel of the Winds ---------------------------------------

templeOfSpite :: CardDef Support
templeOfSpite = supportCard "vessel-of-the-winds-075" "Temple of Spite" do
  race DarkElf
  cost 2
  loyalty 2
  power 1
  body
    "Quest. Action: Discard the top card of your deck to have target unit get -1 hit point \
    \until the end of the turn. Then, put 1 resource token on this card. (Limit once per turn.)"
  quest $ action "Temple of Spite" 0 \usage -> do
    g <- getGame
    let used =
          any (\m -> m.details == ActionUsedThisTurn)
            (Map.findWithDefault [] (UnitRef usage.self.key) g.modifiers)
    unless used $
      withTarget usage.user AnyUnit \k -> do
        until EndOfTurn (PendingBuff usage.self.key ActionUsedThisTurn)
        millFromDeck usage.user 1
        until EndOfTurn $ debuffHP k 1
        adjustSupportTokens usage.self.key 1

-- Bloodquest: Portent of Doom -------------------------------------------

murderlust :: CardDef Tactic
murderlust = tacticCard "portent-of-doom-093" "Murderlust" do
  race DarkElf
  cost 0
  loyalty 2
  body "Action: Sacrifice a unit to restore up to 2 target units."
  playableWhen \g pk -> any (\u -> u.controller == pk) g.units
  whenResolved \self ->
    sacrificeOwnUnit self.controller "Murderlust: sacrifice a unit." \_k -> do
      corrupted <- unitsMatching self.controller (unitWhere (.corrupted))
      chooseUpTo self.controller 2 (map (.key) corrupted) (traverse_ (push . CleanseUnit))

-- The Capital Cycle ----------------------------------------------------

harpyAerie :: CardDef Support
harpyAerie = supportCard "city-of-winter-093" "Harpy Aerie" do
  race DarkElf
  cost 2
  loyalty 2
  power 0
  trait Fortification
  body
    "Action: When this zone is attacked, target attacking unit gets -2 hit points \
    \until the end of the turn."
  onMyZoneAttacked \_owner self cs ->
    case cs.attackers of
      [] -> pure ()
      _ ->
        withTarget self.controller
          (UnitMatching \_ _ u -> u.key `elem` cs.attackers)
          \k -> until EndOfTurn (debuffHP k 2)

raidingShips :: CardDef Quest
raidingShips = questCard "city-of-winter-100" "Raiding Ships" do
  race DarkElf
  cost 0
  loyalty 3
  body
    "Quest. Action: When this card enters play, draw a card. Quest. Action: When you play \
    \a {darkelf} non-Attachment support card from your hand, discard a card at random from \
    \an opponent's hand if a unit is questing here."
  onEnterPlay \_owner self -> drawCard self.controller
  onQuestSupportPayoff DarkElf \self -> discardRandom self.controller.next

callOfTheKraken :: CardDef Tactic
callOfTheKraken = tacticCard "city-of-winter-095" "Call of the Kraken" do
  race DarkElf
  cost 0
  loyalty 3
  body
    "Action: Discard a card from your hand with X loyalty to put a {darkelf} unit with \
    \printed cost X or lower into play from your hand."
  playableWhen \g pk -> not (null (playerOf pk g).hand)
  whenResolved \self -> do
    let pk = self.controller
    discardForLoyalty pk \x -> do
      me <- playerOf pk <$> getGame
      let isCand c = case c.def of
            UnitCardDef cd -> DarkElf `elem` cd.races && someCardCost c.def <= x
            _ -> False
          cands = filter isCand me.hand
      chooseFromCards pk 0 1 cands
        "Call of the Kraken: put a Dark Elf unit (cost X or lower) into play." \chosen ->
        for_ chosen \c -> putUnitIntoPlay pk FromHand c.key BattlefieldZone

bannermanOfTheCrag :: CardDef Unit
bannermanOfTheCrag = unitCard "city-of-winter-087" "Bannerman of the Crag" do
  race DarkElf
  cost 2
  loyalty 1
  power 0
  hitPoints 2
  trait StandardBearer
  body "Action: When a unit enters this zone, discard the top card of target player's deck."
  -- "target player": the opponent, the only meaningful pick.
  onUnitEnterMyZone \_owner self _uk -> millFromDeck self.controller.next 1

anlecLookout :: CardDef Unit
anlecLookout = unitCard "city-of-winter-090" "Anlec Lookout" do
  race DarkElf
  cost 3
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  body "Counterstrike X. X is the highest loyalty on a [Dark Elf] card you control."
  counterstrikeX \g u -> highestLoyaltyControlled DarkElf g u.controller

sacrificialPyre :: CardDef Support
sacrificialPyre = supportCard "the-imperial-throne-115" "Sacrificial Pyre" do
  race DarkElf
  cost 2
  loyalty 2
  power 0
  trait Location
  body "Action: When you sacrifice a unit, corrupt target unit."
  -- Approximation: fires on any friendly unit leaving play, not only
  -- sacrifices — 'DepartedUnit' carries no reason field yet.
  -- TODO: gate on a sacrifice reason once UnitLeftPlay distinguishes
  -- sacrifice from death / return-to-hand.
  onFriendlyUnitLeavePlay \_owner self _uk _zone _code ->
    withTarget self.controller AnyUnit (push . CorruptUnit)

-- Cataclysm cycle ------------------------------------------------------

crimsonBrides :: CardDef Unit
crimsonBrides = unitCard "cataclysm-042" "Crimson Brides" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait WitchElf
  body
    "Action: Sacrifice this unit to discard 1 card at random from each \
    \opponent's hand."
  actionWith "Sacrifice" 0 [SacrificeSelf] \usage ->
    eachPlayer \pk ->
      when (pk /= usage.self.controller) $ discardRandom pk

-- The Morrslieb cycle ---------------------------------------------------

frenziedWitchElf :: CardDef Unit
frenziedWitchElf = unitCard "the-chaos-moon-035" "Frenzied Witch Elf" do
  race DarkElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait WitchElf
  body "Action: When this unit attacks, discard the top 2 cards of target player's deck."
  -- TODO: approximation — always mills the opponent. The printed card
  -- lets the controller choose "target player" (including themselves);
  -- prompt for the player once a target-player picker exists.
  onMyAttackDeclared \_owner self _z _atk ->
    millFromDeck self.controller.next 2

sacrificeToKhaine :: CardDef Tactic
sacrificeToKhaine = tacticCard "the-chaos-moon-037" "Sacrifice to Khaine" do
  race DarkElf
  cost 2
  loyalty 2
  trait Spell
  body "Action: Each opponent must sacrifice a unit he controls."
  whenResolved \self -> do
    let opp = self.controller.next
    cands <- unitsMatching opp ownUnit
    forcePickUnit opp (map (.key) cands) "Sacrifice a unit." destroyUnit

witchHag :: CardDef Unit
witchHag = unitCard "omens-of-ruin-015" "Witch Hag" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Sorceror
  body "Action: Corrupt this unit to discard the top card of target player's deck."
  -- TODO: approximation — always mills the opponent. The printed card
  -- lets the controller choose "target player"; prompt for the player
  -- once a target-player picker exists.
  actionWith "Hex" 0 [CorruptSelf] \usage ->
    millFromDeck usage.user.next 1

darkElfInfiltrator :: CardDef Unit
darkElfInfiltrator = unitCard "omens-of-ruin-016" "Dark Elf Infiltrator" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Action: When this unit enters play, take up to 2 resources from target opponent."
  onEnterPlay \_owner self -> do
    let pk = self.controller
        opp = pk.next
    g <- getGame
    let Resources r = (playerOf opp g).resources
        n = min 2 r
    when (n > 0) $ do
      payResources opp n
      gainResources pk n

toxicHydra :: CardDef Unit
toxicHydra = unitCard "the-eclipse-of-hope-095" "Toxic Hydra" do
  race DarkElf
  cost 5
  loyalty 3
  power 2
  hitPoints 4
  trait Creature
  body
    "Action: When this unit enters play, each unit in any corresponding zone gets \
    \-2 hit points until the end of the turn."
  -- TODO: interpretation — "each unit in any corresponding zone" is
  -- treated as the opponent's matching zone only. Confirm whether the
  -- Hydra's own zone (friendly units) should also be hit, and widen the
  -- filter if so.
  onEnterPlay \_owner self -> do
    g <- getGame
    let opp = self.controller.next
    for_ [u | u <- g.units, u.controller == opp, u.zone == self.zone] \u ->
      until EndOfTurn $ debuffHP u.key 2

enragedManticore :: CardDef Unit
enragedManticore = unitCard "signs-in-the-stars-076" "Enraged Manticore" do
  race DarkElf
  cost 6
  loyalty 3
  power 3
  hitPoints 5
  trait Creature
  body
    "While attacking, this unit gains {power}{power}{power} if there are 3 or more \
    \developments in the defending zone."
  combatPower \g self -> case g.combat of
    Just cs
      | self.key `elem` cs.attackers ->
          let p = playerOf cs.defendingPlayer g
              Developments d = case cs.targetZone of
                KingdomZone -> p.capital.kingdom.developments
                QuestZone -> p.capital.quest.developments
                BattlefieldZone -> p.capital.battlefield.developments
           in if d >= 3 then 3 else 0
    _ -> 0

bloodcallSorceress :: CardDef Unit
bloodcallSorceress = unitCard "the-twin-tailed-comet-055" "Bloodcall Sorceress" do
  race DarkElf
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Sorceror
  body
    "Action: When you play a development from your hand, put a resource token on \
    \this unit. Then, up to X target units lose {power}{power} until the end of \
    \the turn. X is the number of resource tokens on this unit."
  onYouPlayDevelopment \_owner self -> do
    push (AdjustUnitTokens self.key 1)
    let n = self.tokens + 1
    withUpTo self.controller n (unitWhere (const True)) \ks ->
      for_ ks \k -> until EndOfTurn $ buffPower k (-2)

malekithsRage :: CardDef Quest
malekithsRage = questCard "the-eclipse-of-hope-097" "Malekith's Rage" do
  race DarkElf
  cost 0
  loyalty 2
  body
    "Action: When you play a development from your hand, put a resource token on \
    \this card if a unit is questing here. Action: Discard 2 resources on this \
    \card to have target unit get -1 hit points until the end of the turn."
  accrueTokenOnDevelopmentWhileQuesting
  spendTokens "Weaken" 2 \u ->
    withTarget u.user AnyUnit \k -> until EndOfTurn $ debuffHP k 1

anointedCauldron :: CardDef Support
anointedCauldron = supportCard "the-twin-tailed-comet-057" "Anointed Cauldron" do
  race DarkElf
  cost 3
  loyalty 1
  power 1
  trait Siege
  body "Action: When this zone is attacked, the attacking player discards a card from his hand."
  onMyZoneAttacked \_owner _self cs -> do
    let atk = cs.attackingPlayer
    g <- getGame
    let h = (playerOf atk g).hand
    unless (null h) $
      chooseFromCards atk 1 1 h "Anointed Cauldron: discard a card." \case
        [c] -> push (DiscardCardsFromHand atk [c.key])
        _ -> pure ()

-- The Enemy cycle -------------------------------------------------------

druchiiNoble :: CardDef Unit
druchiiNoble = unitCard "the-burning-of-derricksburg-016" "Druchii Noble" do
  race DarkElf
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  traits [Elite, Noble]
  body "Action: When this unit attacks, it gains {power} and target unit loses {power} until the end of the turn."
  onMyAttackDeclared \_owner self _zone _attackers -> do
    until EndOfTurn $ buffPower self.key 1
    withTarget self.controller enemyUnit \k -> until EndOfTurn $ buffPower k (-1)

bloodburnPoison :: CardDef Tactic
bloodburnPoison = tacticCard "the-fourth-waystone-093" "Bloodburn Poison" do
  race DarkElf
  cost 1
  loyalty 2
  body "Action: Deal 1 damage to target unit. That unit loses {power} until the end of the turn."
  playableWhen \g pk -> hasTarget AnyUnit g pk
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> do
      dealDamage k 1
      until EndOfTurn $ buffPower k (-1)

whipTheSlaves :: CardDef Tactic
whipTheSlaves = tacticCard "the-burning-of-derricksburg-017" "Whip the Slaves" do
  race DarkElf
  cost 0
  loyalty 2
  body "Action: Sacrifice a unit to draw 2 cards."
  playableWhen \g pk -> any (\u -> u.controller == pk) g.units
  whenResolved \self ->
    sacrificeOwnUnit self.controller "Sacrifice a unit." \_ ->
      drawCards self.controller 2

vanguardOfWoe :: CardDef Unit
vanguardOfWoe = unitCard "the-fall-of-karak-grimaz-036" "Vanguard of Woe" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Action: When this unit leaves play, target opponent discards a card from his hand."
  onSelfLeavesPlay \_owner self -> discardRandom self.controller.next

dwarfSlaves :: CardDef Unit
dwarfSlaves = unitCard "the-silent-forge-054" "Dwarf Slaves" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Slave
  body "Action: When this unit enters play, put the top card of your deck into this zone as a development."
  onEnterPlay \_owner self -> addDevelopment self.controller self.zone

blackGuardAspirant :: CardDef Unit
blackGuardAspirant = unitCard "the-silent-forge-055" "Black Guard Aspirant" do
  race DarkElf
  cost 4
  loyalty 1
  power 2
  hitPoints 3
  traits [Elite, Warrior]
  body "This unit gains {power} while any unit has one or more cards attached to it."
  selfPower \g _self -> if any (\u -> not (null u.attachments)) g.units then 1 else 0

barbedWhip :: CardDef Support
barbedWhip = supportCard "the-fourth-waystone-095" "Barbed Whip" do
  race DarkElf
  cost 1
  loyalty 3
  traits [Attachment, Weapon]
  body "Attach to a target [Dark Elf] unit. Action: When attached unit attacks, target unit in the defending zone gets -1 hit points until the end of the turn."
  onAttachedHostAttack \_owner self _host -> do
    g <- getGame
    for_ g.combat \cs ->
      withTarget self.controller
        (UnitMatching \_pk _g u -> u.controller == cs.defendingPlayer && u.zone == cs.targetZone)
        \k -> until EndOfTurn $ debuffHP k 1

standardOfClarKarond :: CardDef Support
standardOfClarKarond = supportCard "the-silent-forge-056" "Standard of Clar Karond" do
  race DarkElf
  cost 1
  loyalty 3
  trait Attachment
  body "Attach to a target [Dark Elf] unit in your battlefield. Action: When attached unit attacks, target unit in the defending zone loses {power} until the end of the turn."
  onAttachedHostAttack \_owner self _host -> do
    g <- getGame
    for_ g.combat \cs ->
      withTarget self.controller
        (UnitMatching \_pk _g u -> u.controller == cs.defendingPlayer && u.zone == cs.targetZone)
        \k -> until EndOfTurn $ buffPower k (-1)

-- Assault on Ulthuan ---------------------------------------------------

darkInitiate :: CardDef Unit
darkInitiate = unitCard "assault-on-ulthuan-022" "Dark Initiate" do
  race DarkElf
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  traits [Initiate, Priest]
  body "This unit does not count its power unless you have at least 2 developments in this zone."
  selfPower \g self ->
    if devsInZone g self >= 2 then 0 else negate self.cardDef.power

walkingSacrifice :: CardDef Unit
walkingSacrifice = unitCard "assault-on-ulthuan-023" "Walking Sacrifice" do
  race DarkElf
  cost 0
  loyalty 1
  power 0
  hitPoints 1
  trait Martyr
  body "Forced: When this unit leaves play, draw a card."
  onSelfLeavesPlay \_owner self -> drawCard self.controller

shades :: CardDef Unit
shades = unitCard "assault-on-ulthuan-025" "Shades" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  scout
  body "Scout (discard one card at random from an opponent's hand if this unit survives combat)."

coldOneKnight :: CardDef Unit
coldOneKnight = unitCard "assault-on-ulthuan-026" "Cold One Knight" do
  race DarkElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Warrior
  toughness 1
  body "Toughness 1 (whenever this unit is assigned damage, cancel 1 of that damage)."

darkSorceress :: CardDef Unit
darkSorceress = unitCard "assault-on-ulthuan-028" "Dark Sorceress" do
  race DarkElf
  cost 4
  loyalty 2
  power 2
  hitPoints 2
  trait Sorceror
  body "Reduce the cost to play this unit by 1 for each corrupted unit controlled by your opponents."
  selfCostAdjust \g pk ->
    negate (length [u | u <- g.units, u.controller /= pk, u.corrupted])

corsairsOfGhrond :: CardDef Unit
corsairsOfGhrond = unitCard "assault-on-ulthuan-029" "Corsairs of Ghrond" do
  race DarkElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Warrior
  body "Battlefield. Forced: When this unit leaves play, one target unit gets -2 hit points until the end of the turn."
  battlefield $ onSelfLeavesPlay \_owner self ->
    withTarget self.controller AnyUnit \k -> until EndOfTurn $ debuffHP k 2

lokhirFellheart :: CardDef Unit
lokhirFellheart = unitCard "assault-on-ulthuan-030" "Lokhir Fellheart" do
  hero
  trait Warrior
  race DarkElf
  cost 5
  loyalty 5
  power 3
  hitPoints 4
  body
    "Limit 1 Hero per zone. Battlefield. Action: At the beginning of your turn, one target unit \
    \in any battlefield gets -1 hit points until the end of the turn for each development in this zone."
  battlefield $ onMyTurnBegin \_owner self -> do
    g <- getGame
    let n = devsInZone g self
    when (n > 0) $
      withTarget self.controller
        (UnitMatching \_pk _g u -> u.zone == BattlefieldZone)
        \k -> until EndOfTurn $ debuffHP k n

harGaneth :: CardDef Support
harGaneth = supportCard "assault-on-ulthuan-035" "Har Ganeth" do
  race DarkElf
  cost 2
  loyalty 1
  power 0
  trait Building
  body "Kingdom. Action: At the beginning of your turn, return one target unit with less than 2 hit points to its owner's hand."
  kingdom $ onMyTurnBegin \_owner self ->
    withTarget self.controller (unitWhere (\u -> u.effectiveMaxHP < 2)) returnUnitToHand

lashThePrisoner :: CardDef Tactic
lashThePrisoner = tacticCard "assault-on-ulthuan-037" "Lash the Prisoner!" do
  race DarkElf
  cost 0
  loyalty 3
  body "Action: Sacrifice a unit. If you do, gain 2 resources."
  playableWhen \g pk -> any (\u -> u.controller == pk) g.units
  whenResolved \self ->
    sacrificeOwnUnit self.controller "Sacrifice a unit to gain 2 resources." \_ ->
      gainResources self.controller 2

darkVisions :: CardDef Tactic
darkVisions = tacticCard "assault-on-ulthuan-038" "Dark Visions" do
  race DarkElf
  cost 1
  loyalty 2
  trait Spell
  body "Play during your turn. Action: Search the top 5 cards of your deck for a card and put it into your hand. Then, shuffle your deck."
  playableWhen \g pk -> g.currentPlayer == pk
  whenResolved \self -> do
    let pk = self.controller
    searchTopOfDeck pk 5 \result -> do
      chooseFromCards pk 0 1 result.cards "Choose a card to add to your hand." \chosen ->
        for_ chosen \c -> push (TakeCardsFromDeckToHand pk [c.key])
      shuffleDeck pk

chillwind :: CardDef Tactic
chillwind = tacticCard "assault-on-ulthuan-039" "Chillwind" do
  race DarkElf
  cost 1
  loyalty 1
  trait Spell
  body "Action: Corrupt one target unit. Then, you may restore one corrupted unit."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit corrupt
    may pk "Restore one corrupted unit?" $
      push (RestoreOneCorruptCard pk)

mindKiller :: CardDef Support
mindKiller = supportCard "assault-on-ulthuan-033" "Mind Killer" do
  race DarkElf
  cost 0
  loyalty 2
  traits [Attachment, Hex]
  body "Attach to a target unit. Attached unit loses {power}."
  attachmentPower (-1)

nightRaids :: CardDef Quest
nightRaids = questCard "assault-on-ulthuan-031" "Night Raids" do
  race DarkElf
  cost 2
  loyalty 2
  body
    "Quest. While this quest has 3 or more resource tokens on it, each of your units gain {power}. \
    \Quest. Forced: Place 1 resource token on this card at the beginning of your turn if there is a \
    \unit questing here."
  forced accrueTokenWhileQuesting
  questUnitAura \_g self u ->
    if self.tokens >= 3 && u.controller == self.controller then 1 else 0

sackTorAendris :: CardDef Quest
sackTorAendris = questCard "assault-on-ulthuan-032" "Sack Tor Aendris" do
  race DarkElf
  cost 2
  loyalty 1
  body
    "Quest. Any unit questing here may attack as though it were in your battlefield. \
    \Quest. You may spend resources on this quest to pay for cards and effects. \
    \Quest. Forced: At the end of any turn in which the questing unit participated in an attack, \
    \place a resource token on this card."
  -- Partial: the "spend resources on this quest to pay for cards and
  -- effects" clause is not modelled (the engine only supports a quest
  -- paying for Attachments, via 'paysAttachmentCosts'); tokens accrue
  -- but cannot yet be spent as generic resources.
  questerAttacksAnywhere
  onMyTurnEnd \_owner self ->
    withQuest self.key \q ->
      whenJust q.questingUnit \uk ->
        withHistory ThisTurn \h ->
          when
            ( any
                (\rec -> rec.attacker == self.controller && uk `elem` rec.attackerKeys)
                h.combats
            )
            (addQuestToken self.key 1)

-- March of the Damned --------------------------------------------------

seasonedCorsair :: CardDef Unit
seasonedCorsair = unitCard "march-of-the-damned-026" "Seasoned Corsair" do
  race DarkElf
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Warrior
  body "Action: When this unit enters play, target unit gets -2 hit points until the end of the turn."
  onEnterPlay \_owner self ->
    withTarget self.controller AnyUnit \k -> until EndOfTurn $ debuffHP k 2

blackDragonRider :: CardDef Unit
blackDragonRider = unitCard "march-of-the-damned-027" "Black Dragon Rider" do
  race DarkElf
  cost 5
  loyalty 3
  power 3
  hitPoints 4
  traits [Warrior, Elite]
  body
    "Forced: When this unit is opposed in combat, all attacking and defending units get -1 \
    \hit points until the end of the turn."
  onReceive $ Receive \msg _owner self -> case msg of
    DeclareDefenders ks -> do
      g <- getGame
      case g.combat of
        Just cs
          | (self.key `elem` cs.attackers && not (null ks)) || self.key `elem` ks ->
              for_ (cs.attackers <> ks) \k -> until EndOfTurn $ debuffHP k 1
        _ -> pure ()
    _ -> pure ()

corsairRaider :: CardDef Support
corsairRaider = supportCard "march-of-the-damned-028" "Corsair Raider" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  trait Ship
  body "Action: At the beginning of your turn, target unit loses {power} until the end of the turn."
  onMyTurnBegin \_owner self ->
    withTarget self.controller AnyUnit \k -> until EndOfTurn $ buffPower k (-1)

slavePen :: CardDef Support
slavePen = supportCard "march-of-the-damned-029" "Slave Pen" do
  race DarkElf
  cost 2
  loyalty 2
  power 1
  trait Building
  body
    "This card gains {power} for each resource token on it. Action: Sacrifice a unit to put a \
    \resource token on this card (limit once per turn)."
  zonePowerAura \_g s zone -> if s.zone == zone then s.tokens else 0
  actionWith "Enslave" 0 [SacrificeUnit] \usage -> do
    g <- getGame
    let used =
          any (\m -> m.details == ActionUsedThisTurn)
            (Map.findWithDefault [] (UnitRef usage.self.key) g.modifiers)
    unless used do
      until EndOfTurn (PendingBuff usage.self.key ActionUsedThisTurn)
      adjustSupportTokens usage.self.key 1

-- Legends (deluxe expansion) -------------------------------------------

thiefOfEssence :: CardDef Unit
thiefOfEssence = unitCard "legends-038" "Thief of Essence" do
  race DarkElf
  cost 2
  loyalty 2
  power 1
  hitPoints 1
  trait Thief
  body "Action: When one or more units leaves play, draw a card."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitLeftPlay _ -> drawCard self.controller
    _ -> pure ()

darkElfAssassin :: CardDef Unit
darkElfAssassin = unitCard "legends-039" "Dark Elf Assassin" do
  race DarkElf
  cost 3
  loyalty 2
  power 1
  hitPoints 2
  trait Warrior
  body
    "If you control a legend, this unit gains \"Action: When this unit \
    \attacks, destroy target damaged unit.\""
  onMyAttackDeclared \_owner self _zone _attackers -> do
    g <- getGame
    when (isJust (legendOf self.controller g)) $
      withTarget self.controller (unitWhere isDamaged) destroyUnit

monsterOfTheDeep :: CardDef Unit
monsterOfTheDeep = unitCard "legends-040" "Monster of the Deep" do
  race DarkElf
  cost 6
  loyalty 3
  power 3
  hitPoints 4
  trait Creature
  body
    "Action: Corrupt this unit and choose up to two target units. Those units \
    \cannot attack or defend until the end of the turn."
  action "Hypnotic Gaze" 0 \usage -> do
    corrupt usage.self.key
    withUpTo usage.user 2 (unitWhere (const True)) \chosen ->
      for_ chosen \k -> do
        until EndOfTurn $ disableAttack k
        until EndOfTurn $ disableDefend k

bladewind :: CardDef Tactic
bladewind = tacticCard "legends-042" "Bladewind" do
  race DarkElf
  cost 1
  loyalty 2
  trait Spell
  body "Action: Target opponent discards a card from his hand. Then, draw a card."
  whenResolved \self -> do
    discardRandom self.controller.next
    drawCard self.controller

-- Ambush riders (Eternal War cycle) ------------------------------------

brideOfKhaine :: CardDef Unit
brideOfKhaine = unitCard "days-of-blood-003" "Bride of Khaine" do
  race DarkElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait WitchElf
  body
    "Dark Elf only. Ambush 2. Action: When this unit ambushes, target \
    \attacking unit gets -2 hit points until the end of the turn."
  ambush 2
  onAmbush \_owner self ->
    withTarget self.controller attackingUnit \k -> until EndOfTurn $ debuffHP k 2

outlawSorcerer :: CardDef Unit
outlawSorcerer = unitCard "glory-of-days-past-076" "Outlaw Sorcerer" do
  race DarkElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Sorceror
  body
    "Dark Elf only. Ambush 1. Action: When this unit ambushes, discard a \
    \card at random from attacking opponent's hand."
  ambush 1
  onAmbush \_owner self -> discardRandom self.controller.next

testOfWill :: CardDef Tactic
testOfWill = tacticCard "the-ruinous-hordes-097" "Test of Will" do
  race DarkElf
  cost 2
  loyalty 1
  trait Spell
  body
    "Dark Elf only. Ambush 0. Play only during the Ambush step. Action: \
    \Attacking opponent must sacrifice an attacking unit or cancel the attack."
  ambush 0
  whenResolved \_self -> withCombat \cs -> do
    let attacker = cs.attackingPlayer
    if null cs.attackers
      then cancelAttack
      else do
        keep <- askYesNo attacker
          "Test of Will: sacrifice an attacking unit to continue the attack? \
          \(Declining cancels the attack.)"
        if keep
          then forcePickUnit attacker cs.attackers
            "Choose an attacking unit to sacrifice." destroyUnit
          else cancelAttack

bloodOffering :: CardDef Support
bloodOffering = supportCard "hidden-kingdoms-052" "Blood Offering" do
  race DarkElf
  cost 0
  loyalty 0
  trait Tribute
  body
    "Action: Sacrifice this card to ignore the loyalty cost of the next \
    \[Dark Elf] card you play this turn."
  actionWith "Tribute" 0 [SacrificeSelf] \usage ->
    grantLoyaltyWaiver usage.user DarkElf
