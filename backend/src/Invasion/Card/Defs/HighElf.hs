{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | High Elf core cards (core-051..055). Only 5 cards — the rest of
-- the High Elf range arrives in later sets.
module Invasion.Card.Defs.HighElf (module Invasion.Card.Defs.HighElf) where

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

silverHelmBrigade :: CardDef Unit
silverHelmBrigade = unitCard "core-051" "Silver Helm Brigade" do
  race HighElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  traits [Elite, Noble, Cavalry]
  body "Forced: After this unit takes 1 or more damage, draw a card."
  onSelfDamaged \_owner self _n -> drawCard self.controller

archmageOfSaphery :: CardDef Unit
archmageOfSaphery = unitCard "core-052" "Archmage of Saphery" do
  race HighElf
  cost 1
  loyalty 1
  power 0
  hitPoints 1
  trait Mage
  body "Quest. Action: During your quest phase, you may heal 1 damage on one target unit. (Limit once per turn.)"
  quest $ action "Mend" 0 \usage -> do
    g <- getGame
    let used =
          any (\m -> m.details == ActionUsedThisTurn)
            (Map.findWithDefault [] (UnitRef usage.self.key) g.modifiers)
    unless used do
      until EndOfTurn (PendingBuff usage.self.key ActionUsedThisTurn)
      withTarget usage.user AnyUnit \k -> healUnit k 1

blessingOfIsha :: CardDef Support
blessingOfIsha = supportCard "core-053" "Blessing of Isha" do
  race HighElf
  cost 0
  loyalty 1
  traits [Attachment, Spell]
  body "Attach to a target unit. Restore that unit, if able. Attached unit cannot be corrupted."
  -- "Restore" = remove damage + cleanse corruption when the support
  -- enters play. The "cannot be corrupted" rider lasts while the
  -- attachment is in play, modelled via a Permanent modifier on the
  -- host (the modifier is dropped if the host leaves play; we don't
  -- yet auto-clear it when only the attachment leaves — small gap
  -- to revisit when Permanent-modifier lifecycle gets tightened).
  onEnterPlay \_owner self ->
    case self.attachedTo of
      Just host -> do
        healUnit host 999
        push (CleanseUnit host)
        until Permanent $ shieldFromCorruption host
      Nothing -> pure ()

radiantGaze :: CardDef Tactic
radiantGaze = tacticCard "core-054" "Radiant Gaze" do
  race HighElf
  cost 2
  loyalty 3
  trait Spell
  body "Action: Choose an opponent's zone. All units in that zone lose {power} until the end of the turn."
  playableWhen \g _pk -> any (\u -> True) g.units
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk enemyCapital \(_owner, zk) -> do
      g <- getGame
      let targets = filter
            (\u -> u.controller /= pk && u.zone == zk) g.units
      for_ targets \u ->
        until EndOfTurn $ buffPower u.key (-1)

greaterHeal :: CardDef Tactic
greaterHeal = tacticCard "core-055" "Greater Heal" do
  race HighElf
  cost 3
  loyalty 1
  trait Spell
  body "Action: Heal all damage on your units."
  whenResolved \self -> do
    g <- getGame
    let mine = filter (\u -> u.controller == self.controller) g.units
    for_ mine \u -> healUnit u.key 999

-- The Corruption cycle ------------------------------------------------

tyriel :: CardDef Unit
tyriel = unitCard "the-skavenblight-threat-005" "Tyriel" do
  hero
  race HighElf
  cost 5
  loyalty 3
  power 2
  hitPoints 4
  body
    "Limit one Hero per zone. Forced: Whenever an opponent attacks this zone, he must \
    \return one of his attacking units to its owner's hand at the end of the turn."
  onReceive $ Receive \msg _owner self -> case msg of
    EndTurn _ -> do
      g <- getGame
      let h = Map.findWithDefault mempty ThisTurn g.history
          opp = self.controller.next
          relevant =
            [ rec
            | rec <- h.combats
            , rec.attacker == opp
            , rec.defender == self.controller
            , rec.zone == self.zone
            ]
      for_ relevant \rec -> do
        gNow <- getGame
        let alive =
              [ k
              | k <- rec.attackerKeys
              , Just u <- [findUnit k gNow]
              , u.controller == opp
              ]
        forcePickUnit opp alive
          "Tyriel: return one of your attacking units to your hand."
          returnUnitToHand
    _ -> pure ()

steelsBane :: CardDef Tactic
steelsBane = tacticCard "the-skavenblight-threat-006" "Steel's Bane" do
  race HighElf
  cost 1
  loyalty 4
  body "Action: Cancel the next 10 damage that would be dealt to one target {highelf} unit this turn."
  playableWhen $ hasTarget (unitWhere (`isRace` HighElf))
  whenResolved \self ->
    withTarget self.controller (unitWhere (`isRace` HighElf)) \k ->
      until EndOfTurn $ damageShield k 10

repairTheWaystones :: CardDef Quest
repairTheWaystones = questCard "the-skavenblight-threat-007" "Repair the Waystones" do
  race HighElf
  cost 0
  loyalty 3
  body
    "Quest. Action: Discard 3 resource tokens from this card to target a support card in your \
    \discard pile, and put it into play in your quest zone. \
    \Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  spendTokens "Restore a waystone" 3 \usage -> do
    let pk = usage.user
    me <- playerOf pk <$> getGame
    -- Attachments need a host to enter play, so only free-standing
    -- supports are recoverable through this quest.
    let candidates =
          [ c
          | c <- me.discard
          , Just cd <- [asSupport c.def]
          , Attachment `notElem` cd.traits
          ]
    chooseFromCards pk 0 1 candidates
      "Choose a support card to put into your quest zone." \chosen ->
        for_ chosen \c -> push (PlaySupportFromDiscard pk c.key QuestZone)

vaulsUnmaking :: CardDef Tactic
vaulsUnmaking = tacticCard "path-of-the-zealot-026" "Vaul's Unmaking" do
  race HighElf
  cost 0
  loyalty 1
  trait Spell
  body "Action: Destroy one target Attachment card."
  playableWhen $ hasTarget attachmentCard
  whenResolved \self ->
    withTarget self.controller attachmentCard destroySupport
  where
    attachmentCard = SupportMatching \_ _ s ->
      isJust s.attachedTo || Attachment `elem` s.cardDef.traits

repeaterBoltThrower :: CardDef Support
repeaterBoltThrower = supportCard "path-of-the-zealot-027" "Repeater Bolt Thrower" do
  race HighElf
  cost 3
  loyalty 3
  trait Siege
  body
    "Battlefield. Action: Spend X resources to deal X indirect damage to target opponent. \
    \X is the number of your developments in this zone. (Players assign their own indirect damage.)"
  battlefield $ action "Volley" 0 \usage -> do
    g <- getGame
    let me = playerOf usage.user g
        Developments x = me.capital.battlefield.developments
        Resources r = me.resources
    when (x > 0 && r >= x) do
      payResources usage.user x
      indirectDamage usage.user.next x

dragonmage :: CardDef Unit
dragonmage = unitCard "tooth-and-claw-046" "Dragonmage" do
  race HighElf
  cost 5
  loyalty 3
  power 2
  hitPoints 3
  traits [Mage, Elite]
  body "Whenever this unit is assigned damage, cancel all but 1 of that damage."
  perHitCap 1

giftsOfAenarion :: CardDef Tactic
giftsOfAenarion = tacticCard "tooth-and-claw-047" "Gifts of Aenarion" do
  race HighElf
  cost 4
  loyalty 2
  trait Spell
  body
    "Action: Cancel all damage that would be dealt to your capital until the end of the turn. \
    \For each damage thus cancelled, gain 1 resource."
  whenResolved \self ->
    push (ArmCapitalShield self.controller Nothing 1)

silverHelmDetachment :: CardDef Unit
silverHelmDetachment = unitCard "the-deathmaster-s-dance-067" "Silver Helm Detachment" do
  race HighElf
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  traits [Warrior, Elite]
  body
    "This unit enters play with 3 resource tokens on it. Action: Remove a resource token \
    \from this unit to gain {power} until the end of the turn (limit once per turn)."
  onEnterPlay \_owner self -> push (AdjustUnitTokens self.key 3)
  action "Spur the helms" 0 \usage -> do
    g <- getGame
    whenJust (findUnit usage.self.key g) \u -> do
      let used =
            any (\m -> m.details == ActionUsedThisTurn)
              (Map.findWithDefault [] (UnitRef u.key) g.modifiers)
      when (u.tokens > 0 && not used) do
        until EndOfTurn (PendingBuff u.key ActionUsedThisTurn)
        push (AdjustUnitTokens u.key (-1))
        until EndOfTurn $ buffPower u.key 1

ishasGaze :: CardDef Support
ishasGaze = supportCard "the-deathmaster-s-dance-068" "Isha's Gaze" do
  race HighElf
  cost 0
  loyalty 1
  traits [Attachment, Spell]
  body "Attach to a target unit. Whenever a unit is healed, attached unit gains {power} until the end of the turn."
  onReceive $ Receive \msg _owner self -> case msg of
    HealUnit _ n
      | n > 0 ->
          for_ self.attachedTo \host ->
            until EndOfTurn $ buffPower host 1
    _ -> pure ()

banish :: CardDef Tactic
banish = tacticCard "the-deathmaster-s-dance-069" "Banish" do
  race HighElf
  cost 3
  loyalty 2
  body "Action: Return one target unit without any Attachment cards on it to its owner's hand."
  playableWhen $ hasTarget (unitWhere (null . (.attachments)))
  whenResolved \self ->
    withTarget self.controller (unitWhere (null . (.attachments))) returnUnitToHand

finreirsGuard :: CardDef Unit
finreirsGuard = unitCard "the-warpstone-chronicles-087" "Finreir's Guard" do
  race HighElf
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Warrior
  toughness 1
  body "Toughness 1 (whenever this unit is assigned damage, cancel 1 of that damage)."

warCrownOfSaphery :: CardDef Support
warCrownOfSaphery = supportCard "the-warpstone-chronicles-088" "War Crown of Saphery" do
  unique
  race HighElf
  cost 2
  loyalty 1
  traits [Attachment, Relic]
  body
    "Attach to a target {highelf} unit. Attached unit gains {power} for each resource token \
    \on this card. Forced: At the beginning of your turn, place a resource token on this card."
  attachedTo \self unit ->
    when (self.tokens > 0) $ gainPower unit self.tokens
  onMyTurnBegin \_owner self ->
    adjustSupportTokens self.key 1

secondSight :: CardDef Tactic
secondSight = tacticCard "the-warpstone-chronicles-089" "Second Sight" do
  race HighElf
  cost 1
  loyalty 1
  trait Spell
  body "Action: Look at each opponent's hand. Then, draw a card."
  whenResolved \self -> do
    let pk = self.controller
    opp <- playerOf pk.next <$> getGame
    -- A zero-pick card prompt doubles as the reveal.
    chooseFromCards pk 0 0 opp.hand "Second Sight: the opponent's hand." \_ -> pure ()
    drawCard pk

ellyrianReavers :: CardDef Unit
ellyrianReavers = unitCard "arcane-fire-107" "Ellyrian Reavers" do
  race HighElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  traits [Warrior, Elite]
  body "Forced: When this unit enters play, put the top card of your deck into your battlefield as a development."
  onEnterPlay \_owner self ->
    addDevelopment self.controller BattlefieldZone

morvaelsLegacy :: CardDef Tactic
morvaelsLegacy = tacticCard "arcane-fire-108" "Morvael's Legacy" do
  race HighElf
  cost 10
  loyalty 3
  traits [Epic, Spell]
  body
    "Play only during your turn. Action: Put into play all units in your discard pile. \
    \(You choose which zone each unit enters.)"
  playableWhen \g pk ->
    g.currentPlayer == pk
      && any (isJust . asUnit . (.def)) (playerOf pk g).discard
  whenResolved \self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    let units = [c | c <- me.discard, isJust (asUnit c.def)]
    for_ units \c ->
      withTarget pk MyAnyZone \zk ->
        putUnitIntoPlay pk FromDiscard c.key zk

chargeOfTheSilverHelms :: CardDef Tactic
chargeOfTheSilverHelms = tacticCard "arcane-fire-109" "Charge of the Silver Helms" do
  race HighElf
  cost 1
  loyalty 2
  body "Action: One of your target units gets -1 hit points and gains {power}{power}{power} until the end of the turn."
  playableWhen $ hasTarget ownUnit
  whenResolved \self ->
    withTarget self.controller ownUnit \k -> do
      until EndOfTurn $ debuffHP k 1
      until EndOfTurn $ buffPower k 3

-- Days of Blood --------------------------------------------------------

greatFireDragon :: CardDef Unit
greatFireDragon = unitCard "days-of-blood-010" "Great Fire Dragon" do
  race HighElf
  cost 5
  loyalty 2
  power 3
  hitPoints 4
  trait Creature
  battlefieldOnly
  body
    "Battlefield only. Action: When this unit attacks, put 1 resource token on it. \
    \Then, you may remove X resource tokens from this unit to deal X damage to target \
    \unit in the attacked zone."
  onMyAttackDeclared \_owner self zone _attackers -> do
    push (AdjustUnitTokens self.key 1)
    g <- getGame
    let avail = maybe 0 (.tokens) (findUnit self.key g) + 1
        inZone u = u.zone == zone && u.controller /= self.controller
        targets = [u | u <- g.units, inZone u]
    when (avail > 0 && not (null targets)) $
      may self.controller "Great Fire Dragon: remove resource tokens to deal damage?" do
        x <- chooseAmount self.controller 1 avail "Remove how many resource tokens?"
        withTarget self.controller (UnitMatching \_ _ u -> inZone u) \k -> do
          push (AdjustUnitTokens self.key (negate x))
          dealDamage k x

-- Oaths of Vengeance ---------------------------------------------------

outlyingTower :: CardDef Support
outlyingTower = supportCard "oaths-of-vengeance-023" "Outlying Tower" do
  race HighElf
  cost 1
  loyalty 1
  power 1
  trait Building
  body "If you control a non-[High Elf] card, sacrifice this card."
  sacrificeIfControlsOffFaction HighElf

-- Battle for the Old World ---------------------------------------------

lilea :: CardDef Unit
lilea = unitCard "battle-for-the-old-world-050" "Lilea" do
  unique
  race HighElf
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  traits [Elite, Ranger]
  body
    "Action: When this unit attacks, put 1 resource token on it. Then, deal X indirect \
    \damage to target opponent. X is the number of resource tokens on this unit."
  onMyAttackDeclared \_owner self _zone _attackers -> do
    push (AdjustUnitTokens self.key 1)
    g <- getGame
    let x = maybe 0 (.tokens) (findUnit self.key g) + 1
    when (x > 0) $ indirectDamage self.controller.next x

-- Glory of Days Past ---------------------------------------------------

masterOfQhaysh :: CardDef Unit
masterOfQhaysh = unitCard "glory-of-days-past-067" "Master of Qhaysh" do
  race HighElf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Mage
  body
    "Action: When this unit survives an attack on an opponent's zone, put 1 resource \
    \token on a card with at least 1 resource token on it."
  onCombatResolveAsAttacker \_owner self _cs -> do
    g <- getGame
    when (isJust (findUnit self.key g)) $
      withTarget self.controller (UnitMatching \_pk _g u -> u.tokens >= 1) \k ->
        push (AdjustUnitTokens k 1)

-- The Ruinous Hordes ---------------------------------------------------

avelornSojourner :: CardDef Unit
avelornSojourner = unitCard "the-ruinous-hordes-091" "Avelorn Sojourner" do
  race HighElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Mage
  body "While this unit is questing, raise the cost of each tactic played by an opponent by 1."
  unitCostAdjust \_g self pk filt ->
    if self.zone == QuestZone && pk /= self.controller && filt.cfKind == Tactic
      then 1
      else 0

-- Bloodquest: Rising Dawn -----------------------------------------------

lothernSeaMaster :: CardDef Unit
lothernSeaMaster = unitCard "rising-dawn-009" "Lothern Sea Master" do
  race HighElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  traits [Warrior, Elite]
  body
    "Battlefield. This unit enters play with 3 resource tokens on it. Action: Deal X indirect \
    \damage to target opponent. X is the number of resource tokens on this unit. Then, remove \
    \1 resource token from this card. (Limit once per turn)."
  onEnterPlay \_owner self -> push (AdjustUnitTokens self.key 3)
  battlefield $ action "Bombard" 0 \usage -> do
    g <- getGame
    whenJust (findUnit usage.self.key g) \u -> do
      let used =
            any (\m -> m.details == ActionUsedThisTurn)
              (Map.findWithDefault [] (UnitRef u.key) g.modifiers)
      when (u.tokens > 0 && not used) do
        until EndOfTurn (PendingBuff u.key ActionUsedThisTurn)
        indirectDamage usage.user.next u.tokens
        push (AdjustUnitTokens u.key (-1))

-- Bloodquest: The Accursed Dead -----------------------------------------

lionStandard :: CardDef Unit
lionStandard = unitCard "the-accursed-dead-045" "Lion Standard" do
  race HighElf
  cost 0
  loyalty 2
  power 0
  hitPoints 2
  body "Action: Spend 1 resource to have target unit get +1 hit point until the end of the turn."
  action "Bolster" 1 \usage ->
    withTarget usage.user AnyUnit \k -> until EndOfTurn $ buffHP k 1

-- Bloodquest: Portent of Doom -------------------------------------------

princeAlthran :: CardDef Unit
princeAlthran = unitCard "portent-of-doom-089" "Prince Althran" do
  hero
  trait Noble
  race HighElf
  cost 3
  loyalty 3
  power 2
  hitPoints 3
  body
    "Limit one Hero per zone. This unit enters play with 1 resource token on it. Action: \
    \Remove 1 resource token from a unit you control to have this unit gain {power} until the \
    \end of the turn."
  onEnterPlay \_owner self -> push (AdjustUnitTokens self.key 1)
  action "Rally" 0 \usage ->
    withTarget usage.user (UnitMatching \pk _g u -> u.controller == pk && u.tokens > 0) \k -> do
      push (AdjustUnitTokens k (-1))
      until EndOfTurn $ buffPower usage.self.key 1

-- Bloodquest: Shield of the Gods ----------------------------------------

ellyrianElite :: CardDef Unit
ellyrianElite = unitCard "shield-of-the-gods-109" "Ellyrian Elite" do
  race HighElf
  cost 4
  loyalty 3
  power 2
  hitPoints 4
  traits [Cavalry, Elite]
  scout
  body "Scout."

-- The Capital Cycle ----------------------------------------------------

princeOfCaledor :: CardDef Unit
princeOfCaledor = unitCard "the-inevitable-city-004" "Prince of Caledor" do
  race HighElf
  cost 8
  loyalty 3
  power 4
  hitPoints 4
  trait Noble
  body "Lower the cost to play this unit by 1 for each damaged unit you control."
  selfCostAdjust \g pk ->
    negate (length [u | u <- g.units, u.controller == pk, isDamaged u])

heraldOfMoraiHeg :: CardDef Unit
heraldOfMoraiHeg = unitCard "realm-of-the-phoenix-king-027" "Herald of Morai-Heg" do
  race HighElf
  cost 2
  loyalty 1
  power 0
  hitPoints 3
  trait Warrior
  body "Counterstrike X. X is the highest loyalty on a [High Elf] card you control."
  counterstrikeX \g u -> highestLoyaltyControlled HighElf g u.controller

straitsOfLothern :: CardDef Support
straitsOfLothern = supportCard "realm-of-the-phoenix-king-030" "Straits of Lothern" do
  race HighElf
  cost 3
  loyalty 3
  power 1
  trait Location
  body "Kingdom. This card gains {power} equal to the number of units in this zone."
  zonePowerAura \g s zone ->
    if s.zone == zone
      then length [u | u <- g.units, u.controller == s.controller, u.zone == zone]
      else 0

inflame :: CardDef Tactic
inflame = tacticCard "realm-of-the-phoenix-king-033" "Inflame" do
  race HighElf
  cost 1
  loyalty 3
  trait Spell
  body
    "Action: Discard a card from your hand with X loyalty to have target unit gain X \
    \power until the end of the turn."
  playableWhen \g pk -> not (null (playerOf pk g).hand) && hasTarget AnyUnit g pk
  whenResolved \self ->
    withTarget self.controller AnyUnit \k ->
      discardForLoyalty self.controller \x ->
        when (x > 0) $ until EndOfTurn $ buffPower k x

-- Cataclysm cycle ------------------------------------------------------

lirdir :: CardDef Unit
lirdir = unitCard "cataclysm-025" "Lirdir" do
  race HighElf
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  traits [Hero, Mage]
  limitOneHeroPerZone
  body
    "Limit 1 Hero per zone. Action: When you heal a unit, that unit gains \
    \{power} until the end of the turn."
  onReceive $ Receive \msg _owner _self -> case msg of
    HealUnit uk n | n > 0 -> until EndOfTurn $ buffPower uk 1
    _ -> pure ()

arcanePurifier :: CardDef Unit
arcanePurifier = unitCard "cataclysm-028" "Arcane Purifier" do
  race HighElf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Mage
  body
    "Action: When this unit enters or leaves play, heal all damage on \
    \target unit."
  onEnterPlay \_owner self ->
    withTarget self.controller AnyUnit \k -> healUnit k 999
  onSelfLeavesPlay \_owner self ->
    withTarget self.controller AnyUnit \k -> healUnit k 999

-- The Morrslieb cycle ---------------------------------------------------

valorousMage :: CardDef Unit
valorousMage = unitCard "the-eclipse-of-hope-089" "Valorous Mage" do
  race HighElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Mage
  body
    "Action: When this unit enters play, search the top five cards of your deck \
    \for a Spell card, reveal it, and put it into your hand. Then, shuffle your deck."
  onEnterPlay \_owner self -> do
    let pk = self.controller
    searchTopOfDeck pk 5 \result -> do
      let spells = [c | c <- result.cards, Just cd <- [asTactic c.def], Spell `elem` cd.traits]
      chooseFromCards pk 0 1 spells "Choose a Spell to add to your hand." \chosen ->
        for_ chosen \c -> push (TakeCardsFromDeckToHand pk [c.key])
      shuffleDeck pk

trueMage :: CardDef Unit
trueMage = unitCard "the-twin-tailed-comet-051" "True Mage" do
  race HighElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Mage
  body
    "Action: When you play a development from your hand, put a resource token on \
    \this unit. Then, deal X indirect damage to each player. X is the number of \
    \resource tokens on this card."
  onYouPlayDevelopment \_owner self -> do
    push (AdjustUnitTokens self.key 1)
    let n = self.tokens + 1
    eachPlayer \pk -> indirectDamage pk n

whiteLionVanguard :: CardDef Unit
whiteLionVanguard = unitCard "the-twin-tailed-comet-050" "White Lion Vanguard" do
  race HighElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  traits [Warrior, Elite]
  body
    "Redirect the first point of damage dealt to this unit each turn to target \
    \unit in any corresponding zone."
  preDamageRedirectHook (redirectFirstDamageEachTurn 1)

bladelord :: CardDef Unit
bladelord = unitCard "signs-in-the-stars-070" "Bladelord" do
  race HighElf
  cost 5
  loyalty 2
  power 3
  hitPoints 4
  traits [Warrior, Elite]
  body
    "Redirect the first 2 damage dealt to this unit each turn to another target \
    \unit in any corresponding zone."
  preDamageRedirectHook (redirectFirstDamageEachTurn 2)

-- | Shared "redirect the first N damage dealt to this unit each turn to
-- another target unit" hook (White Lion Vanguard, Bladelord). Mirrors
-- the Warrior Priests pattern: claim up to N of the first hit each turn
-- provided some other unit exists to receive it.
--
-- TODO: approximation for N > 1 (Bladelord) — claims up to N from the
-- *first* damage event of the turn only. The printed "first 2 damage
-- dealt each turn" should redirect across multiple smaller hits until N
-- total has been redirected; model with a per-turn redirected-damage
-- counter instead of the single RedirectedThisTurn flag. Also redirects
-- to "another unit" generally rather than restricting to a
-- corresponding zone.
redirectFirstDamageEachTurn
  :: Int -> Game -> UnitDetails -> Int -> Maybe PreDamageRedirect
redirectFirstDamageEachTurn n g self inbound =
  let used =
        any (\m -> m.details == RedirectedThisTurn)
          (Map.findWithDefault [] (UnitRef self.key) g.modifiers)
      anyTarget = any (\u -> u.key /= self.key) g.units
   in if not used && anyTarget && inbound > 0
        then Just PreDamageRedirect
          { amount = min n inbound
          , run = ActionEffect \usage -> do
              until EndOfTurn (PendingBuff usage.self.key RedirectedThisTurn)
              withTarget usage.user
                (UnitMatching \_ _ u -> u.key /= usage.self.key)
                \k -> dealDamage k (min n inbound)
          }
        else Nothing

-- The Enemy cycle -------------------------------------------------------

spearhostOfAsuryan :: CardDef Unit
spearhostOfAsuryan = unitCard "the-fourth-waystone-086" "Spearhost of Asuryan" do
  race HighElf
  cost 4
  loyalty 1
  power 2
  hitPoints 3
  traits [Warrior, Elite]
  body "Action: When this unit attacks, deal 2 indirect damage to target opponent."
  onMyAttackDeclared \_owner self _zone _attackers ->
    indirectDamage self.controller.next 2

descendantOfIndraugnir :: CardDef Unit
descendantOfIndraugnir = unitCard "the-silent-forge-046" "Descendant of Indraugnir" do
  race HighElf
  cost 6
  loyalty 3
  power 4
  hitPoints 4
  trait Dragon
  body "Action: When this unit attacks, deal 4 indirect damage to each opponent."
  onMyAttackDeclared \_owner self _zone _attackers ->
    indirectDamage self.controller.next 4

dreamerOfDragons :: CardDef Unit
dreamerOfDragons = unitCard "redemption-of-a-mage-068" "Dreamer of Dragons" do
  race HighElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  traits [Mage, Elite]
  body "Action: This unit takes 1 uncancellable damage. If it does, it gains {power} until the end of the turn."
  action "Wound for power" 0 \u -> do
    dealUncancellableDamage u.self.key 1
    until EndOfTurn $ buffPower u.self.key 1

courageOfAenarion :: CardDef Tactic
courageOfAenarion = tacticCard "redemption-of-a-mage-069" "Courage of Aenarion" do
  race HighElf
  cost 1
  loyalty 2
  body "Action: Restore all corrupt units that you control."
  playableWhen \g pk -> any (\u -> u.controller == pk && u.corrupted) g.units
  whenResolved \self -> do
    g <- getGame
    let n = length [u | u <- g.units, u.controller == self.controller, u.corrupted]
    replicateM_ n $ push (RestoreOneCorruptCard self.controller)

-- The Enemy cycle (batch 2) ---------------------------------------------

moonStaffOfLileath :: CardDef Support
moonStaffOfLileath = supportCard "bleeding-sun-108" "Moon Staff of Lileath" do
  race HighElf
  cost 2
  loyalty 3
  traits [Attachment, Weapon]
  body "Attach to target [High Elf] unit. Action: When attached unit attacks or defends, deal 2 indirect damage to each opponent."
  onAttachedHostAttackOrDefend \_owner self _host ->
    indirectDamage self.controller.next 2

dragonArmour :: CardDef Support
dragonArmour = supportCard "bleeding-sun-109" "Dragon Armour" do
  race HighElf
  cost 2
  loyalty 2
  trait Attachment
  body "Attach to a target [High Elf] unit. Action: When attached unit defends, draw a card."
  onAttachedHostDefend \_owner self _host -> drawCard self.controller

doYouKnowWhoIAm :: CardDef Tactic
doYouKnowWhoIAm = tacticCard "the-burning-of-derricksburg-008" "'Do You Know Who I Am?'" do
  race HighElf
  cost 3
  loyalty 2
  body "Action: Each non-[High Elf] unit in play loses {power} until the end of the turn."
  whenResolved \_self -> do
    g <- getGame
    for_ [u | u <- g.units, HighElf `notElem` u.cardDef.races] \u ->
      until EndOfTurn $ buffPower u.key (-1)

-- Assault on Ulthuan ---------------------------------------------------

envoyFromAverlorn :: CardDef Unit
envoyFromAverlorn = unitCard "assault-on-ulthuan-001" "Envoy from Averlorn" do
  race HighElf
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  traits [Messenger]
  questOnly
  body "Quest zone only."

highElfSpearmen :: CardDef Unit
highElfSpearmen = unitCard "assault-on-ulthuan-004" "High Elf Spearmen" do
  race HighElf
  cost 3
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior

initiateOfSaphery :: CardDef Unit
initiateOfSaphery = unitCard "assault-on-ulthuan-006" "Initiate of Saphery" do
  race HighElf
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  traits [Initiate, Mage]
  body "Kingdom. Action: At the beginning of your turn, heal 1 damage from each unit you control."
  kingdom $ onMyTurnBegin \_owner self -> do
    g <- getGame
    for_ [u | u <- g.units, u.controller == self.controller] \u ->
      healUnit u.key 1

loremasterOfHoeth :: CardDef Unit
loremasterOfHoeth = unitCard "assault-on-ulthuan-007" "Loremaster of Hoeth" do
  race HighElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Mage
  body "Forced: After this unit enters play, each player takes 2 indirect damage. (Players assign their own indirect damage.)"
  onEnterPlay \_owner _self -> eachPlayer \pk -> indirectDamage pk 2

dragonMageWakening :: CardDef Support
dragonMageWakening = supportCard "assault-on-ulthuan-012" "Dragon Mage Wakening" do
  race HighElf
  cost 0
  loyalty 1
  trait Attachment
  body "Attach to a target unit. Attached unit gets +3 hit points."
  attachmentHp 3

tearOfIsha :: CardDef Tactic
tearOfIsha = tacticCard "assault-on-ulthuan-016" "Tear of Isha" do
  race HighElf
  cost 1
  loyalty 1
  trait Spell
  body "Action: Heal all damage on one target unit."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> healUnit k 999

flamesOfThePhoenix :: CardDef Tactic
flamesOfThePhoenix = tacticCard "assault-on-ulthuan-017" "Flames of the Phoenix" do
  race HighElf
  cost 4
  loyalty 3
  trait Spell
  body "Play during your turn. Action: Return all units in play to their owner's hands."
  playableWhen \g pk -> g.currentPlayer == pk
  whenResolved \_self -> do
    g <- getGame
    for_ g.units \u -> returnUnitToHand u.key

swordMastersOfHoeth :: CardDef Unit
swordMastersOfHoeth = unitCard "assault-on-ulthuan-003" "Sword Masters of Hoeth" do
  race HighElf
  cost 4
  loyalty 2
  power 2
  hitPoints 3
  traits [Warrior, Elite]
  body "Battlefield. Cancel all combat damage assigned to this unit."
  -- Approximation: the engine's 'cancelAllDamageWhen' slot cancels ALL
  -- cancellable damage, not strictly combat damage; gated to the
  -- battlefield zone to match the printed "Battlefield." line.
  damageImmuneWhen \_g self -> self.zone == BattlefieldZone

templeOfVaul :: CardDef Support
templeOfVaul = supportCard "assault-on-ulthuan-015" "Temple of Vaul" do
  race HighElf
  cost 3
  loyalty 1
  power 3
  trait Building
  body "Forced: At the beginning of your turn, deal 2 damage to your capital (you choose which section(s))."
  onMyTurnBegin \_owner self ->
    withTarget self.controller (CapitalMatching \pk (owner, _) -> owner == pk) \(owner, zk) ->
      dealZoneDamage owner zk 2

giftOfLife :: CardDef Tactic
giftOfLife = tacticCard "assault-on-ulthuan-020" "Gift of Life" do
  race HighElf
  cost 1
  loyalty 2
  trait Spell
  body "Action: Return target [High Elf] unit from your discard pile to your hand."
  playableWhen \g pk -> not (null (highElfUnitsIn (playerOf pk g).discard))
  whenResolved \self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    chooseFromCards pk 0 1 (highElfUnitsIn me.discard)
      "Choose a High Elf unit to return to your hand." \chosen ->
        for_ chosen \c -> returnFromDiscardToHand pk [c.key]
  where
    highElfUnitsIn cards =
      [c | c <- cards, Just cd <- [asUnit c.def], HighElf `elem` cd.races]

-- March of the Damned --------------------------------------------------

seaGuardCaptain :: CardDef Unit
seaGuardCaptain = unitCard "march-of-the-damned-011" "Sea Guard Captain" do
  race HighElf
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Warrior
  body
    "Action: When this unit attacks, deal 1 indirect damage to target opponent (players \
    \allocate their own indirect damage)."
  onMyAttackDeclared \_owner self _zone _attackers ->
    indirectDamage self.controller.next 1

keeperOfTheFlame :: CardDef Unit
keeperOfTheFlame = unitCard "march-of-the-damned-012" "Keeper of the Flame" do
  race HighElf
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body
    "Action: This unit takes 1 uncancellable damage. If it does, destroy target Attachment \
    \support card."
  action "Quench" 0 \usage -> do
    dealUncancellableDamage usage.self.key 1
    withTarget usage.user (SupportMatching \_pk _g s -> Attachment `elem` s.cardDef.traits)
      destroySupport

elvenWarship :: CardDef Support
elvenWarship = supportCard "march-of-the-damned-014" "Elven Warship" do
  race HighElf
  cost 3
  loyalty 1
  power 1
  trait Ship
  body
    "Action: At the beginning of your turn, deal 2 indirect damage to target opponent \
    \(players allocate their own indirect damage)."
  onMyTurnBegin \_owner self ->
    indirectDamage self.controller.next 2

fromBeneathTheWaves :: CardDef Tactic
fromBeneathTheWaves = tacticCard "march-of-the-damned-015" "From Beneath the Waves" do
  race HighElf
  cost 2
  loyalty 2
  body
    "Action: Deal 3 indirect damage to target opponent (players allocate their own indirect \
    \damage)."
  whenResolved \self -> indirectDamage self.controller.next 3

-- Legends (deluxe expansion) -------------------------------------------

elvenSteed :: CardDef Support
elvenSteed = supportCard "legends-027" "Elven Steed" do
  race HighElf
  cost 1
  loyalty 2
  trait Attachment
  body
    "Attach to a target unit you control. Attached unit gains +3 hit points. \
    \If attached unit leaves play, you may spend 2 resources to return Elven \
    \Steed to its owner's hand."
  attachmentHp 3

masterOfTheEarth :: CardDef Tactic
masterOfTheEarth = tacticCard "legends-028" "Master of the Earth" do
  race HighElf
  cost 3
  loyalty 3
  trait Spell
  body
    "Action: Deal X indirect damage to target player. X is the number of \
    \developments you control."
  whenResolved \self -> do
    g <- getGame
    let me = playerOf self.controller g
        devCount = sum [d | z <- me.capital.zones, let Developments d = z.developments]
    indirectDamage self.controller.next devCount
