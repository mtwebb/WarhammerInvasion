{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Orc core cards (core-056..080). Every printed ability is
-- implemented; one card uses an engine-driven simplification
-- (Rip Dere 'eads Off! always destroys rather than flipping
-- then conditionally summoning) — the compromise is called out
-- in the card's local comment.
module Invasion.Card.Defs.Orc (module Invasion.Card.Defs.Orc) where

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

crookedTeefGoblins :: CardDef Unit
crookedTeefGoblins = unitCard "core-056" "Crooked Teef Goblins" do
  race Orc
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  traits [Goblin, Warrior]
  body "Battlefield only."
  battlefieldOnly

squigHerders :: CardDef Unit
squigHerders = unitCard "core-057" "Squig Herders" do
  race Orc
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  traits [Goblin, Warrior]
  body "Squig Herders gain {power} while you control at least 1 damaged unit."
  effects \self owner ->
    let mine = filter (\u -> u.controller == owner.key && isDamaged u) <$> getGameUnits
     in mine >>= \xs -> when (not (null xs)) (gainPower self 1)

ironclawsHorde :: CardDef Unit
ironclawsHorde = unitCard "core-058" "Ironclaw's Horde" do
  race Orc
  cost 5
  loyalty 2
  power 4
  hitPoints 2
  trait Warrior
  body "Battlefield only."
  battlefieldOnly

followersOfMork :: CardDef Unit
followersOfMork = unitCard "core-059" "Followers of Mork" do
  race Orc
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Shaman
  body "Forced: After this unit enters play, each player takes 2 indirect damage. (Players allocate their own indirect damage.)"
  onEnterPlay \_owner self -> do
    indirectDamage self.controller 2
    indirectDamage self.controller.next 2

blackOrcSquad :: CardDef Unit
blackOrcSquad = unitCard "core-060" "Black Orc Squad" do
  race Orc
  cost 3
  loyalty 1
  power 1
  hitPoints 4
  traits [Warrior, Elite]

boarBoyz :: CardDef Unit
boarBoyz = unitCard "core-061" "Boar Boyz" do
  race Orc
  cost 4
  loyalty 2
  power 1
  hitPoints 4
  trait Cavalry
  body "This unit gains {power}{power} while you control at least one damaged unit."
  effects \self owner ->
    let mine = filter (\u -> u.controller == owner.key && isDamaged u) <$> getGameUnits
     in mine >>= \xs -> when (not (null xs)) (gainPower self 2)

urguck :: CardDef Unit
urguck = unitCard "core-062" "Urguck" do
  hero
  trait Warrior
  race Orc
  cost 3
  loyalty 3
  power 1
  hitPoints 3
  body "Limit one Hero per zone. During your capital phase, you may spend damage on this unit as though it were resources."
  -- A free repeatable action: trade 1 damage on Urguck for 1
  -- resource for the controller. Gated to the capital phase by
  -- checking 'g.phase' at fire time. Repeat to spend more damage.
  action "Spend a wound" 0 \usage -> do
    g <- getGame
    case findUnit usage.self.key g of
      Just u
        | isDamaged u
        , g.phase == Just CapitalPhase
        , g.currentPlayer == usage.user -> do
            push (HealUnit usage.self.key 1)
            push (GainResources usage.user 1)
      _ -> pure ()

grimgorIronhide :: CardDef Unit
grimgorIronhide = unitCard "core-063" "Grimgor Ironhide" do
  hero
  trait Warrior
  race Orc
  cost 6
  loyalty 5
  power 3
  hitPoints 6
  body "Limit one Hero per zone. Forced: After this unit enters play, destroy all support cards and developments in each player's corresponding zone."
  onEnterPlay \_owner self -> do
    g <- getGame
    -- Destroy every free-standing support in the same zone, both
    -- sides. Attached supports are left alone (their host unit is
    -- the targetable thing).
    let supps =
          [ s.key
          | s <- g.supports
          , s.attachedTo == Nothing
          , s.zone == self.zone
          ]
    for_ supps destroySupport
    -- Then pop every development from the matching zone on each
    -- side. We just push one DestroyDevelopment per dev.
    for_ [Player1, Player2] \pk -> do
      let p = playerOf pk g
          Developments n = case self.zone of
            KingdomZone -> p.capital.kingdom.developments
            QuestZone -> p.capital.quest.developments
            BattlefieldZone -> p.capital.battlefield.developments
      replicateM_ n (destroyDevelopment pk self.zone)

nightGoblins :: CardDef Unit
nightGoblins = unitCard "core-064" "Night Goblins" do
  race Orc
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  traits [Goblin, Shaman]
  body "Forced: After this unit enters play, destroy one target Attachment card in any player's corresponding zone, if able."
  onEnterPlay \_owner self -> do
    g <- getGame
    let attachments =
          [ s
          | u <- g.units
          , u.zone == self.zone
          , s <- u.attachments
          ]
    case attachments of
      [] -> pure ()
      _ -> do
        let cards =
              [ mkCard s.key (SupportCardDef s.cardDef)
              | s <- attachments
              ]
        chooseFromCards self.controller 1 1 cards
          "Choose an attachment to destroy." \chosen ->
            for_ chosen \c -> destroySupport c.key

doomDivers :: CardDef Unit
doomDivers = unitCard "core-065" "Doom Divers" do
  race Orc
  cost 4
  loyalty 1
  power 2
  hitPoints 2
  trait Goblin
  body "Battlefield. Forced: After your turn begins, each player must either sacrifice a development or deal 1 damage to each section of his capital."
  -- Each player picks: sacrifice a development (if any) or take the
  -- per-section capital damage. We ask only if there's an actual
  -- development to choose; with none, the damage is mandatory.
  onMyTurnBegin \_owner self ->
    when (self.zone == BattlefieldZone) $ do
      g <- getGame
      for_ [self.controller, self.controller.next] \pk -> do
        let p = playerOf pk g
            devZones =
              [ zk
              | (zk, Developments n) <-
                  [ (KingdomZone, p.capital.kingdom.developments)
                  , (QuestZone, p.capital.quest.developments)
                  , (BattlefieldZone, p.capital.battlefield.developments)
                  ]
              , n > 0
              ]
            dealAll =
              for_ [KingdomZone, QuestZone, BattlefieldZone] \zk ->
                dealZoneDamage pk zk 1
        case devZones of
          [] -> dealAll
          _ -> do
            sacrifice <- askYesNo pk "Sacrifice a development instead of taking 1 damage to each capital section?"
            if sacrifice
              then withTarget pk
                (CapitalMatching \_ (owner, zk) ->
                  owner == pk && zk `elem` devZones)
                \(owner, zk) -> destroyDevelopment owner zk
              else dealAll

lobberCrew :: CardDef Unit
lobberCrew = unitCard "core-066" "Lobber Crew" do
  race Orc
  cost 2
  loyalty 1
  power 1
  hitPoints 1
  trait Goblin
  body "Kingdom. Action: Sacrifice this unit to force an opponent to sacrifice a unit he controls, if able."
  kingdom $ actionWith "Force a sacrifice" 0 [SacrificeUnit] \usage -> do
    let opp = usage.user.next
    withTarget opp (UnitMatching \_ _ u -> u.controller == opp) \k ->
      destroyUnit k

bigUns :: CardDef Unit
bigUns = unitCard "core-067" "Big 'Uns" do
  race Orc
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body "Battlefield. Your damaged units gain Toughness 1."
  toughnessAura \_g self target ->
    if self.zone == BattlefieldZone
        && target.controller == self.controller
        && isDamaged target
      then 1
      else 0

rockLobber :: CardDef Support
rockLobber = supportCard "core-068" "Rock Lobber" do
  race Orc
  cost 2
  loyalty 2
  trait Siege
  body "Battlefield. Action: Pay 2 resources and sacrifice one of your units in this zone to deal 2 damage to one section of any capital (limit once per turn)."
  battlefield $ actionWith "Lob a rock" 2 [SacrificeUnit] \usage -> do
    g <- getGame
    let used =
          any (\m -> m.details == ActionUsedThisTurn)
            (Map.findWithDefault [] (UnitRef usage.self.key) g.modifiers)
    unless used do
      until EndOfTurn (PendingBuff usage.self.key ActionUsedThisTurn)
      withTarget usage.user AnyCapital \(owner, zk) ->
        dealZoneDamage owner zk 2

choppa :: CardDef Support
choppa = supportCard "core-069" "Choppa" do
  race Orc
  cost 1
  loyalty 2
  traits [Attachment, Weapon]
  body "Attach to a target in your battlefield. Attached unit gains {power}{power}."
  attachedTo \_self unit -> gainPower unit 2

totemOfGork :: CardDef Support
totemOfGork = supportCard "core-070" "Totem of Gork" do
  race Orc
  cost 3
  loyalty 3
  power 1
  trait Siege
  body "Units in this zone gain {power} while attacking or defending."
  -- Combat-only bonus for units sharing the Totem's zone — NOT a
  -- zone-income aura. 'supportCombat' contributions flow into
  -- 'combatDamageOf', which only runs for declared attackers and
  -- defenders, and the attacking/defending flags pin it further.
  supportCombat \_g self u ->
    if u.controller == self.controller
      && u.zone == self.zone
      && (u.attacking || u.defending)
      then 1
      else 0

bannaOfDaRedSunz :: CardDef Support
bannaOfDaRedSunz = supportCard "core-071" "Banna of Da Red Sunz" do
  race Orc
  cost 4
  loyalty 1
  power 2
  trait Banner
  body "Kingdom. Each opponent that collects 7 or more resources for his kingdom phase must assign one of those resources as a damage token to a target unit of your choice."
  onReceive $ Receive \msg _owner self -> case msg of
    CollectResources opp
      | opp /= self.controller, self.zone == KingdomZone -> do
          g <- getGame
          let p = playerOf opp g
              Resources r = p.resources
          when (r >= 7) do
            -- Take the resource as damage to a unit of our choice.
            push (SpendResources opp 1)
            withTarget self.controller AnyUnit \k -> dealDamage k 1
    _ -> pure ()

smashEmAll :: CardDef Quest
smashEmAll = questCard "core-072" "Smash 'Em All!" do
  race Orc
  cost 1
  loyalty 2
  body "Quest. Action: Sacrifice the unit on this quest to destroy all enemy support cards. Use this ability only if Smash 'Em All! has 3 or more resource tokens on it. Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  action "Smash them all" 0 \usage ->
    withQuest usage.self.key \q ->
      when (q.tokens >= 3) $
        for_ q.questingUnit \quester -> do
          destroyUnit quester
          g <- getGame
          let enemySupports =
                [ s.key
                | s <- g.supports
                , s.controller /= usage.user
                , s.attachedTo == Nothing
                ]
          for_ enemySupports destroySupport

grimgorsCamp :: CardDef Support
grimgorsCamp = supportCard "core-073" "Grimgor's Camp" do
  race Orc
  cost 3
  loyalty 1
  power 1
  trait Building
  body "Kingdom. Lower the cost of the first {orc} unit you play each turn by 1."
  globalCostAdjust \g self pk filt ->
    let h = case Map.lookup ThisTurn g.history of
              Just hx -> hx
              Nothing -> mempty
        played = Map.findWithDefault 0 pk h.unitsPlayedBy
        zoneGate = self.zone == KingdomZone
     in if pk == self.controller
           && zoneGate
           && filt.cfKind == Unit
           && Orc `elem` filt.cfRaces
           && played == 0
          then -1
          else 0

smashGoBoom :: CardDef Tactic
smashGoBoom = tacticCard "core-074" "Smash-Go-Boom!" do
  race Orc
  costVariable
  loyalty 2
  body "Play during your turn. Action: Destroy X target developments in one zone."
  whenResolved \self ->
    when (self.xValue > 0) $
      withTarget self.controller AnyDevelopmentZone \(owner, zk) ->
        replicateM_ self.xValue (destroyDevelopment owner zk)

ripDereEadsOff :: CardDef Tactic
ripDereEadsOff = tacticCard "core-075" "Rip Dere 'eads Off!" do
  race Orc
  cost 1
  loyalty 1
  body "Action: Turn one target development faceup. If it is a unit, leave it in play and sacrifice it at the end of the turn. Otherwise, sacrifice it immediately."
  playableWhen \g _ -> hasAnyDevelopment g
  whenResolved \self ->
    withTarget self.controller AnyDevelopmentZone \(owner, zk) ->
      flipDevelopment owner zk

wezBigga :: CardDef Tactic
wezBigga = tacticCard "core-076" "We'z Bigga!" do
  race Orc
  cost 0
  loyalty 1
  body "Action: Lower the cost of the next unit you play this turn by 1. That unit comes into play with 1 damage on it."
  whenResolved \self -> do
    push (ScheduleNextUnitDiscount self.controller 1)
    push (ScheduleNextUnitDamage self.controller 1)

favourOfMork :: CardDef Tactic
favourOfMork = tacticCard "core-077" "Favour of Mork" do
  race Orc
  cost 1
  loyalty 2
  trait Spell
  body "Action: One target unit loses {power} and another target unit gains {power} until the end of the turn."
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \loser ->
      withTarget pk AnyUnit \winner -> do
        until EndOfTurn $ buffPower loser (-1)
        until EndOfTurn $ buffPower winner 1

pillage :: CardDef Tactic
pillage = tacticCard "core-078" "Pillage" do
  race Orc
  cost 2
  loyalty 2
  body "Action: Destroy one target support card."
  playableWhen hasEnemySupport
  tacticTargets SupportTargetSchema
  onResolveEnemySupport \_pk s -> destroySupport s.key

waaagh :: CardDef Tactic
waaagh = tacticCard "core-079" "Waaagh!" do
  race Orc
  cost 3
  loyalty 2
  body "Action: Each attacking unit gains {power}{power} until the end of the turn."
  whenResolved \_ ->
    withCombat \cs ->
      for_ cs.attackers \k ->
        until EndOfTurn $ buffPower k 2

trollVomit :: CardDef Tactic
trollVomit = tacticCard "core-080" "Troll Vomit" do
  race Orc
  cost 4
  loyalty 2
  body "Play during your turn. Action: Destroy all units in play."
  whenResolved \_ -> do
    g <- getGame
    for_ g.units \u -> destroyUnit u.key

-- The Corruption cycle ------------------------------------------------

spiderRiders :: CardDef Unit
spiderRiders = unitCard "the-skavenblight-threat-008" "Spider Riders" do
  race Orc
  cost 1
  loyalty 1
  power 1
  hitPoints 1
  trait Cavalry
  body "Battlefield. This unit gains {power}{power} while attacking."
  battlefield $ constant \self ->
    when self.attacking $ gainPower self 2

warPaint :: CardDef Support
warPaint = supportCard "the-skavenblight-threat-009" "War Paint" do
  race Orc
  cost 0
  loyalty 1
  trait Attachment
  body "Attach to a target unit in your battlefield. Attached unit gains {power} for each damage on it."
  attachedTo \_self unit -> do
    let Damage d = unit.damage
    when (d > 0) $ gainPower unit d

arrerBoyz :: CardDef Unit
arrerBoyz = unitCard "path-of-the-zealot-028" "Arrer Boyz" do
  race Orc
  cost 4
  loyalty 2
  power 1
  hitPoints 3
  trait Ranger
  body
    "Battlefield. Action: Spend 2 resources to deal 1 damage to one target unit in any \
    \battlefield. Arrer Boyz then takes 1 damage."
  battlefield $ action "Loose arrers" 2 \usage ->
    withTarget usage.user (unitWhere \u -> u.zone == BattlefieldZone) \k -> do
      dealDamage k 1
      dealDamage usage.self.key 1

wolfRiderAssault :: CardDef Tactic
wolfRiderAssault = tacticCard "path-of-the-zealot-029" "Wolf Rider Assault" do
  race Orc
  cost 0
  loyalty 1
  body
    "Action: Move one target {orc} unit from your kingdom or your quest zone to your \
    \battlefield. That unit must attack this turn if able."
  playableWhen $ hasTarget movableOrc
  whenResolved \self ->
    -- "Must attack this turn" is left to the controller — the engine
    -- doesn't force attack declarations.
    withTarget self.controller movableOrc \k ->
      moveUnit k BattlefieldZone
  where
    movableOrc = UnitMatching \me _ u ->
      u.controller == me && u `isRace` Orc && u.zone /= BattlefieldZone

ugrokBeardburna :: CardDef Unit
ugrokBeardburna = unitCard "tooth-and-claw-048" "Ugrok Beardburna" do
  hero
  trait Warrior
  race Orc
  cost 5
  loyalty 2
  power 3
  hitPoints 5
  body "Limit one Hero per zone. This unit gains {power} for each damage on it."
  selfPower \_g u -> let Damage d = u.damage in d

mobUp :: CardDef Tactic
mobUp = tacticCard "tooth-and-claw-049" "Mob Up" do
  race Orc
  cost 0
  loyalty 1
  body "Action: Until the end of the turn, combat damage cannot be cancelled."
  whenResolved \_ -> push SetCombatDamageUncancellable

datsMine :: CardDef Quest
datsMine = questCard "tooth-and-claw-050" "Dat's Mine!" do
  race Orc
  cost 0
  loyalty 2
  body
    "Quest. You may spend resources from this card to pay for Attachment cards that are \
    \played from your hand. \
    \Quest. Forced: Place 1 resource token on this card at the beginning of your turn if a unit is questing here."
  forced accrueTokenWhileQuesting
  paysAttachmentCosts

ironBoyz :: CardDef Unit
ironBoyz = unitCard "the-deathmaster-s-dance-070" "Iron Boyz" do
  race Orc
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Warrior
  toughness 1
  body "Toughness 1 (whenever this unit is assigned damage, cancel 1 of that damage)."

grimgorsSpike :: CardDef Support
grimgorsSpike = supportCard "the-deathmaster-s-dance-071" "Grimgor's Spike" do
  race Orc
  cost 1
  loyalty 2
  traits [Attachment, Weapon]
  body
    "Attach to an {orc} unit. If attached unit is defending alone, destroy all attacking \
    \units when they take combat damage this phase."
  onReceive $ Receive \msg _owner self -> case msg of
    DealDamageToUnit uk n | n > 0 -> spikeImpale self uk
    DealDamageToUnitUncancellable uk n | n > 0 -> spikeImpale self uk
    _ -> pure ()
  where
    spikeImpale self uk = do
      g <- getGame
      case (self.attachedTo, g.combat) of
        (Just hostKey, Just cs)
          | cs.defenders == [hostKey]
          , uk `elem` cs.attackers ->
              destroyUnit uk
        _ -> pure ()

swarmEm :: CardDef Tactic
swarmEm = tacticCard "the-deathmaster-s-dance-072" "Swarm 'Em" do
  race Orc
  -- Printed cost X, where X is forced to the count below — modelled
  -- as printed 0 plus a self cost adjustment so the engine collects
  -- exactly X.
  cost 0
  loyalty 2
  body
    "Action: Deal X damage to one target defending unit, where X is the number of units \
    \and developments in your battlefield."
  selfCostAdjust \g pk -> swarmCount g pk
  playableWhen $ hasTarget defendingUnit
  whenResolved \self -> do
    g <- getGame
    let x = swarmCount g self.controller
    when (x > 0) $
      withTarget self.controller defendingUnit \k -> dealDamage k x
  where
    swarmCount g pk =
      let me = playerOf pk g
          Developments d = me.capital.battlefield.developments
          units = length [u | u <- g.units, u.controller == pk, u.zone == BattlefieldZone]
       in d + units

snotlingPumpWagon :: CardDef Unit
snotlingPumpWagon = unitCard "the-warpstone-chronicles-090" "Snotling Pump Wagon" do
  race Orc
  cost 2
  loyalty 1
  power 2
  hitPoints 1
  trait Warrior
  body "Battlefield only."
  battlefieldOnly

bashasBloodaxe :: CardDef Support
bashasBloodaxe = supportCard "the-warpstone-chronicles-091" "Basha's Bloodaxe" do
  unique
  race Orc
  cost 2
  loyalty 1
  traits [Attachment, Relic]
  body
    "Attach to a target {orc} unit. Corrupt that unit. Attached unit deals +2 damage in \
    \combat. While attached unit is attacking, double all damage dealt to the defending \
    \opponent's capital."
  onEnterPlay \_owner self -> for_ self.attachedTo corrupt
  supportCombat \_g s u ->
    if s.attachedTo == Just u.key then 2 else 0
  doublesCapitalDamage \g s targetPk ->
    case (s.attachedTo, g.combat) of
      (Just hostKey, Just cs) ->
        hostKey `elem` cs.attackers && cs.defendingPlayer == targetPk
      _ -> False

thickSkinned :: CardDef Support
thickSkinned = supportCard "the-warpstone-chronicles-092" "Thick-Skinned" do
  race Orc
  cost 0
  loyalty 2
  trait Attachment
  body
    "Attach to a target {orc} unit. Action: Sacrifice this card to redirect any number of \
    \combat damage assigned to attached unit to one target unit you control."
  actionWith "Soak it up" 0 [SacrificeSelf] \usage -> do
    g <- getGame
    for_ usage.self.attachedTo \host -> do
      let pending = case g.combat of
            Just cs ->
              sum
                [ pd.cancellable
                | pd <- cs.pendingAssignments
                , pd.target == PDUnit host
                ]
            Nothing -> 0
      when (pending > 0) $
        withTarget usage.user
          (UnitMatching \me _ u -> u.controller == me && u.key /= host)
          \dst -> do
            n <- chooseAmount usage.user 1 pending "Redirect how much combat damage?"
            push (RedirectAssignedUnitDamage host dst n)

snotlingSaboteurs :: CardDef Unit
snotlingSaboteurs = unitCard "arcane-fire-110" "Snotling Saboteurs" do
  race Orc
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Ranger
  body
    "Action: Spend 2 resources and sacrifice this unit to destroy one target support card \
    \or development."
  actionWith "Sabotage" 2 [SacrificeSelf] \usage ->
    withTarget usage.user (AnySupportCard `Or` AnyDevelopmentZone) \case
      TargetSupportOption k -> destroySupport k
      TargetZoneOption owner zk -> destroyDevelopment owner zk
      _ -> pure ()

daBrainbusta :: CardDef Tactic
daBrainbusta = tacticCard "arcane-fire-111" "Da Brainbusta!" do
  race Orc
  cost 10
  loyalty 3
  traits [Epic, Spell]
  body "Play at the beginning of your turn. Action: Destroy all opponents' units."
  playableWhen \g pk ->
    g.currentPlayer == pk
      && case g.actionWindow of
        Just aw -> aw.trigger == BeginningOfTurnActionWindow
        Nothing -> False
  whenResolved \self -> do
    g <- getGame
    for_ [u.key | u <- g.units, u.controller /= self.controller] destroyUnit

easyPickins :: CardDef Tactic
easyPickins = tacticCard "arcane-fire-112" "Easy Pickin's" do
  race Orc
  cost 2
  loyalty 1
  body
    "Action: The unit in play with the lowest printed cost must be sacrificed. You choose \
    \which unit in case of a tie."
  playableWhen \g _pk -> not (null g.units)
  whenResolved \self -> do
    g <- getGame
    let printedCost u = case u.cardDef.cost of
          Fixed n -> n
          Variable -> 0
    case g.units of
      [] -> pure ()
      us -> do
        let lowest = minimum (map printedCost us)
            candidates = [u.key | u <- us, printedCost u == lowest]
        forcePickUnit self.controller candidates
          "Easy Pickin's: choose which lowest-cost unit is sacrificed."
          destroyUnit

-- | A small lifted lookup that retrieves the unit list from
-- 'getGame'. The 'effects' DSL works inside 'EffectM', not a 'HasGame'
-- monad; we drive it through getGame manually.
getGameUnits :: HasGame m => m [UnitDetails]
getGameUnits = (.units) <$> getGame

-- | True if any zone (either player's) has at least one development.
-- Used as a 'playableWhen' gate for development-targeting tactics.
hasAnyDevelopment :: Game -> Bool
hasAnyDevelopment g =
  let withDev :: [Zone] -> Bool
      withDev zs = any (\z -> case z.developments of Developments n -> n > 0) zs
   in withDev g.player1.capital.zones || withDev g.player2.capital.zones

-- Oaths of Vengeance ---------------------------------------------------

wolfChariot :: CardDef Unit
wolfChariot = unitCard "oaths-of-vengeance-030" "Wolf Chariot" do
  race Orc
  cost 5
  loyalty 2
  power 3
  hitPoints 4
  traits [Cavalry, Goblin]
  battlefieldOnly
  body
    "Battlefield only. Action: When this unit attacks, sacrifice a unit you control. \
    \This unit gains {power} equal to the sacrificed unit's loyalty until the end of the phase."
  onMyAttackDeclared \_owner self _zone _attackers ->
    sacrificeOwnUnit self.controller "Wolf Chariot: sacrifice a unit you control." \k -> do
      g <- getGame
      let loy = maybe 0 (\u -> u.cardDef.loyalty) (findUnit k g)
      when (loy > 0) $ until EndOfTurn $ buffPower self.key loy

-- Battle for the Old World ---------------------------------------------

mobOHutz :: CardDef Support
mobOHutz = supportCard "battle-for-the-old-world-043" "Mob O' Hutz" do
  race Orc
  cost 1
  loyalty 1
  power 1
  trait Building
  body "If you control a faceup non-[Orc] unit or support card, sacrifice this card."
  sacrificeWhenBoardChanges \g self ->
    controlsNonRaceUnitOrSupport g self.controller Orc

-- The Ruinous Hordes ---------------------------------------------------

wolfGobbos :: CardDef Unit
wolfGobbos = unitCard "the-ruinous-hordes-094" "Wolf Gobbos" do
  race Orc
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Goblin
  body
    "Action: When this unit survives an attack on an opponent's zone, sacrifice this unit \
    \to destroy target unit in that zone."
  onCombatResolveAsAttacker \_owner self cs -> do
    g <- getGame
    when (isJust (findUnit self.key g)) $
      may self.controller "Wolf Gobbos: sacrifice to destroy a unit in that zone?" do
        destroyUnit self.key
        withTarget self.controller
          (UnitMatching \_pk _g u -> u.zone == cs.targetZone && u.controller /= self.controller)
          destroyUnit

-- Faith and Steel ------------------------------------------------------

gobboBigBoss :: CardDef Unit
gobboBigBoss = unitCard "faith-and-steel-111" "Gobbo Big Boss" do
  race Orc
  cost 3
  loyalty 1
  power 0
  hitPoints 3
  trait Goblin
  body "This unit gains {power} for each attacking or defending [Orc] unit you control."
  selfPower \g u -> case g.combat of
    Just cs ->
      length
        [ k
        | k <- cs.attackers <> cs.defenders
        , Just v <- [findUnit k g]
        , v.controller == u.controller
        , Orc `elem` v.cardDef.races
        ]
    Nothing -> 0

goblinRaiders :: CardDef Unit
goblinRaiders = unitCard "oaths-of-vengeance-031" "Goblin Raiders" do
  race Orc
  cost 1
  loyalty 2
  power 1
  hitPoints 1
  trait Goblin
  battlefieldOnly
  raider 2
  body "Battlefield only. Raider 2."

-- Bloodquest: Rising Dawn -----------------------------------------------

orcBully :: CardDef Unit
orcBully = unitCard "rising-dawn-005" "Orc Bully" do
  race Orc
  cost 3
  loyalty 2
  power 2
  hitPoints 2
  trait Elite
  body "Forced: When this unit enters play, deal 1 damage to each Goblin unit you control."
  onEnterPlay \_owner self -> do
    g <- getGame
    for_ [u.key | u <- g.units, u.controller == self.controller, Goblin `elem` u.cardDef.traits] \k ->
      dealDamage k 1

-- Bloodquest: Shield of the Gods ----------------------------------------

manglerSquigs :: CardDef Unit
manglerSquigs = unitCard "shield-of-the-gods-105" "Mangler Squigs" do
  race Orc
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  trait Creature
  body
    "Action: When this unit attacks, reveal the top card of your deck. If the printed cost of \
    \the revealed card is odd, double this unit's power until the end of the turn. Otherwise, \
    \this unit takes 2 damage."
  onMyAttackDeclared \owner self _zone _attackers ->
    case owner.deck of
      [] -> pure ()
      (top : _) -> do
        g <- getGame
        if odd (someCardCost top.def)
          then whenJust (findUnit self.key g) \u ->
            until EndOfTurn $ buffPower self.key u.effectivePower
          else dealDamage self.key 2

-- The Capital Cycle ----------------------------------------------------

rugludsArmouredOrcs :: CardDef Unit
rugludsArmouredOrcs = unitCard "the-iron-rock-045" "Ruglud's Armoured Orcs" do
  race Orc
  cost 4
  loyalty 1
  power 2
  hitPoints 2
  trait Warrior
  body "Toughness X. X is the highest loyalty of an {orc} card you control."
  selfToughness \g u -> highestLoyaltyControlled Orc g u.controller

squigLobber :: CardDef Unit
squigLobber = unitCard "the-iron-rock-044" "Squig Lobber" do
  race Orc
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Siege
  body
    "Battlefield. Action: At the beginning of your turn, put a resource token on this \
    \card. Action: Remove a resource token from this unit to deal 1 indirect damage to \
    \target opponent."
  battlefield $ do
    onMyTurnBegin \_owner self -> push (AdjustUnitTokens self.key 1)
    action "Lob a squig" 0 \usage -> do
      g <- getGame
      whenJust (findUnit usage.self.key g) \u ->
        when (u.tokens > 0) do
          push (AdjustUnitTokens u.key (-1))
          indirectDamage usage.user.next 1

raidingParties :: CardDef Quest
raidingParties = questCard "the-iron-rock-060" "Raiding Parties" do
  race Orc
  cost 0
  loyalty 3
  body
    "Quest. Action: When this card enters play, draw a card. Quest. Action: When you play \
    \an {orc} non-Attachment support card from your hand, destroy target development if a \
    \unit is questing here."
  onEnterPlay \_owner self -> drawCard self.controller
  onQuestSupportPayoff Orc \self ->
    withTarget self.controller AnyDevelopmentZone \(owner, zk) ->
      destroyDevelopment owner zk

snotlingAmbush :: CardDef Tactic
snotlingAmbush = tacticCard "the-iron-rock-050" "Snotling Ambush" do
  race Orc
  cost 2
  loyalty 3
  body
    "Action: Discard a card from your hand with X loyalty to discard X resources from \
    \target player."
  -- "target player": the opponent, the only meaningful pick.
  playableWhen \g pk -> not (null (playerOf pk g).hand)
  whenResolved \self ->
    discardForLoyalty self.controller \x ->
      when (x > 0) $ payResources self.controller.next x

bannaThief :: CardDef Unit
bannaThief = unitCard "the-iron-rock-042" "Banna Thief" do
  race Orc
  cost 2
  loyalty 1
  power 0
  hitPoints 2
  traits [Goblin, StandardBearer]
  body "Action: When a unit enters this zone, target unit gains power until the end of the turn."
  onUnitEnterMyZone \_owner self _uk ->
    withTarget self.controller AnyUnit \k -> until EndOfTurn $ buffPower k 1

-- Cataclysm cycle ------------------------------------------------------

makkaGreenfist :: CardDef Unit
makkaGreenfist = unitCard "cataclysm-009" "Makka Greenfist" do
  race Orc
  cost 3
  loyalty 3
  power 1
  hitPoints 3
  traits [Hero, Shaman]
  limitOneHeroPerZone
  body
    "Limit 1 Hero per zone. This unit gains {power}{power}{power} while \
    \attacking a damaged zone."
  combatPower \g self -> case g.combat of
    Just cs | self.key `elem` cs.attackers && zoneDamaged g cs.defendingPlayer cs.targetZone -> 3
    _ -> 0

bigGuns :: CardDef Support
bigGuns = supportCard "cataclysm-013" "Big Guns" do
  race Orc
  cost 2
  loyalty 2
  power 0
  trait Condition
  battlefieldOnly
  body "Battlefield. Each attacking {orc} unit deals +1 damage in combat."
  supportCombat \_g _s u ->
    if u.attacking && Orc `elem` u.cardDef.races then 1 else 0

-- | True iff the named section of the named player's capital currently
-- has one or more damage tokens (and is not yet burned). Used by Makka
-- Greenfist's "attacking a damaged zone" bonus.
zoneDamaged :: Game -> PlayerKey -> ZoneKind -> Bool
zoneDamaged g pk zk =
  let p = playerOf pk g
      z = case zk of
        KingdomZone -> p.capital.kingdom
        QuestZone -> p.capital.quest
        BattlefieldZone -> p.capital.battlefield
   in case z.damage of Damage d -> d > 0

-- The Morrslieb cycle ---------------------------------------------------

lootedUmieTown :: CardDef Support
lootedUmieTown = supportCard "the-chaos-moon-025" "Looted Umie Town" do
  race Orc
  cost 3
  loyalty 2
  power 1
  trait Building
  body "This card gains {power} for each unit in this zone."
  zonePowerAura \g s z ->
    if z == s.zone
      then length [u | u <- g.units, u.controller == s.controller, u.zone == s.zone]
      else 0

poisonousWyvern :: CardDef Unit
poisonousWyvern = unitCard "omens-of-ruin-003" "Poisonous Wyvern" do
  race Orc
  cost 4
  loyalty 2
  power 2
  hitPoints 6
  trait Creature

riverTroll :: CardDef Unit
riverTroll = unitCard "omens-of-ruin-004" "River Troll" do
  race Orc
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  trait Troll
  body "Forced: At the beginning of your turn, heal all damage from this unit."
  onMyTurnBegin \_owner self -> healUnit self.key 99

squigHopper :: CardDef Unit
squigHopper = unitCard "signs-in-the-stars-065" "Squig Hopper" do
  race Orc
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  traits [Goblin, Warrior]
  body "This unit gains {power} as long as you control another Goblin unit."
  selfPower \g self ->
    if any (\u -> u.controller == self.controller && u.key /= self.key && Goblin `elem` u.cardDef.traits) g.units
      then 1
      else 0

bigBoss :: CardDef Unit
bigBoss = unitCard "the-eclipse-of-hope-084" "Big Boss" do
  race Orc
  cost 6
  loyalty 3
  power 3
  hitPoints 5
  trait Warrior
  body
    "Forced: At the beginning of your turn, the unit with the lowest printed \
    \cost must be sacrificed. You choose in case of a tie."
  -- TODO: confirm scope — this considers every unit in play (both
  -- players') for the lowest printed cost, and lets Big Boss's
  -- controller resolve ties. Verify against the ruling on whose units
  -- are eligible; narrow to a single player's units if required.
  onMyTurnBegin \_owner self -> do
    g <- getGame
    let costOf u = case u.cardDef.cost of Fixed v -> v; _ -> maxBound
        m = minimum (maxBound : map costOf g.units)
        cands = [u.key | u <- g.units, costOf u == m]
    forcePickUnit self.controller cands "Big Boss: sacrifice the lowest-cost unit." destroyUnit

smashEm :: CardDef Tactic
smashEm = tacticCard "fiery-dawn-106" "Smash 'Em!" do
  race Orc
  cost 3
  loyalty 3
  body "Action: Destroy all units with a printed cost of 2 or lower."
  whenResolved \_self -> do
    g <- getGame
    for_ [u.key | u <- g.units, costAtMost 2 u.cardDef] destroyUnit

-- The Enemy cycle -------------------------------------------------------

madShaman :: CardDef Unit
madShaman = unitCard "bleeding-sun-110" "Mad Shaman" do
  race Orc
  cost 4
  loyalty 2
  power 1
  hitPoints 3
  trait Shaman
  body
    "This unit gains {power} for each resource token on it. If this unit has \
    \more resource tokens on it than hit points, sacrifice it. Forced: At the \
    \beginning of your turn, put a resource token on this unit."
  selfPower \_g self -> self.tokens
  onMyTurnBegin \_owner self -> do
    push (AdjustUnitTokens self.key 1)
    when (self.tokens + 1 > self.effectiveMaxHP) $ destroyUnit self.key

giantSpider :: CardDef Unit
giantSpider = unitCard "bleeding-sun-111" "Giant Spider" do
  race Orc
  cost 3
  loyalty 2
  power 2
  hitPoints 4
  trait Creature
  battlefieldOnly
  body "Battlefield only. This unit cannot attack unless the defending zone has at least one unit."
  canAttack \g pk zone _u ->
    any (\v -> v.controller /= pk && v.zone == zone) g.units

followersOfSkarsnik :: CardDef Unit
followersOfSkarsnik = unitCard "the-fall-of-karak-grimaz-029" "Followers of Skarsnik" do
  race Orc
  cost 3
  loyalty 1
  power 1
  hitPoints 2
  trait Goblin
  body "This unit gains {power} and +1 hit points for every other copy of \"Followers of Skarsnik\" you control."
  selfPower \g self -> otherCopies g self
  selfHP \g self -> otherCopies g self
  where
    otherCopies g self =
      length
        [ u
        | u <- g.units
        , u.controller == self.controller
        , u.key /= self.key
        , u.cardDef.code == self.cardDef.code
        ]

-- The Enemy cycle (batch 2) ---------------------------------------------

githitFroatcutta :: CardDef Unit
githitFroatcutta = unitCard "redemption-of-a-mage-070" "Githit Froatcutta" do
  hero
  race Orc
  cost 5
  loyalty 2
  power 2
  hitPoints 4
  trait Goblin
  body "Limit one Hero per zone. This unit gains {power} for each damaged unit you control."
  selfPower \g self ->
    length [u | u <- g.units, u.controller == self.controller, isDamaged u]

highMountainTroll :: CardDef Unit
highMountainTroll = unitCard "the-fourth-waystone-089" "High Mountain Troll" do
  race Orc
  cost 6
  loyalty 3
  power 4
  hitPoints 5
  trait Troll
  body "Forced: At the beginning of your turn, heal all damage on this unit."
  onMyTurnBegin \_owner self -> healUnit self.key 99

tooArdToDie :: CardDef Support
tooArdToDie = supportCard "redemption-of-a-mage-071" "Too 'Ard to Die" do
  race Orc
  cost 2
  loyalty 3
  trait Attachment
  body "Attach to target [Orc] unit in your battlefield. Attached unit gains Toughness X, where X is the damage on that unit."
  supportToughnessAura \_g s u ->
    if Just u.key == s.attachedTo
      then let Damage d = u.damage in d
      else 0

smashEmBashEm :: CardDef Quest
smashEmBashEm = questCard "the-fall-of-karak-grimaz-033" "Smash 'Em, Bash 'Em" do
  race Orc
  cost 0
  loyalty 2
  body
    "Quest. Action: Discard 1 resource token from this card to have target unit \
    \in the battlefield gain {power} until the end of the turn. Quest. Forced: \
    \Place 1 resource token on this card at the beginning of your turn if a unit \
    \is questing here."
  forced accrueTokenWhileQuesting
  spendTokens "Buff a battlefield unit" 1 \u ->
    withTarget u.user (UnitMatching \_pk _g unit -> unit.zone == BattlefieldZone) \k ->
      until EndOfTurn $ buffPower k 1

smashDerBeards :: CardDef Tactic
smashDerBeards = tacticCard "bleeding-sun-113" "Smash Der Beards" do
  race Orc
  cost 2
  loyalty 3
  body "Action: Target attacking unit gains {power} for each development in the defending player's zone."
  playableWhen \g pk -> hasTarget attackingUnit g pk
  whenResolved \self -> do
    g <- getGame
    let n = case g.combat of
          Just cs ->
            let p = playerOf cs.defendingPlayer g
                Developments d = case cs.targetZone of
                  KingdomZone -> p.capital.kingdom.developments
                  QuestZone -> p.capital.quest.developments
                  BattlefieldZone -> p.capital.battlefield.developments
             in d
          Nothing -> 0
    withTarget self.controller attackingUnit \k -> until EndOfTurn $ buffPower k n

disStuffBurnsGud :: CardDef Tactic
disStuffBurnsGud = tacticCard "the-fourth-waystone-090" "Dis Stuff Burns Gud" do
  race Orc
  cost 3
  loyalty 2
  body "Action: Sacrifice a unit to destroy target Building support card. Then, each player takes 3 indirect damage."
  playableWhen \g pk -> any (\u -> u.controller == pk) g.units
  whenResolved \self ->
    sacrificeOwnUnit self.controller "Sacrifice a unit." \_ -> do
      withTarget self.controller
        (SupportMatching \_pk _g s -> Building `elem` s.cardDef.traits)
        destroySupport
      eachPlayer \pk -> indirectDamage pk 3

-- Assault on Ulthuan ---------------------------------------------------

wyvernRider :: CardDef Unit
wyvernRider = unitCard "assault-on-ulthuan-049" "Wyvern Rider" do
  race Orc
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Cavalry
  body "Battlefield. This unit gains {power} if you have no developments in this zone."
  battlefield $ constant \self -> do
    g <- getGame
    when (devsInZone g self == 0) $ gainPower self 1

scrapHeap :: CardDef Support
scrapHeap = supportCard "assault-on-ulthuan-050" "Scrap Heap" do
  race Orc
  cost 1
  loyalty 2
  power 0
  trait Siege
  body "[Orc] units in this zone get +1 hit points."
  supportHPAura \_g s u ->
    if u.controller == s.controller && u.zone == s.zone && Orc `elem` u.cardDef.races
      then 1
      else 0

footOfGork :: CardDef Tactic
footOfGork = tacticCard "assault-on-ulthuan-051" "Foot of Gork" do
  race Orc
  cost 2
  loyalty 3
  body "Play during your turn. Action: Destroy one target unit with printed cost 2 or lower."
  playableWhen \g pk ->
    g.currentPlayer == pk && hasTarget (unitWhere (costAtMost 2 . (.cardDef))) g pk
  whenResolved \self ->
    withTarget self.controller (unitWhere (costAtMost 2 . (.cardDef))) destroyUnit

-- March of the Damned --------------------------------------------------

savageBoyz :: CardDef Unit
savageBoyz = unitCard "march-of-the-damned-016" "Savage Boyz" do
  race Orc
  cost 4
  loyalty 2
  power 2
  hitPoints 4
  trait Warrior
  body "Action: Spend 1 resource to deal 1 damage to target unit you control."
  actionFriendlyUnit "Whip into a frenzy" 1 \_usage k -> dealDamage k 1

forestGoblins :: CardDef Unit
forestGoblins = unitCard "march-of-the-damned-017" "Forest Goblins" do
  race Orc
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Goblin
  body "Action: When this unit enters play, target unit gains {power} until the end of the turn."
  onEnterPlay \_owner self ->
    withTarget self.controller AnyUnit \k -> until EndOfTurn $ buffPower k 1

trollDen :: CardDef Support
trollDen = supportCard "march-of-the-damned-018" "Troll Den" do
  race Orc
  cost 4
  loyalty 2
  power 3
  trait Building
  body
    "Forced: At the beginning of your turn, deal 2 damage to target unit you control in this \
    \zone or sacrifice this card."
  forced \self -> onTurnBegin self.controller do
    g <- getGame
    let candidates =
          [ u.key
          | u <- g.units
          , u.controller == self.controller
          , u.zone == self.zone
          ]
    if null candidates
      then destroySupport self.key
      else do
        feed <- askYesNo self.controller
          "Deal 2 damage to one of your units in Troll Den's zone? (Decline to sacrifice Troll Den.)"
        if feed
          then forcePickUnit self.controller candidates
            "Choose a unit to take 2 damage." \k -> dealDamage k 2
          else destroySupport self.key

spidaHuntin :: CardDef Tactic
spidaHuntin = tacticCard "march-of-the-damned-019" "Spida Huntin'" do
  race Orc
  cost 1
  loyalty 2
  body
    "Action: Deal 2 damage to target unit you control. That unit gains {power}{power} until \
    \the end of the turn."
  playableWhen $ hasTarget ownUnit
  whenResolved \self ->
    withTarget self.controller ownUnit \k -> do
      dealDamage k 2
      until EndOfTurn $ buffPower k 2

-- Legends (deluxe expansion) -------------------------------------------

blindRiverGoblin :: CardDef Unit
blindRiverGoblin = unitCard "legends-010" "Blind River Goblin" do
  race Orc
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Goblin
  body "If you control a legend, this unit gains {power}."
  selfPower \g self ->
    if isJust (legendOf self.controller g) then 1 else 0

redEadBoyz :: CardDef Unit
redEadBoyz = unitCard "legends-011" "Red 'Ead Boyz" do
  race Orc
  cost 5
  loyalty 2
  power 2
  hitPoints 4
  traits [Warrior, Elite]
  body
    "Action: When this unit enters play, deal 1 damage to each unit with \
    \printed cost 3 or lower."
  onEnterPlay \_owner _self -> do
    g <- getGame
    for_ [u | u <- g.units, costAtMost 3 u.cardDef] \u ->
      dealDamage u.key 1

frenziedBigUn :: CardDef Unit
frenziedBigUn = unitCard "legends-012" "Frenzied Big 'Un" do
  race Orc
  cost 3
  loyalty 2
  power 1
  hitPoints 3
  trait Warrior
  body "Battlefield. Each damaged unit you control gains {power}."
  unitAura \_g self target ->
    if self.zone == BattlefieldZone
        && target.controller == self.controller
        && isDamaged target
      then 1
      else 0

-- Hidden Kingdoms (deluxe expansion) -----------------------------------

orcishArtillery :: CardDef Tactic
orcishArtillery = tacticCard "hidden-kingdoms-054" "Orcish Artillery" do
  race Orc
  cost 2
  loyalty 1
  body
    "Play only on your turn. Action: Deal 1 damage to each attacking unit you \
    \control and deal 1 damage to each unit in the attacked zone."
  playableWhen \g pk -> g.currentPlayer == pk && inCombat g pk
  whenResolved \_self -> withCombat \cs -> do
    for_ cs.attackers \k -> dealDamage k 1
    g <- getGame
    for_
      [u.key | u <- g.units, u.controller == cs.defendingPlayer, u.zone == cs.targetZone]
      \k -> dealDamage k 1

-- Ambush riders (Eternal War cycle) ------------------------------------

iddenBoy :: CardDef Unit
iddenBoy = unitCard "days-of-blood-013" "'Idden Boy" do
  race Orc
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Warrior
  body
    "Orc only. Ambush 1. Action: When this unit ambushes, it gains \
    \{power}{power} until the end of the phase."
  ambush 1
  onAmbush \_owner self -> until EndOfTurn $ buffPower self.key 2

dedScaryBoy :: CardDef Unit
dedScaryBoy = unitCard "battle-for-the-old-world-044" "Ded Scary Boy" do
  race Orc
  cost 3
  loyalty 1
  power 1
  hitPoints 3
  trait Berserker
  body
    "Orc only. Ambush 2. Action: When this unit ambushes, it takes 1 damage. \
    \Then, units with no damage lose {power}{power} until the end of the phase."
  ambush 2
  onAmbush \_owner self -> do
    dealDamage self.key 1
    g <- getGame
    for_ [u.key | u <- g.units, u.key /= self.key, not (isDamaged u)] \k ->
      until EndOfTurn $ buffPower k (-2)

getEmLadz :: CardDef Tactic
getEmLadz = tacticCard "glory-of-days-past-072" "Get 'Em Ladz!" do
  race Orc
  cost 2
  loyalty 1
  body
    "Orc only. Ambush 0. Action: Choose a zone. Until the end of the phase, \
    \draw a card for each damage dealt to that zone."
  ambush 0
  whenResolved \self ->
    withTarget self.controller AnyCapital \(owner, zone) ->
      watchZoneForDamageDraw self.controller owner zone

-- Reveal-the-top units --------------------------------------------------

recklessBoyz :: CardDef Unit
recklessBoyz = unitCard "hidden-kingdoms-053" "Reckless Boyz" do
  race Orc
  cost 3
  loyalty 1
  power 3
  hitPoints 3
  trait Warrior
  battlefieldOnly
  body
    "Battlefield only. Forced: When this unit attacks, reveal the top card of \
    \your deck. If the cost of the revealed card is even, deal 2 damage to \
    \this unit. Then, shuffle your deck."
  onMyAttackDeclared \_owner self _zone _attackers ->
    revealTopOfDeck self.controller 1 \r -> do
      case r.cards of
        (c : _) | even (someCardCost c.def) -> dealDamage self.key 2
        _ -> pure ()
      shuffleDeck self.controller

nightGoblinFanatic :: CardDef Unit
nightGoblinFanatic = unitCard "the-chaos-moon-024" "Night Goblin Fanatic" do
  race Orc
  cost 2
  loyalty 2
  power 1
  hitPoints 2
  trait Goblin
  body
    "Action: When this unit attacks, reveal the top card of your deck. If the \
    \printed cost of the revealed card is odd, deal 2 uncancellable damage to \
    \each unit in the defending zone. Otherwise, sacrifice this unit. Then, \
    \shuffle your deck."
  onMyAttackDeclared \_owner self zone _attackers ->
    revealTopOfDeck self.controller 1 \r -> do
      case r.cards of
        (c : _) | odd (someCardCost c.def) -> do
          g <- getGame
          for_
            [u.key | u <- g.units, u.zone == zone, u.controller == self.controller.next]
            \k -> dealUncancellableDamage k 2
        _ -> destroyUnit self.key
      shuffleDeck self.controller

sneakyGit :: CardDef Unit
sneakyGit = unitCard "signs-in-the-stars-064" "Sneaky Git" do
  race Orc
  cost 2
  loyalty 1
  power 1
  hitPoints 2
  trait Goblin
  body
    "Action: When this unit attacks, reveal the top card of your deck. If the \
    \printed cost of the revealed card is odd, deal 2 damage to target \
    \capital. Otherwise, deal 2 damage to your capital. Then, shuffle your deck."
  onMyAttackDeclared \_owner self _zone _attackers ->
    revealTopOfDeck self.controller 1 \r -> do
      case r.cards of
        (c : _) | odd (someCardCost c.def) ->
          withTarget self.controller AnyCapital \(owner, zk) -> dealZoneDamage owner zk 2
        _ ->
          withTarget self.controller
            (CapitalMatching \pk (owner, _) -> owner == pk)
            \(owner, zk) -> dealZoneDamage owner zk 2
      shuffleDeck self.controller

deyzBigga :: CardDef Tactic
deyzBigga = tacticCard "fiery-dawn-105" "Dey'z Bigga" do
  race Orc
  cost 1
  loyalty 3
  body
    "Action: Each unit with the highest printed cost cannot be targeted by \
    \card effects until the end of the turn."
  whenResolved \_self -> do
    g <- getGame
    let costOf u = someCardCost (UnitCardDef u.cardDef)
        maxCost = maximum (0 : map costOf g.units)
    when (maxCost > 0) $
      for_ [u.key | u <- g.units, costOf u == maxCost] \k ->
        until EndOfTurn $ untargetable False k
