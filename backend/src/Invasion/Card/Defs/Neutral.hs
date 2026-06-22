{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Neutral core cards (core-111..127, no core-119). 16 cards.
-- Neutrals don't belong to a faction (no 'race' call), so they
-- contribute no race symbol toward loyalty and pay the full loyalty
-- surcharge — which for every core neutral is 0 since they're
-- printed with loyalty 0.
module Invasion.Card.Defs.Neutral (module Invasion.Card.Defs.Neutral) where

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

contestedVillage :: CardDef Support
contestedVillage = supportCard "core-111" "Contested Village" do
  cost 1
  loyalty 0
  power 1
  trait Building
  limited
  body "Limited (you cannot play more than one limited card per turn)."

contestedFortress :: CardDef Support
contestedFortress = supportCard "core-112" "Contested Fortress" do
  cost 3
  loyalty 0
  power 1
  trait Building
  limited
  body "Limited (you cannot play more than one limited card per turn). Cancel 1 damage to your capital each turn."
  -- Evaluated live by the engine's capital-damage pipeline: cancels 1
  -- point per turn on EITHER player's turn (the old turn-begin token
  -- was wiped at the opponent's BeginTurn, so it never protected
  -- against actual attacks).
  capitalShieldEachTurn

contestedStronghold :: CardDef Support
contestedStronghold = supportCard "core-113" "Contested Stronghold" do
  cost 4
  loyalty 0
  power 1
  trait Building
  limited
  body "Limited (you cannot play more than one limited card per turn). This support gains {power} for each of your developments in this zone."
  zonePowerAura \g self zk ->
    if zk == self.zone
      then let me = playerOf self.controller g
               Developments n = zoneDevs me.capital zk
            in n
      else 0

armoury :: CardDef Support
armoury = supportCard "core-114" "Armoury" do
  cost 2
  loyalty 0
  power 1
  trait Building
  limited
  body "Limited (you cannot play more than one limited card per turn). Kingdom. This card gains {power} while you have at least two developments in this zone."
  zonePowerAura \g self zk ->
    if zk == self.zone && self.zone == KingdomZone
      then let me = playerOf self.controller g
               Developments n = zoneDevs me.capital zk
            in if n >= 2 then 1 else 0
      else 0

forgottenCemetery :: CardDef Support
forgottenCemetery = supportCard "core-115" "Forgotten Cemetery" do
  cost 2
  loyalty 0
  power 1
  trait Building
  limited
  body "Limited (you cannot play more than one limited card per turn). Quest. This card gains {power} while you have at least two developments in this zone."
  zonePowerAura \g self zk ->
    if zk == self.zone && self.zone == QuestZone
      then let me = playerOf self.controller g
               Developments n = zoneDevs me.capital zk
            in if n >= 2 then 1 else 0
      else 0

warpstoneExcavation :: CardDef Support
warpstoneExcavation = supportCard "core-116" "Warpstone Excavation" do
  cost 0
  loyalty 0
  power 1
  trait Warpstone
  body "Your units enter this zone corrupted."
  onReceive $ Receive \msg _owner self -> case msg of
    UnitEnteredPlay pk uk
      | pk == self.controller -> do
          g <- getGame
          case findUnit uk g of
            Just u | u.zone == self.zone -> push (CorruptUnit uk)
            _ -> pure ()
    _ -> pure ()

pilgrimage :: CardDef Tactic
pilgrimage = tacticCard "core-117" "Pilgrimage" do
  cost 4
  loyalty 0
  body "Lower the cost to play this card by 1 for each development in your quest zone. Action: Return one target unit to its owner's hand."
  selfCostAdjust \g pk ->
    let me = playerOf pk g
        Developments n = me.capital.quest.developments
     in negate n
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \k -> returnUnitToHand k

burnItDown :: CardDef Tactic
burnItDown = tacticCard "core-118" "Burn It Down" do
  cost 2
  loyalty 0
  body "Action: Destroy one target support card with printed cost X or lower. X is the number of developments in your battlefield."
  whenResolved \self -> do
    g <- getGame
    let pk = self.controller
        me = playerOf pk g
        Developments x = me.capital.battlefield.developments
        eligible s = case s.cardDef.cost of
          Fixed c -> c <= x
          Variable -> False
        candidates = [s | s <- g.supports, eligible s, s.attachedTo == Nothing]
    case candidates of
      [] -> pure ()
      (s : _) -> destroySupport s.key

infiltrate :: CardDef Quest
infiltrate = questCard "core-120" "Infiltrate!" do
  cost 2
  loyalty 0
  body "Quest. Forced: At the beginning of your turn, discard the top X cards from each opponent's deck. X is the number of resource tokens on this quest. Quest. Forced: At the beginning of your turn, place 1 resource token on this quest if a unit is questing here."
  forced accrueTokenWhileQuesting
  onMyTurnBegin \_owner self -> do
    g <- getGame
    case findQuest self.key g of
      Just q | q.tokens > 0 -> millFromDeck self.controller.next q.tokens
      _ -> pure ()

prepareForWar :: CardDef Quest
prepareForWar = questCard "core-121" "Prepare for War!" do
  cost 0
  loyalty 0
  body "Quest. Action: Sacrifice the unit on this quest to shuffle X target cards from your discard pile into your deck, where X is the number of resource tokens on this quest. Quest. Forced: At the beginning of your turn, place 1 resource token on this quest if a unit is questing here."
  forced accrueTokenWhileQuesting
  action "March to war" 0 \usage ->
    withQuest usage.self.key \q ->
      for_ q.questingUnit \quester -> do
        destroyUnit quester
        recycleDiscard usage.user q.tokens

-- Alliance supports list BOTH races they support so the engine's
-- 'raceSymbolCount' (which reads 'cardDef.races') credits the
-- controller with one symbol of each. That's how the printed
-- "provides both X and Y loyalty symbols" rider takes effect with no
-- additional engine work.

allianceDwarfEmpire :: CardDef Support
allianceDwarfEmpire = supportCard "core-122" "Alliance - Dwarf and Empire" do
  race Dwarf
  race Empire
  cost 2
  loyalty 0
  power 1
  trait Banner
  orderOnly
  body "Order only. (This Alliance provides both a Dwarf and an Empire loyalty symbol.)"

allianceEmpireHighElf :: CardDef Support
allianceEmpireHighElf = supportCard "core-123" "Alliance - Empire and High Elf" do
  race Empire
  race HighElf
  cost 2
  loyalty 0
  power 1
  trait Banner
  orderOnly
  body "Order only. (This Alliance provides both an Empire and a High Elf loyalty symbol.)"

allianceDwarfHighElf :: CardDef Support
allianceDwarfHighElf = supportCard "core-124" "Alliance - Dwarf and High Elf" do
  race Dwarf
  race HighElf
  cost 2
  loyalty 0
  power 1
  trait Banner
  orderOnly
  body "Order only. (This Alliance provides both a Dwarf and a High Elf loyalty symbol.)"

allianceChaosOrc :: CardDef Support
allianceChaosOrc = supportCard "core-125" "Alliance - Chaos and Orc" do
  race Chaos
  race Orc
  cost 2
  loyalty 0
  power 1
  trait Banner
  destructionOnly
  body "Destruction only. (This Alliance provides both a Chaos and an Orc loyalty symbol.)"

allianceChaosDarkElf :: CardDef Support
allianceChaosDarkElf = supportCard "core-126" "Alliance - Chaos and Dark Elf" do
  race Chaos
  race DarkElf
  cost 2
  loyalty 0
  power 1
  trait Banner
  destructionOnly
  body "Destruction only. (This alliance provides both a Chaos and a Dark Elf loyalty symbol.)"

allianceOrcDarkElf :: CardDef Support
allianceOrcDarkElf = supportCard "core-127" "Alliance - Orc and Dark Elf" do
  race Orc
  race DarkElf
  cost 2
  loyalty 0
  power 1
  trait Banner
  destructionOnly
  body "Destruction only. (This Alliance provides both an Orc and a Dark Elf loyalty symbol.)"

-- The Corruption cycle ------------------------------------------------

greyseerThanquol :: CardDef Unit
greyseerThanquol = unitCard "the-skavenblight-threat-015" "Greyseer Thanquol" do
  hero
  trait Skaven
  destructionOnly
  cost 3
  loyalty 0
  power 1
  hitPoints 1
  body
    "Limit one Hero per zone. Destruction only. This unit may attack from any zone. While \
    \attacking, this unit gains {power} for each Skaven unit you control."
  attacksFromZones [KingdomZone, QuestZone, BattlefieldZone]
  effects \self owner -> do
    g <- getGame
    let skaven =
          length
            [ u
            | u <- g.units
            , u.controller == owner.key
            , hasTrait Skaven u
            ]
    when (self.attacking && skaven > 0) $ gainPower self skaven

clanRats :: CardDef Unit
clanRats = unitCard "the-skavenblight-threat-016" "Clan Rats" do
  trait Skaven
  destructionOnly
  cost 2
  loyalty 0
  power 1
  hitPoints 1
  body
    "Destruction only. Action: Corrupt this unit to have one other target Skaven unit \
    \gain {power} until the end of the turn."
  actionWith "You go first" 0 [CorruptSelf] \usage ->
    withTarget usage.user
      (unitWhere \u -> hasTrait Skaven u && u.key /= usage.self.key)
      \k -> until EndOfTurn $ buffPower k 1

warpLightningCannon :: CardDef Support
warpLightningCannon = supportCard "the-skavenblight-threat-017" "Warp Lightning Cannon" do
  traits [Attachment, Weapon, Skaven]
  destructionOnly
  cost 2
  loyalty 0
  body
    "Destruction only. Attach to a target unit. Corrupt that unit. Attached unit gains \
    \{power}{power}{power} while attacking or defending."
  onEnterPlay \_owner self -> for_ self.attachedTo corrupt
  attachedTo \_self unit ->
    when (unit.attacking || unit.defending) $ gainPower unit 3

mariusTheRighteous :: CardDef Unit
mariusTheRighteous = unitCard "the-skavenblight-threat-018" "Marius the Righteous" do
  hero
  trait WitchHunter
  orderOnly
  cost 4
  loyalty 0
  power 2
  hitPoints 4
  body
    "Limit one Hero per zone. Order only. Quest. Action: Spend 1 resource to deal 1 damage \
    \to one target corrupted unit in the battlefield."
  quest $ action "Purge the unclean" 1 \usage ->
    withTarget usage.user
      (unitWhere \u -> u.corrupted && u.zone == BattlefieldZone)
      \k -> dealDamage k 1

abandonedMine :: CardDef Support
abandonedMine = supportCard "the-skavenblight-threat-019" "Abandoned Mine" do
  cost 4
  loyalty 0
  power 2
  trait Building
  body "Kingdom. Action: At the beginning of your turn, you may return one of your developments to its owner's hand."
  kingdom $ onMyTurnBegin \_owner self -> do
    g <- getGame
    let pk = self.controller
        me = playerOf pk g
        devZones =
          [ zk
          | zk <- [KingdomZone, QuestZone, BattlefieldZone]
          , not (null (Map.findWithDefault [] zk me.developmentCards))
          ]
    unless (null devZones) $
      may pk "Abandoned Mine: return one of your developments to hand?" $
        withTarget pk
          (CapitalMatching \me' (owner, zk) -> owner == me' && zk `elem` devZones)
          \(owner, zk) -> push (ReturnDevelopmentToHand owner zk)

forge :: CardDef Support
forge = supportCard "the-skavenblight-threat-020" "Forge" do
  cost 2
  loyalty 0
  power 1
  trait Building
  body
    "Kingdom. While you control two or more developments in this zone, lower the cost for \
    \you to play Attachment cards by 1."
  globalCostAdjust \g s pk filt ->
    let me = playerOf s.controller g
        Developments n = me.capital.kingdom.developments
     in if pk == s.controller
          && s.zone == KingdomZone
          && n >= 2
          && Attachment `elem` filt.cfTraits
          then -1
          else 0

poisonWindGlobadiers :: CardDef Unit
poisonWindGlobadiers = unitCard "path-of-the-zealot-035" "Poison Wind Globadiers" do
  trait Skaven
  destructionOnly
  cost 3
  loyalty 0
  power 1
  hitPoints 2
  body
    "Destruction only. Battlefield. Action: Corrupt this unit to deal 1 damage to one \
    \target attacking or defending unit."
  battlefield $ actionWith "Hurl the globes" 0 [CorruptSelf] \usage ->
    withTarget usage.user (attackingUnit `Or` defendingUnit) \case
      TargetUnitOption k -> dealDamage k 1
      _ -> pure ()

chitteringHorde :: CardDef Tactic
chitteringHorde = tacticCard "path-of-the-zealot-036" "Chittering Horde" do
  trait Skaven
  destructionOnly
  cost 1
  loyalty 0
  body
    "Destruction only. Action: Search the top five cards of your deck. You may reveal any \
    \number of Skaven cards found and put them into your hand. Shuffle the rest of the \
    \searched cards into your deck."
  playableWhen \g pk -> hasDeckSize 1 g pk
  whenResolved \self -> do
    let pk = self.controller
    searchTopOfDeck pk 5 \result -> do
      let skaven = [c | c <- result.cards, Skaven `elem` someCardTraits c.def]
      chooseFromCards pk 0 (length skaven) skaven
        "Reveal any number of Skaven cards to take into hand." \chosen -> do
          unless (null chosen) $
            push (TakeCardsFromDeckToHand pk (map (.key) chosen))
          shuffleDeck pk

zealotHunter :: CardDef Unit
zealotHunter = unitCard "path-of-the-zealot-037" "Zealot Hunter" do
  trait WitchHunter
  orderOnly
  cost 3
  loyalty 0
  power 1
  hitPoints 2
  body
    "Order only. Forced: After this unit enters play, destroy a unit that does not share \
    \the racial affiliation of its controller's capital."
  onEnterPlay \_owner self -> do
    g <- getGame
    let strangers =
          [ u.key
          | u <- g.units
          , (playerOf u.controller g).race `notElem` u.cardDef.races
          ]
    forcePickUnit self.controller strangers
      "Zealot Hunter: destroy a unit that does not match its controller's capital."
      destroyUnit

veteranSellswords :: CardDef Unit
veteranSellswords = unitCard "path-of-the-zealot-038" "Veteran Sellswords" do
  cost 0
  loyalty 0
  power 1
  hitPoints 1
  trait Warrior
  battlefieldOnly
  body
    "Battlefield only. Forced: At the end of your turn, target opponent gains control of \
    \this unit and moves it to his corresponding zone."
  onMyTurnEnd \_owner self ->
    push (TakeControlOfUnit self.controller.next self.key)

surpriseAssault :: CardDef Tactic
surpriseAssault = tacticCard "path-of-the-zealot-039" "Surprise Assault" do
  cost 2
  loyalty 0
  body
    "Action: Deal X indirect damage to one target player. X is the number of developments \
    \in your battlefield. (Players assign their own indirect damage.)"
  whenResolved \self -> do
    g <- getGame
    let me = playerOf self.controller g
        Developments x = me.capital.battlefield.developments
    when (x > 0) $ indirectDamage self.controller.next x

animosity :: CardDef Tactic
animosity = tacticCard "path-of-the-zealot-040" "Animosity" do
  cost 2
  loyalty 0
  body "Play after a zone is attacked. Action: Target unit must defend this turn, if able."
  playableWhen \g _pk -> isJust g.combat
  whenResolved \self ->
    withTarget self.controller
      (UnitMatching \_ g u -> case g.combat of
        Just cs -> u.controller == cs.defendingPlayer && u.zone == cs.targetZone
        Nothing -> False)
      \k -> until EndOfTurn $ mustDefend k

ratOgres :: CardDef Unit
ratOgres = unitCard "tooth-and-claw-055" "Rat Ogres" do
  trait Skaven
  destructionOnly
  cost 4
  loyalty 0
  power 2
  hitPoints 3
  body "Destruction only. Action: At the beginning of your turn, uncorrupt all Skaven units."
  onMyTurnBegin \_owner self -> do
    g <- getGame
    let corrupted = [u.key | u <- g.units, hasTrait Skaven u, u.corrupted]
    unless (null corrupted) $
      may self.controller "Rat Ogres: uncorrupt all Skaven units?" $
        for_ corrupted \k -> push (CleanseUnit k)

gutterRunners :: CardDef Unit
gutterRunners = unitCard "tooth-and-claw-056" "Gutter Runners" do
  trait Skaven
  destructionOnly
  battlefieldOnly
  scout
  cost 2
  loyalty 0
  power 1
  hitPoints 2
  body "Destruction only. Battlefield only. Scout. This unit enters play corrupted."
  onEnterPlay \_owner self -> corrupt self.key

clanMouldersElite :: CardDef Unit
clanMouldersElite = unitCard "tooth-and-claw-057" "Clan Moulder's Elite" do
  traits [Warrior, Skaven]
  destructionOnly
  battlefieldOnly
  cost 2
  loyalty 0
  power 2
  hitPoints 5
  body "Destruction only. Battlefield only. This unit cannot defend."
  neverDefends

errantWolf :: CardDef Unit
errantWolf = unitCard "tooth-and-claw-058" "Errant Wolf" do
  trait Knight
  orderOnly
  questOnly
  limited
  cost 2
  loyalty 0
  power 2
  hitPoints 1
  body "Order only. Quest zone only. Limited (you cannot play more than one Limited card per turn)."

reapWhatsSown :: CardDef Tactic
reapWhatsSown = tacticCard "tooth-and-claw-059" "Reap What's Sown" do
  costVariable
  loyalty 0
  body "Action: Each player with X or more total developments may discard his hand and draw X cards."
  whenResolved \self -> do
    let x = self.xValue
    g <- getGame
    when (x > 0) $
      eachPlayer \pk -> do
        let me = playerOf pk g
            total = sum [n | z <- me.capital.zones, let Developments n = z.developments]
        when (total >= x) $
          may pk ("Reap What's Sown: discard your hand and draw " <> tshow x <> " cards?") do
            discardHand pk
            drawCards pk x

scoutCamp :: CardDef Support
scoutCamp = supportCard "tooth-and-claw-060" "Scout Camp" do
  cost 2
  loyalty 0
  power 1
  trait Building
  body "Kingdom. Whenever you search your deck, you may search an additional card."
  searchBonus \_g s pk ->
    if pk == s.controller && s.zone == KingdomZone then 1 else 0

deathmasterSniktch :: CardDef Unit
deathmasterSniktch = unitCard "the-deathmaster-s-dance-079" "Deathmaster Sniktch" do
  hero
  trait Skaven
  destructionOnly
  cost 4
  loyalty 0
  power 2
  hitPoints 2
  body
    "Limit one Hero per zone. Destruction only. Action: Corrupt this unit to destroy one \
    \target unit with fewer remaining hit points than the number of Skaven cards in play."
  actionWith "Slay-kill" 0 [CorruptSelf] \usage -> do
    g <- getGame
    let skavenCount =
          length [u | u <- g.units, hasTrait Skaven u]
            + length [s | s <- allInPlaySupports g, hasTrait Skaven s]
            + length [q | q <- g.quests, hasTrait Skaven q]
        remaining u = let Damage d = u.damage in u.effectiveMaxHP - d
    withTarget usage.user
      (unitWhere \u -> remaining u < skavenCount)
      destroyUnit

juvenileWyvern :: CardDef Unit
juvenileWyvern = unitCard "the-deathmaster-s-dance-080" "Juvenile Wyvern" do
  trait Creature
  destructionOnly
  cost 4
  loyalty 0
  power 1
  hitPoints 2
  body "Destruction only. When Juvenile Wyvern defends, it deals its combat damage to all attacking units."
  defenderHitsAllAttackers

ancientWaystone :: CardDef Support
ancientWaystone = supportCard "the-warpstone-chronicles-099" "Ancient Waystone" do
  cost 2
  loyalty 0
  power 1
  body
    "Action: Spend 1 resource to deal 1 damage to any target unit in this corresponding \
    \zone. Use this ability only once per turn, and only if any player has played a Spell \
    \card this turn."
  action "Discharge" 1 \usage -> do
    g <- getGame
    whenJust (findSupport usage.self.key g) \s -> do
      let used =
            any (\m -> m.details == ActionUsedThisTurn)
              (Map.findWithDefault [] (UnitRef s.key) g.modifiers)
          h = Map.findWithDefault mempty ThisTurn g.history
          spellPlayed =
            any (any (\cf -> Spell `elem` cf.cfTraits)) (Map.elems h.playedBy)
      when (not used && spellPlayed) do
        until EndOfTurn (PendingBuff s.key ActionUsedThisTurn)
        withTarget usage.user (unitWhere \u -> u.zone == s.zone) \k ->
          dealDamage k 1

fellblade :: CardDef Support
fellblade = supportCard "the-warpstone-chronicles-100" "Fellblade" do
  unique
  traits [Attachment, Relic, Skaven]
  cost 2
  loyalty 0
  body
    "Attach to a target Skaven unit. Corrupt that unit. Whenever a unit is corrupted, \
    \place 1 resource token on this card. Attached unit deals +X damage in combat, where \
    \X is the number of resource tokens on this card."
  onEnterPlay \_owner self -> for_ self.attachedTo corrupt
  supportCombat \_g s u ->
    if s.attachedTo == Just u.key then s.tokens else 0
  onReceive $ Receive \msg _owner self -> case msg of
    CorruptUnit uk -> do
      g <- getGame
      case findUnit uk g of
        Just u
          | not u.corrupted
          , all
              (\m -> m.details /= CannotBeCorrupted)
              (Map.findWithDefault [] (UnitRef uk) g.modifiers) ->
              push (AdjustSupportTokens self.key 1)
        _ -> pure ()
    _ -> pure ()

greyseersLair :: CardDef Support
greyseersLair = supportCard "arcane-fire-119" "Greyseer's Lair" do
  traits [Warpstone, Skaven]
  destructionOnly
  cost 4
  loyalty 0
  power 2
  body "Destruction only. Kingdom. Lower the cost of the first Skaven card you play each turn by 1."
  globalCostAdjust \g s pk filt ->
    let h = Map.findWithDefault mempty ThisTurn g.history
        skavenPlayed =
          length
            [ cf
            | cf <- Map.findWithDefault [] pk h.playedBy
            , Skaven `elem` cf.cfTraits
            ]
     in if pk == s.controller
          && s.zone == KingdomZone
          && Skaven `elem` filt.cfTraits
          && skavenPlayed == 0
          then -1
          else 0

plagueMonk :: CardDef Unit
plagueMonk = unitCard "arcane-fire-120" "Plague Monk" do
  trait Skaven
  destructionOnly
  cost 2
  loyalty 0
  power 1
  hitPoints 2
  body
    "Destruction only. Kingdom. Action: Whenever you play a Spell, look at the top two \
    \cards of any player's deck. Discard any number of those cards and place the rest \
    \back on top of the deck in any order."
  kingdom $ onReceive $ Receive \msg _owner self -> case msg of
    TacticResolved pk _code _target _x
      | pk == self.controller -> do
          g <- getGame
          let h = Map.findWithDefault mempty ThisTurn g.history
              lastPlay = listToMaybe (Map.findWithDefault [] pk h.playedBy)
              wasSpell = maybe False (\cf -> Spell `elem` cf.cfTraits) lastPlay
          when wasSpell $
            -- Approximation: the printed "place the rest back on top
            -- of the deck in any order" reorder choice isn't offered;
            -- undiscarded cards keep their order.
            may pk "Plague Monk: look at the top two cards of a deck?" do
              mine <- askYesNo pk "Look at YOUR deck? (No looks at the opponent's.)"
              let target = if mine then pk else pk.next
              tp <- playerOf target <$> getGame
              let top2 = take 2 tp.deck
              unless (null top2) $
                chooseFromCards pk 0 (length top2) top2
                  "Discard any of these; the rest stay on top of the deck." \chosen ->
                    unless (null chosen) $
                      push (DiscardCardsFromDeck target (map (.key) chosen))
    _ -> pure ()

-- | Helper: developments count in the named zone of a capital.
zoneDevs :: Capital -> ZoneKind -> Developments
zoneDevs cap = \case
  KingdomZone -> cap.kingdom.developments
  QuestZone -> cap.quest.developments
  BattlefieldZone -> cap.battlefield.developments

-- Days of Blood --------------------------------------------------------

bordertown :: CardDef Support
bordertown = supportCard "days-of-blood-019" "Bordertown" do
  cost 2
  loyalty 0
  power 2
  trait Building
  body "Forced: When this zone takes combat damage, sacrifice this card."
  onReceive $ Receive \msg _owner self -> case msg of
    DealDamageToZone pk zone n
      | pk == self.controller, zone == self.zone, n > 0 -> do
          g <- getGame
          when (isJust g.combat) $ destroySupport self.key
    _ -> pure ()

-- Bloodquest: Vessel of the Winds ---------------------------------------

magePriestOfItza :: CardDef Unit
magePriestOfItza = unitCard "vessel-of-the-winds-076" "Mage-Priest of Itza" do
  cost 3
  loyalty 0
  power 1
  hitPoints 3
  trait Priest
  orderOnly
  body
    "Order only. Action: When this unit enters play, shuffle the top 5 cards of your discard \
    \pile back into your deck."
  onEnterPlay \_owner self -> recycleDiscard self.controller 5

-- The Capital Cycle ----------------------------------------------------

willpower :: CardDef Tactic
willpower = tacticCard "the-inevitable-city-019" "Willpower" do
  cost 1
  loyalty 0
  body "Action: Target Hero unit gains {power} equal to its loyalty until the end of the turn."
  playableWhen $ hasTarget (unitWhere \u -> Hero `elem` u.cardDef.traits)
  whenResolved \self ->
    withTarget self.controller (unitWhere \u -> Hero `elem` u.cardDef.traits) \k -> do
      g <- getGame
      whenJust (findUnit k g) \u ->
        until EndOfTurn $ buffPower k u.cardDef.loyalty

-- The Morrslieb cycle ---------------------------------------------------

viciousClanrat :: CardDef Unit
viciousClanrat = unitCard "the-chaos-moon-040" "Vicious Clanrat" do
  cost 4
  power 1
  hitPoints 3
  trait Skaven
  destructionOnly
  body "While attacking, this unit gains {power} for each corrupted Skaven unit you control."
  combatPower \g self ->
    if self.key `elem` maybe [] (.attackers) g.combat
      then length [u | u <- g.units, u.controller == self.controller, u.corrupted, Skaven `elem` u.cardDef.traits]
      else 0

-- The Enemy cycle -------------------------------------------------------

mountainBrigands :: CardDef Unit
mountainBrigands = unitCard "the-fourth-waystone-100" "Mountain Brigands" do
  cost 2
  power 1
  hitPoints 2
  trait Thief
  body "Action: When this unit enters play, target opponent must give you 1 resource."
  onEnterPlay \_owner self -> do
    let opp = self.controller.next
    payResources opp 1
    gainResources self.controller 1

entropy :: CardDef Tactic
entropy = tacticCard "the-burning-of-derricksburg-019" "Entropy" do
  cost 1
  destructionOnly
  body "Destruction only. Action: Discard the top two cards of target player's deck."
  whenResolved \self -> millFromDeck self.controller.next 2

muck :: CardDef Tactic
muck = tacticCard "the-silent-forge-060" "Muck!" do
  cost 2
  body "Action: Discard your hand. Then, draw as many cards as you just discarded."
  whenResolved \self -> do
    let pk = self.controller
    n <- (\g -> length (playerOf pk g).hand) <$> getGame
    discardHand pk
    drawCards pk n

grailKnight :: CardDef Unit
grailKnight = unitCard "bleeding-sun-118" "Grail Knight" do
  cost 4
  power 2
  hitPoints 2
  traits [Bretonnian, Knight]
  orderOnly
  body "Order only. Lower the cost to play this unit by 2 while you have a quest in play."
  selfCostAdjust \g pk -> if any (\q -> q.controller == pk) g.quests then -2 else 0

battlePilgrims :: CardDef Unit
battlePilgrims = unitCard "the-silent-forge-058" "Battle Pilgrims" do
  cost 2
  power 1
  hitPoints 2
  traits [Bretonnian, Warrior]
  orderOnly
  body "Order only. Action: Sacrifice this unit to destroy target corrupted unit."
  action "Sacrifice to destroy a corrupted unit" 0 \u -> do
    destroyUnit u.self.key
    withTarget u.user (unitWhere (.corrupted)) destroyUnit

bottomlessMine :: CardDef Support
bottomlessMine = supportCard "the-fall-of-karak-grimaz-038" "Bottomless Mine" do
  cost 2
  power 1
  trait Building
  limited
  body "Limited. Kingdom. This card gains {power}{power} if there are at least two units in this zone."
  zonePowerAura \g s z ->
    if z == s.zone
      && length [u | u <- g.units, u.controller == s.controller, u.zone == s.zone] >= 2
      then 2
      else 0

darkAbyss :: CardDef Support
darkAbyss = supportCard "the-fall-of-karak-grimaz-039" "Dark Abyss" do
  cost 2
  power 1
  trait Location
  body "Action: At the beginning of your turn, discard the top card of target player's deck."
  onMyTurnBegin \_owner self -> millFromDeck self.controller.next 1

ancientAlliance :: CardDef Support
ancientAlliance = supportCard "redemption-of-a-mage-078" "Ancient Alliance" do
  race Dwarf
  race Empire
  race HighElf
  cost 2
  loyalty 0
  power 1
  trait Banner
  orderOnly
  limited
  body "Order only. Limited. (This Alliance provides a Dwarf, an Empire, and a High Elf Loyalty symbol.)"

evilAlliance :: CardDef Support
evilAlliance = supportCard "redemption-of-a-mage-079" "Evil Alliance" do
  race Chaos
  race DarkElf
  race Orc
  cost 2
  loyalty 0
  power 1
  trait Banner
  destructionOnly
  limited
  body "Destruction only. Limited. (This Alliance provides a Chaos, a Dark Elf, and an Orc Loyalty symbol.)"

-- Assault on Ulthuan ---------------------------------------------------

ancientMap :: CardDef Tactic
ancientMap = tacticCard "assault-on-ulthuan-056" "Ancient Map" do
  cost 1
  loyalty 1
  body "Action: Search your deck for a quest card, reveal it to each player, and add it to your hand. Then, shuffle your deck."
  whenResolved \self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    searchTopOfDeck pk (length me.deck) \result -> do
      let quests = [c | c <- result.cards, isJust (asQuest c.def)]
      chooseFromCards pk 0 1 quests "Choose a quest card to add to your hand." \chosen ->
        for_ chosen \c -> push (TakeCardsFromDeckToHand pk [c.key])
      shuffleDeck pk

innovation :: CardDef Tactic
innovation = tacticCard "assault-on-ulthuan-057" "Innovation" do
  cost 0
  loyalty 1
  body "Action: Gain 1 resource for each development in your kingdom."
  whenResolved \self -> do
    me <- playerOf self.controller <$> getGame
    let Developments d = me.capital.kingdom.developments
    when (d > 0) $ gainResources self.controller d

treasureVaults :: CardDef Support
treasureVaults = supportCard "assault-on-ulthuan-055" "Treasure Vaults" do
  cost 3
  loyalty 0
  power 1
  trait Building
  kingdomOnly
  body "Kingdom. While you have 3 or more developments in this zone, each Building support card in this zone gains {power}."
  -- Each Building support in the zone gains +1; expressed as a single
  -- zone-power contribution equal to the count of such supports while
  -- the 3-development threshold holds.
  zonePowerAura \g s zone ->
    if s.zone /= zone
      then 0
      else
        let me = playerOf s.controller g
            Developments d = case zone of
              KingdomZone -> me.capital.kingdom.developments
              QuestZone -> me.capital.quest.developments
              BattlefieldZone -> me.capital.battlefield.developments
         in if d >= 3
              then
                length
                  [ b
                  | b <- g.supports
                  , b.controller == s.controller
                  , b.zone == zone
                  , Building `elem` b.cardDef.traits
                  ]
              else 0

-- March of the Damned --------------------------------------------------
--
-- The Lizardmen (Order) and Undead (Destruction) minor factions, plus a
-- lone Skaven raider. These are Neutral cards gated by the printed
-- "Order only." / "Destruction only." deck-construction keywords.

skinksOfSotek :: CardDef Unit
skinksOfSotek = unitCard "march-of-the-damned-031" "Skinks of Sotek" do
  cost 2
  power 1
  hitPoints 1
  trait Lizardmen
  orderOnly
  body "Order only. Action: When this unit enters play, deal 1 uncancellable damage to target unit."
  onEnterPlay \_owner self ->
    withTarget self.controller AnyUnit \k -> dealUncancellableDamage k 1

spawnOfItzl :: CardDef Unit
spawnOfItzl = unitCard "march-of-the-damned-033" "Spawn of Itzl" do
  cost 3
  power 1
  hitPoints 3
  trait Lizardmen
  orderOnly
  body "Order only. Action: When this unit attacks, destroy target damaged unit in the defending zone."
  onMyAttackDeclared \_owner self zone _attackers ->
    withTarget self.controller
      (UnitMatching \me _g u -> u.controller /= me && u.zone == zone && isDamaged u)
      destroyUnit

saurusWarriors :: CardDef Unit
saurusWarriors = unitCard "march-of-the-damned-034" "Saurus Warriors" do
  cost 4
  power 2
  hitPoints 3
  trait Lizardmen
  orderOnly
  body "Order only. This unit gains {power} when opposed in combat."
  combatPower \g u -> if isOpposed g u then 1 else 0

templeGuard :: CardDef Unit
templeGuard = unitCard "march-of-the-damned-035" "Temple Guard" do
  cost 3
  power 1
  hitPoints 3
  trait Lizardmen
  orderOnly
  body
    "Order only. Action: Spend 1 resource to redirect the next 2 damage dealt to target unit \
    \you control to another target unit you control."
  action "Interpose" 1 \usage ->
    withTarget usage.user ownUnit \k1 ->
      withTarget usage.user (UnitMatching \me _g u -> u.controller == me && u.key /= k1) \k2 ->
        until EndOfTurn $ redirectNextDamage k1 2 k2

loqtza :: CardDef Unit
loqtza = unitCard "march-of-the-damned-037" "Loqtza" do
  hero
  cost 6
  power 3
  hitPoints 5
  traits [Lizardmen, Mage]
  orderOnly
  body
    "Order only. Limit one Hero per zone. Action: Spend 2 resources to deal X damage to target \
    \unit. X is the number of Lizardmen units in this zone."
  action "Star-fire" 2 \usage ->
    withTarget usage.user AnyUnit \k -> do
      g <- getGame
      let x =
            length
              [ u
              | u <- g.units
              , u.controller == usage.self.controller
              , u.zone == usage.self.zone
              , Lizardmen `elem` u.cardDef.traits
              ]
      when (x > 0) $ dealDamage k x

bloodShrineOfSotek :: CardDef Support
bloodShrineOfSotek = supportCard "march-of-the-damned-039" "Blood Shrine of Sotek" do
  cost 2
  power 1
  traits [Lizardmen, Building]
  orderOnly
  body "Order only. Your Lizardmen units in this zone gain +1 hit points."
  supportHPAura \_g s u ->
    if u.controller == s.controller && u.zone == s.zone && Lizardmen `elem` u.cardDef.traits
      then 1
      else 0

bornPredators :: CardDef Tactic
bornPredators = tacticCard "march-of-the-damned-041" "Born Predators" do
  cost 2
  traits [Lizardmen]
  orderOnly
  body "Order only. Action: Deal X damage to target unit. X is the number of Lizardmen cards you control."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self ->
    withTarget self.controller AnyUnit \k -> do
      g <- getGame
      let x = lizardmenControlled g self.controller
      when (x > 0) $ dealDamage k x
  where
    lizardmenControlled g pk =
      length [u | u <- g.units, u.controller == pk, Lizardmen `elem` u.cardDef.traits]
        + length [s | s <- allInPlaySupports g, s.controller == pk, Lizardmen `elem` s.cardDef.traits]

cryptGhouls :: CardDef Unit
cryptGhouls = unitCard "march-of-the-damned-043" "Crypt Ghouls" do
  cost 2
  power 1
  hitPoints 1
  traits [Undead, Warrior]
  destructionOnly
  body "Destruction only. Action: When this unit leaves play, gain 2 resources."
  onSelfLeavesPlay \_owner self -> gainResources self.controller 2

enragedVarghulf :: CardDef Unit
enragedVarghulf = unitCard "march-of-the-damned-047" "Enraged Varghulf" do
  cost 4
  power 2
  hitPoints 3
  traits [Undead, Creature]
  destructionOnly
  body
    "Destruction only. Action: When this unit attacks, it gains {power} equal to the total \
    \{power} of target unit in the defending zone."
  onMyAttackDeclared \_owner self zone _attackers ->
    withTarget self.controller (UnitMatching \_me _g u -> u.zone == zone) \k ->
      withUnit k \u -> when (u.effectivePower > 0) $
        until EndOfTurn $ buffPower self.key u.effectivePower

corpseCart :: CardDef Support
corpseCart = supportCard "march-of-the-damned-051" "Corpse Cart" do
  cost 1
  power 0
  traits [Undead, WarMachine]
  destructionOnly
  body "Destruction only. Battlefield. Action: Spend 1 resource to discard the top 2 cards of your deck."
  battlefield $ action "Grind the dead" 1 \usage -> millFromDeck usage.user 2

theScreamingBanner :: CardDef Support
theScreamingBanner = supportCard "march-of-the-damned-052" "The Screaming Banner" do
  cost 2
  power 0
  traits [Attachment, Undead]
  destructionOnly
  body
    "Destruction only. Attach to a target Undead unit you control. Action: When attached unit \
    \attacks, target unit loses {power}{power} until the end of the turn."
  onAttachedHostAttack \_owner self _host ->
    withTarget self.controller AnyUnit \k -> until EndOfTurn $ buffPower k (-2)

raiseDead :: CardDef Tactic
raiseDead = tacticCard "march-of-the-damned-053" "Raise Dead" do
  cost 4
  traits [Undead, Spell]
  destructionOnly
  body
    "Destruction only. Play during your turn. Action: Choose a target unit in your discard pile \
    \and put it into play (you choose which zone the unit enters)."
  playableWhen \g pk ->
    g.currentPlayer == pk && not (null (unitsIn (playerOf pk g).discard))
  whenResolved \self -> do
    let pk = self.controller
    me <- playerOf pk <$> getGame
    chooseFromCards pk 1 1 (unitsIn me.discard) "Choose a unit to raise into play." \chosen ->
      for_ chosen \c ->
        withTarget pk MyAnyZone \zk -> putUnitIntoPlay pk FromDiscard c.key zk
  where
    unitsIn cards = [c | c <- cards, isJust (asUnit c.def)]

beguile :: CardDef Tactic
beguile = tacticCard "march-of-the-damned-054" "Beguile" do
  cost 2
  traits [Undead, Spell]
  destructionOnly
  body
    "Destruction only. Action: Target unit deals damage equal to its power to another target \
    \unit in its zone."
  playableWhen $ hasTarget AnyUnit
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyUnit \k1 ->
      withUnit k1 \u1 ->
        withTarget pk (UnitMatching \_me _g u -> u.zone == u1.zone && u.key /= k1) \k2 ->
          when (u1.effectivePower > 0) $ dealDamage k2 u1.effectivePower

jezzailTeam :: CardDef Unit
jezzailTeam = unitCard "march-of-the-damned-055" "Jezzail Team" do
  cost 3
  power 1
  hitPoints 2
  trait Skaven
  destructionOnly
  body "Destruction only. Action: Corrupt this unit to deal 1 damage to target unit in any battlefield."
  actionWith "Snipe" 0 [CorruptSelf] \usage ->
    withTarget usage.user (UnitMatching \_me _g u -> u.zone == BattlefieldZone) \k ->
      dealDamage k 1

-- Legends (deluxe expansion) -------------------------------------------

terradonRider :: CardDef Unit
terradonRider = unitCard "legends-043" "Terradon Rider" do
  cost 4
  loyalty 0
  power 1
  hitPoints 3
  trait Lizardmen
  orderOnly
  body
    "Order only. This unit gains {power} equal to the total number of other \
    \Lizardmen units in this zone."
  selfPower \g self ->
    length
      [ u
      | u <- g.units
      , u.key /= self.key
      , u.zone == self.zone
      , hasTrait Lizardmen u
      ]

stormvermin :: CardDef Unit
stormvermin = unitCard "legends-046" "Stormvermin" do
  cost 4
  loyalty 0
  power 2
  hitPoints 4
  traits [Skaven, Warrior]
  destructionOnly
  body
    "Destruction only. Action: Spend 1 resource to redirect the next 2 damage \
    \dealt to target unit you control to this unit."
  action "Bodyguard" 1 \usage ->
    withTarget usage.user ownUnit \k ->
      until Permanent $ redirectNextDamage k 2 usage.self.key

screamingBell :: CardDef Support
screamingBell = supportCard "legends-047" "Screaming Bell" do
  cost 3
  loyalty 0
  power 1
  traits [Skaven, WarMachine]
  destructionOnly
  body "Destruction only. Each attacking Skaven unit you control gains {power}."
  supportAura \g s u ->
    if u.controller == s.controller && hasTrait Skaven u && unitIsAttacking g u
      then 1
      else 0

shadowSentinel :: CardDef Unit
shadowSentinel = unitCard "legends-049" "Shadow Sentinel" do
  cost 2
  loyalty 0
  power 1
  hitPoints 2
  traits [WoodElf, Warrior]
  orderOnly
  body
    "Order only. Action: When one or more units an opponent controls leaves \
    \play, put the top card of your deck into this zone facedown as a \
    \development."
  onOpponentUnitLeavePlay \_owner self _uk _zone _code ->
    addDevelopment self.controller self.zone

wildRider :: CardDef Unit
wildRider = unitCard "legends-050" "Wild Rider" do
  cost 3
  loyalty 0
  power 1
  hitPoints 3
  traits [WoodElf, Warrior]
  orderOnly
  body
    "Order only. If you have five or more developments in this zone, this unit \
    \gains {power}{power}{power}{power}."
  selfPower \g self ->
    if devsInZone g self >= 5 then 4 else 0

bloodDragonKnight :: CardDef Unit
bloodDragonKnight = unitCard "legends-053" "Blood Dragon Knight" do
  cost 4
  loyalty 0
  power 2
  hitPoints 4
  traits [Undead, Vampire]
  destructionOnly
  body
    "Destruction only. Action: When this unit attacks, deal damage equal to \
    \its power to target unit in the defending zone."
  onMyAttackDeclared \_owner self zone _attackers ->
    withTarget self.controller
      (unitWhere \u -> u.zone == zone && u.controller == self.controller.next)
      \k -> dealDamage k self.effectivePower

curseOfYears :: CardDef Support
curseOfYears = supportCard "legends-054" "Curse of Years" do
  cost 2
  loyalty 0
  traits [Undead, Attachment, Spell]
  destructionOnly
  body
    "Destruction only. Attach to a target unit. Attached unit gets -1 hit \
    \points for each resource token on this card. Action: At the beginning of \
    \its controller's turn, put a resource token on this card."
  supportHPAura \_g self target -> case self.attachedTo of
    Just hk | hk == target.key -> negate self.tokens
    _ -> 0
  onAttachedHostTurnBegin \_owner self _host ->
    adjustSupportTokens self.key 1

-- Hidden Kingdoms (deluxe expansion) -----------------------------------

chameleonStalker :: CardDef Unit
chameleonStalker = unitCard "hidden-kingdoms-003" "Chameleon Stalker" do
  cost 1
  loyalty 0
  power 1
  hitPoints 1
  trait Lizardmen
  body "Lizardmen only. This unit cannot be assigned damage during your battlefield phase."
  damageImmuneWhen \g self ->
    g.phase == Just BattlefieldPhase && g.currentPlayer == self.controller

greatTempleOfTlazcotl :: CardDef Support
greatTempleOfTlazcotl = supportCard "hidden-kingdoms-006" "Great Temple of Tlazcotl" do
  cost 3
  loyalty 0
  power 2
  traits [Lizardmen, Pyramid]
  body
    "Lizardmen only. Lizardmen units in a zone with at least 1 Pyramid card \
    \deal +1 damage in combat."
  supportCombat \g _s u ->
    if hasTrait Lizardmen u && zoneHasPyramid g u then 1 else 0
  where
    zoneHasPyramid g u =
      any
        (\s -> hasTrait Pyramid s && s.controller == u.controller && s.zone == u.zone)
        g.supports

ruinationOfCities :: CardDef Tactic
ruinationOfCities = tacticCard "hidden-kingdoms-009" "Ruination of Cities" do
  cost 6
  loyalty 0
  traits [Lizardmen, Spell]
  body "Lizardmen only. Action: Destroy all support cards and developments in target zone."
  whenResolved \self -> do
    let pk = self.controller
    withTarget pk AnyCapital \(owner, zk) -> do
      g <- getGame
      for_ [s.key | s <- g.supports, s.controller == owner, s.zone == zk, s.attachedTo == Nothing]
        destroySupport
      let me = playerOf owner g
          n = length (Map.findWithDefault [] zk me.developmentCards)
      replicateM_ n (destroyDevelopment owner zk)

giantRats :: CardDef Unit
giantRats = unitCard "hidden-kingdoms-024" "Giant Rats" do
  cost 2
  loyalty 0
  power 1
  hitPoints 1
  traits [Skaven, Creature]
  destructionOnly
  body
    "Destruction only. This unit gains {power} for each other copy of this \
    \unit you control."
  selfPower \g self ->
    length
      [ u
      | u <- g.units
      , u.controller == self.controller
      , u.key /= self.key
      , u.cardDef.code == self.cardDef.code
      ]
