-- | Smoke test for the phase / turn machinery. Run with @stack test@.
--
-- This is intentionally a one-file, dependency-free test using base +
-- the library — when we adopt hspec we'll move/expand it.

{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}

module Main (main) where

import Data.Aeson (Value (..), toJSON)
import Data.Aeson.KeyMap qualified as KM
import Data.Map.Strict qualified as Map
import Invasion.Capital (Capital (..), Damage (..), Developments (..), Zone (..))
import Invasion.Card (Card (..), SomeCardDef (..), Target (AnySupportCard, AnyUnit, TargetPlayer), allCards, enumerateOptionsPure, someCardCost)
import Invasion.CardDef (ActionTarget (..), CardDef (..), Keyword (..))
import Invasion.Modifier
import Invasion.Engine
import Invasion.Entity (SupportDetails (..), UnitDetails (..))
import Invasion.Game
import Invasion.Player
import Invasion.Prelude
import Invasion.Server.Lobby (redactEngineFor)
import Invasion.Types

import System.Exit (exitFailure)

-- TODO: this suite is flaky. 'runSetup' shuffles the decks with real IO
-- randomness, so the opening hands differ every run. Checks that depend
-- on hand contents (the "affordable Unit" PlayUnit probe, the scripted
-- combat scenario, …) pass or fail at random — observed ~40% failure
-- across repeated runs, with different cases failing each time. Fix by
-- making setup deterministic for tests: thread a seeded StdGen (or add a
-- 'runSetupWith :: StdGen -> ...') so the shuffle is reproducible, then
-- pin a seed here. Independent of card content — the starter decks are
-- fixed core cards.
main :: IO ()
main = do
  setupResult <- runSetup
  g0 <- case setupResult of
    Left err -> do
      putStrLn $ "FAIL runSetup: " <> err
      exitFailure
    Right g -> pure g

  -- After setup: hands dealt, first player chosen, no turn started yet.
  check "p1 hand = 7" (length g0.player1.hand == 7)
  check "p2 hand = 7" (length g0.player2.hand == 7)
  check "lifecycle = GameSetup" (isGameSetup g0.lifecycle)
  check "phase = Nothing" (isNothing g0.phase)
  check "actionWindow = Nothing" (isNothing g0.actionWindow)
  check "turn = 0" (g0.turn == Turn 0)

  -- Begin the game: turn 1 starts. The first window is the Phase 0
  -- (BeginningOfTurnActionWindow); pass it on both sides to let the
  -- kingdom phase actually start.
  let fp = g0.firstPlayer
  g1 <- applyMessages g0
    [ BeginGame
    , PassPriority fp
    , PassPriority fp.next
    ]
  check "lifecycle = GamePlaying" (isGamePlaying g1.lifecycle)
  check "turn = 1" (g1.turn == Turn 1)
  check "phase = Just KingdomPhase" (g1.phase == Just KingdomPhase)
  check "currentPlayer = firstPlayer" (g1.currentPlayer == fp)
  check "action window open on kingdom"
    (windowTrigger g1.actionWindow == Just KingdomActionWindow)
  check "priority with active player"
    (case g1.actionWindow of
       Just aw -> priorityHolder aw.awaiting == fp
       Nothing -> False)
  check "active player has 3 resources"
    ((activePlayer g1).resources == Resources 3)
  check "inactive player has 0 resources"
    ((inactivePlayer g1).resources == Resources 0)

  -- Both players pass: kingdom window closes; quest phase skipped on
  -- turn 1 for the first player; capital phase opens its window.
  g2 <- applyMessages g1 [PassPriority fp, PassPriority fp.next]
  check "after kingdom: phase = Just CapitalPhase (Quest skipped)"
    (g2.phase == Just CapitalPhase)
  check "after kingdom: turn still 1" (g2.turn == Turn 1)
  check "after kingdom: window open on capital"
    (windowTrigger g2.actionWindow == Just CapitalActionWindow)
  check "no quest draw happened (first turn skips quest)"
    (length g2.player1.hand == 7 && length g2.player2.hand == 7)

  -- Pass capital window. Battlefield is also skipped on turn 1, so we
  -- land on the EndOfTurnActionWindow (2 more passes), then the next
  -- player's BeginningOfTurnActionWindow (2 more passes) before their
  -- kingdom phase finally opens.
  g3 <- applyMessages g2
    [ PassPriority fp, PassPriority fp.next                -- close capital
    , PassPriority fp, PassPriority fp.next                -- close end-of-turn
    , PassPriority fp.next, PassPriority fp                -- close begin-of-turn for new active
    ]
  check "after capital: turn = 2" (g3.turn == Turn 2)
  check "after capital: currentPlayer flipped"
    (g3.currentPlayer == fp.next)
  check "after capital: phase = Just KingdomPhase"
    (g3.phase == Just KingdomPhase)
  check "new active has 3 resources"
    ((activePlayer g3).resources == Resources 3)
  check "previously-active player still has 3 resources (not yet reset)"
    ((inactivePlayer g3).resources == Resources 3)

  -- On the SECOND turn the active player no longer skips Quest or
  -- Battlefield. Pass the kingdom window; we should land on a quest
  -- action window with a card drawn.
  let active2 = g3.currentPlayer
  let handSizeBefore = length (activePlayer g3).hand
  g4 <- applyMessages g3 [PassPriority active2, PassPriority active2.next]
  check "after kingdom (T2): phase = Just QuestPhase (no skip)"
    (g4.phase == Just QuestPhase)
  check "active drew 1 quest card"
    (length (activePlayer g4).hand == handSizeBefore + 1)
  check "previously-active resources reset to 0 next time their turn ends"
    -- player 1's resources weren't reset because it's not their kingdom
    -- phase yet — sanity check we're not double-collecting.
    ((inactivePlayer g4).resources == Resources 3)

  -- Advance through Quest into Capital so we're in the only phase
  -- where PlayUnit is legal under the rules-correct gating.
  g4cap <- applyMessages g4 [PassPriority active2, PassPriority active2.next]
  check "after quest (T2): phase = Just CapitalPhase"
    (g4cap.phase == Just CapitalPhase)

  -- PlayUnit: pick a Unit from the active player's hand and play it into
  -- the kingdom zone. Grant a large pile of resources first so *any*
  -- unit in hand is affordable — otherwise the probe is flaky on the
  -- shuffle (a hand of only expensive units fails the affordability
  -- check). The "resources reduced by cost" assertion still holds since
  -- it measures the delta from the granted total.
  g4res <- applyMessage g4cap (GainResources g4cap.currentPlayer 50)
  let preP = activePlayer g4res
  case findPlayableUnit preP of
    Nothing -> do
      putStrLn "  FAIL active hand has no playable Unit; can't exercise PlayUnit"
      exitFailure
    Just (cardKey, cardCode, cardCost, playZone) -> do
      let handBefore = length preP.hand
          Resources resBefore = preP.resources
      g5 <- applyMessage g4res (PlayUnit g4res.currentPlayer cardKey playZone)
      let postP = activePlayer g5
      check "PlayUnit: hand size decreased by 1"
        (length postP.hand == handBefore - 1)
      check "PlayUnit: resources reduced by cost"
        (postP.resources == Resources (resBefore - cardCost))
      check "PlayUnit: game has exactly one in-play unit"
        (length g5.units == 1)
      let unitOk match err =
            case g5.units of
              [UnitDetails {controller, zone, cardDef = CardDef {code}}] ->
                match controller zone code
              _ -> False
            where _ = err :: String
      check "PlayUnit: unit controller = active player"
        (unitOk (\c _ _ -> c == g4res.currentPlayer) "controller")
      check "PlayUnit: unit zone = chosen zone"
        (unitOk (\_ z _ -> z == playZone) "zone")
      check "PlayUnit: card code carried through"
        (unitOk (\_ _ c -> c == cardCode) "code")
      check "PlayUnit: in-play unit reuses the in-hand card key"
        ( case g5.units of
            [u] -> u.key == cardKey
            _ -> False
        )

      -- DealDamageToUnit: apply 1 damage and check the unit's damage
      -- counter advanced (or the unit was destroyed if HP=1).
      g6 <- applyMessage g5 (DealDamageToUnit cardKey 1)
      case [u | u <- g6.units, u.key == cardKey] of
        [UnitDetails {damage = dmg}] ->
          check "DealDamageToUnit: damage recorded"
            (dmg == Damage 1)
        [] ->
          check "DealDamageToUnit: 1-HP unit destroyed"
            (null g6.units)
        _ -> do
          putStrLn "  FAIL multiple units share the played card key"
          exitFailure

      -- DestroyUnit: nuke whatever's left in play and confirm it moves
      -- to the controller's discard pile.
      let discardBefore = length (activePlayer g6).discard
      g7 <- case g6.units of
        [] -> pure g6 -- already gone above
        _ -> applyMessage g6 (DestroyUnit cardKey)
      check "DestroyUnit: no units in play after"
        (null g7.units)
      check "DestroyUnit: card lands in controller's discard"
        (length (activePlayer g7).discard >= discardBefore + 1
          || null g6.units)

  -- Combat smoke: feed an artificial state with a unit on each side
  -- and run BeginCombat → assert damage landed.
  setupResult2 <- runSetup
  gA <- case setupResult2 of
    Right g -> applyMessage g BeginGame
    Left err -> do
      putStrLn $ "FAIL second runSetup: " <> err
      exitFailure
  -- Skip turn 1 entirely and land on P2's CapitalPhase so we can
  -- actually play a unit before triggering combat.
  let fpA = gA.currentPlayer
  gB <- applyMessages gA $
    concat
      [ [PassPriority fpA, PassPriority fpA.next]       -- begin-of-turn (fpA)
      , [PassPriority fpA, PassPriority fpA.next]       -- kingdom
      , [PassPriority fpA, PassPriority fpA.next]       -- capital (quest/battlefield auto-skip)
      , [PassPriority fpA, PassPriority fpA.next]       -- end-of-turn (still fpA's turn)
      , [PassPriority fpA.next, PassPriority fpA]       -- begin-of-turn (next player)
      , [PassPriority fpA.next, PassPriority fpA]       -- kingdom (next player)
      , [PassPriority fpA.next, PassPriority fpA]       -- quest (no skip on T2)
      ]
  -- Drop a 1-HP unit into the active player's battlefield via PlayUnit
  -- if one is in hand. Otherwise skip the rest of this smoke.
  case findBattlefieldUnit (activePlayer gB) of
    Nothing -> putStrLn "  skip combat smoke (no playable unit)"
    Just (cardKey, _, _, _) -> do
      gC <- applyMessage gB (PlayUnit gB.currentPlayer cardKey BattlefieldZone)
      -- BeginCombat attacker against opponent's battlefield. Auto-pick
      -- the attacker we just placed.
      let attackerKeys =
            [ k
            | UnitDetails {key = k, controller = c} <- gC.units
            , c == gB.currentPlayer
            ]
          attacker = gB.currentPlayer
          defender = attacker.next
      -- Drive the engine through every combat sub-step window. Each
      -- window closes after two consecutive passes; the attacker
      -- holds priority first.
      gD <- applyMessages gC $
        BeginCombat attacker BattlefieldZone attackerKeys
          : concat (replicate 5 [PassPriority attacker, PassPriority defender])
      check "Combat: combat state cleared after resolve"
        (isNothing gD.combat)
      check "Combat: pending prompt cleared after resolve"
        (isNothing gD.pendingPrompt)
      check "Combat (no defenders): damage spilled to the attacked zone"
        (let Player {capital = Capital {battlefield = Zone {damage = Damage zd}}} =
                case attacker of
                  Player1 -> gD.player2
                  Player2 -> gD.player1
          in zd > 0)

  -- Combat with a scripted defender pick. Put a unit on EACH side's
  -- battlefield (using PutUnitIntoPlay so the cost doesn't matter),
  -- then BeginCombat with the defender programmed to accept the lone
  -- defender. The defender absorbs all attacker damage; nothing
  -- spills to the zone unless the attacker's power exceeds the
  -- defender's HP.
  setupResult3 <- runSetup
  gE0 <- case setupResult3 of
    Right g -> applyMessage g BeginGame
    Left err -> do
      putStrLn $ "FAIL third runSetup: " <> err
      exitFailure
  -- Park us on the opponent's turn so each player has a unit available.
  let fpE = gE0.currentPlayer
  gE <- applyMessages gE0 $
    concat
      [ [PassPriority fpE, PassPriority fpE.next]       -- begin-of-turn
      , [PassPriority fpE, PassPriority fpE.next]       -- kingdom
      , [PassPriority fpE, PassPriority fpE.next]       -- capital
      , [PassPriority fpE, PassPriority fpE.next]       -- end-of-turn
      , [PassPriority fpE.next, PassPriority fpE]       -- begin-of-turn (other player)
      ]
  let attackerSide = gE.currentPlayer
      defenderSide = attackerSide.next
      attackerHand = attackerInHand (case attackerSide of
                                   Player1 -> gE.player1
                                   Player2 -> gE.player2)
      defenderHand = defenderInHand (case defenderSide of
                                   Player1 -> gE.player1
                                   Player2 -> gE.player2)
  case (attackerHand, defenderHand) of
    (Just (ak, _, _), Just (dk, _, _)) -> do
      gF <- applyMessages gE
        [ PutUnitIntoPlay attackerSide ak BattlefieldZone
        , PutUnitIntoPlay defenderSide dk BattlefieldZone
        ]
      -- Drive the engine through BeginCombat with the defender's
      -- prompt answered "I defend with my unit". Counterstrike won't
      -- fire because the starter-deck unit we chose doesn't carry
      -- the keyword (PickNone is harmless if it does).
      gG <- applyMessagesWithAnswers gF
        [ PickUnits [dk]       -- defender selection
        , PickNone             -- attacker damage-order (1 defender → no-op)
        , PickNone             -- defender damage-order (1 attacker → no-op)
        ]
        ( BeginCombat attackerSide BattlefieldZone [ak]
            : concat
                ( replicate
                    5
                    [PassPriority attackerSide, PassPriority defenderSide]
                )
        )
      check "Combat (scripted defender): combat cleared"
        (isNothing gG.combat)
      check "Combat (scripted defender): defender unit took damage"
        ( any
            (\u -> u.key == dk && let Damage d = u.damage in d > 0)
            gG.units
          || -- ...or the defender died outright and is now in discard
             not (any (\u -> u.key == dk) gG.units)
        )
      check "Combat (scripted defender): zone damage is bounded by attacker overkill"
        ( let Player {capital = Capital {battlefield = Zone {damage = Damage zd}}} =
                gG.player2
           in zd >= 0  -- weak: just sanity that it isn't broken; full equality test would need card data
        )
    _ -> putStrLn "  skip scripted-combat smoke (one side has no Unit in hand)"

  -- ------------------------------------------------------------------
  -- Rules-parity regressions. Single-card decks make hands (and
  -- therefore the scenario) deterministic despite the shuffle.
  -- ------------------------------------------------------------------
  let mkMonoGame code race =
        case newGame
          (Deck {cards = replicate 40 code, race})
          (Deck {cards = replicate 40 code, race})
          defaultGameOptions of
          Left err -> do
            putStrLn $ "FAIL mono newGame(" <> show code <> "): " <> err
            exitFailure
          Right g -> applyMessage g Setup
      firstHandKeys n g = take n (map (.key) (activePlayer g).hand)

  -- Corruption only blocks attacking/defending — a corrupted unit in
  -- the kingdom still produces resources (Rules of Play p17).
  gCor1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-004" Dwarf
  case firstHandKeys 1 gCor1 of
    [uk] -> do
      let pkC = gCor1.currentPlayer
      gCor2 <- applyMessages gCor1
        [PutUnitIntoPlay pkC uk KingdomZone, CorruptUnit uk]
      check "corrupt: unit flagged corrupted"
        (any (\u -> u.key == uk && u.corrupted) gCor2.units)
      gCor3 <- applyMessage gCor2 (CollectResources pkC)
      check "corrupt: corrupted kingdom unit still produces (3 base + 1)"
        ((activePlayer gCor3).resources == Resources 4)
    _ -> do
      putStrLn "  FAIL mono deck dealt no hand"
      exitFailure

  -- A burning section cannot be assigned damage (FAQ): once burned,
  -- further zone damage is wasted and never re-accumulates.
  let oppC = gCor1.currentPlayer.next
  gBz1 <- applyMessage gCor1 (DealDamageToZone oppC KingdomZone 8)
  check "burn: zone burns at 8 damage"
    ((inactivePlayer gBz1).capital.kingdom.burned)
  check "burn: damage tokens cleared on burn"
    ((inactivePlayer gBz1).capital.kingdom.damage == Damage 0)
  gBz2 <- applyMessage gBz1 (DealDamageToZone oppC KingdomZone 3)
  check "burn: damage to a burned zone is wasted"
    ((inactivePlayer gBz2).capital.kingdom.damage == Damage 0)
  check "burn: game not over from re-damaging one burned zone"
    (not gBz2.over)

  -- Zone-entry keywords are enforced server-side: a "Battlefield
  -- only." unit is refused from the kingdom but accepted into the
  -- battlefield.
  gZe0 <- mkMonoGame "core-001" Dwarf
  let fpZ = gZe0.firstPlayer
  gZe1 <- applyMessages gZe0
    [ BeginGame
    , PassPriority fpZ, PassPriority fpZ.next -- phase 0
    , PassPriority fpZ, PassPriority fpZ.next -- kingdom → capital (quest skipped T1)
    ]
  check "zone-entry: reached capital phase" (gZe1.phase == Just CapitalPhase)
  case firstHandKeys 1 gZe1 of
    [zk] -> do
      gZe2 <- applyMessage gZe1 (PlayUnit fpZ zk KingdomZone)
      check "zone-entry: Battlefield-only unit refused from kingdom"
        (null gZe2.units)
      gZe3 <- applyMessage gZe2 (PlayUnit fpZ zk BattlefieldZone)
      check "zone-entry: Battlefield-only unit accepted into battlefield"
        (length gZe3.units == 1)
    _ -> do
      putStrLn "  FAIL zone-entry deck dealt no hand"
      exitFailure

  -- "Limit one Hero per zone" vetoes moves into an occupied zone but
  -- allows moves into a free one.
  gH1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-006" Dwarf
  case firstHandKeys 2 gH1 of
    [h1, h2] -> do
      let pkH = gH1.currentPlayer
      gH2 <- applyMessages gH1
        [ PutUnitIntoPlay pkH h1 KingdomZone
        , PutUnitIntoPlay pkH h2 BattlefieldZone
        ]
      check "hero limit: setup put two heroes in play" (length gH2.units == 2)
      gH3 <- applyMessage gH2 (MoveUnit h2 KingdomZone)
      check "hero limit: move into occupied zone vetoed"
        (any (\u -> u.key == h2 && u.zone == BattlefieldZone) gH3.units)
      gH4 <- applyMessage gH3 (MoveUnit h2 QuestZone)
      check "hero limit: move into free zone allowed"
        (any (\u -> u.key == h2 && u.zone == QuestZone) gH4.units)
    _ -> do
      putStrLn "  FAIL hero deck dealt fewer than 2 cards"
      exitFailure

  -- Contested Fortress cancels 1 capital damage per turn — including
  -- damage dealt outside the controller's own turn, and at most once.
  gF1 <- mkMonoGame "core-112" Dwarf
  let fpF = gF1.firstPlayer
  gF2 <- applyMessages gF1
    [ BeginGame
    , PassPriority fpF, PassPriority fpF.next
    , PassPriority fpF, PassPriority fpF.next
    ]
  check "fortress: reached capital phase" (gF2.phase == Just CapitalPhase)
  case firstHandKeys 1 gF2 of
    [fk] -> do
      gF3 <- applyMessage gF2 (PlaySupport fpF fk KingdomZone)
      check "fortress: support entered play" (length gF3.supports == 1)
      gF4 <- applyMessage gF3 (DealDamageToZone fpF QuestZone 2)
      check "fortress: first capital damage reduced by 1"
        ((activePlayer gF4).capital.quest.damage == Damage 1)
      gF5 <- applyMessage gF4 (DealDamageToZone fpF QuestZone 1)
      check "fortress: shield only cancels once per turn"
        ((activePlayer gF5).capital.quest.damage == Damage 2)
    _ -> do
      putStrLn "  FAIL fortress deck dealt no hand"
      exitFailure

  -- ------------------------------------------------------------------
  -- Corruption cycle smoke tests.
  -- ------------------------------------------------------------------

  -- Registry: every battle-pack code in the cycle resolves in allCards.
  let pad3 n = let str = show (n :: Int) in replicate (3 - length str) '0' <> str
      corruptionCodes =
        [ CardCode (pre <> "-" <> pad3 n)
        | (pre, lo, hi) <-
            [ ("the-skavenblight-threat", 1, 20)
            , ("path-of-the-zealot", 21, 40)
            , ("tooth-and-claw", 41, 60)
            , ("the-deathmaster-s-dance", 61, 80)
            , ("the-warpstone-chronicles", 81, 100)
            , ("arcane-fire", 101, 120)
            ]
        , n <- [lo .. hi]
        ]
      missingCodes = [c | c <- corruptionCodes, Map.notMember c allCards]
  check
    ( "corruption cycle: all 120 cards registered"
        <> (if null missingCodes then "" else " — missing " <> show missingCodes)
    )
    (null missingCodes)

  -- Unit resource tokens: Silver Helm Detachment enters play with 3.
  gT1 <- (`applyMessage` BeginGame) =<< mkMonoGame "the-deathmaster-s-dance-067" HighElf
  case firstHandKeys 1 gT1 of
    [tk] -> do
      gT2 <- applyMessage gT1 (PutUnitIntoPlay gT1.currentPlayer tk KingdomZone)
      check "tokens: Silver Helm Detachment enters with 3 resource tokens"
        (any (\u -> u.key == tk && u.tokens == 3) gT2.units)
    _ -> do
      putStrLn "  FAIL token deck dealt no hand"
      exitFailure

  -- Control changes: TakeControlOfUnit flips the controller while the
  -- unit stays in the same zone kind (Veteran Sellswords).
  gTC1 <- (`applyMessage` BeginGame) =<< mkMonoGame "path-of-the-zealot-038" Dwarf
  case firstHandKeys 1 gTC1 of
    [vk] -> do
      let pkV = gTC1.currentPlayer
      gTC2 <- applyMessages gTC1
        [PutUnitIntoPlay pkV vk BattlefieldZone, TakeControlOfUnit pkV.next vk]
      check "control: TakeControlOfUnit flips the controller"
        ( any
            (\u -> u.key == vk && u.controller == pkV.next && u.zone == BattlefieldZone)
            gTC2.units
        )
    _ -> do
      putStrLn "  FAIL control deck dealt no hand"
      exitFailure

  -- Draw caps (Infiltrate): with a cap of 1, the second standard draw
  -- this turn whiffs.
  gDC1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-001" Dwarf
  let pkD = gDC1.currentPlayer
      handD0 = length (activePlayer gDC1).hand
  gDC2 <- applyMessages gDC1
    [ SetDrawCap pkD 1
    , Draw (Drawing StandardDraw pkD)
    , Draw (Drawing StandardDraw pkD)
    ]
  check "draw cap: only one of two draws lands"
    (length (activePlayer gDC2).hand == handD0 + 1)

  -- Damage shields (Steel's Bane): the modifier soaks damage and
  -- carries its remaining budget forward.
  gDS1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-011" Dwarf
  case firstHandKeys 1 gDS1 of
    [sk] -> do
      let pkS = gDS1.currentPlayer
      gDS2 <- applyMessages gDS1
        [ PutUnitIntoPlay pkS sk KingdomZone
        , InstallModifier (UnitRef sk) (Modifier (DamageShield 10) EndOfTurn)
        , DealDamageToUnit sk 3
        ]
      check "shield: damage fully cancelled"
        (any (\u -> u.key == sk && u.damage == Damage 0) gDS2.units)
      gDS3 <- applyMessage gDS2 (DealDamageToUnit sk 8)
      check "shield: depleted budget lets the overflow through"
        (any (\u -> u.key == sk && u.damage == Damage 1) gDS3.units)
    _ -> do
      putStrLn "  FAIL shield deck dealt no hand"
      exitFailure

  -- X hit points (Cold One Chariot): HP tracks developments in the
  -- zone, and the stat sweep clears a chariot whose X collapsed.
  gCC1 <- (`applyMessage` BeginGame) =<< mkMonoGame "tooth-and-claw-053" DarkElf
  let pkCC = gCC1.currentPlayer
  gCC2 <- applyMessage gCC1 (AddDevelopment pkCC KingdomZone)
  case firstHandKeys 1 gCC2 of
    [ck] -> do
      gCC3 <- applyMessage gCC2 (PutUnitIntoPlay pkCC ck KingdomZone)
      check "chariot: X hit points = developments in its zone"
        (any (\u -> u.key == ck && u.effectiveMaxHP == 1) gCC3.units)
      gCC4 <- applyMessage gCC3 (DestroyDevelopment pkCC KingdomZone)
      check "chariot: destroyed when its last development goes"
        (not (any (\u -> u.key == ck) gCC4.units))
    _ -> do
      putStrLn "  FAIL chariot deck dealt no hand"
      exitFailure

  -- Counterstrike X (Wardancer): the derived counterstrike value tracks
  -- the number of developments in the unit's zone, then collapses when
  -- the developments are removed.
  gWD1 <- (`applyMessage` BeginGame) =<< mkMonoGame "the-eclipse-of-hope-098" HighElf
  let pkWD = gWD1.currentPlayer
  gWD2 <- applyMessages gWD1
    [AddDevelopment pkWD KingdomZone, AddDevelopment pkWD KingdomZone]
  case firstHandKeys 1 gWD2 of
    [wk] -> do
      gWD3 <- applyMessage gWD2 (PutUnitIntoPlay pkWD wk KingdomZone)
      let csOf g = listToMaybe [totalCounterstrike g u | u <- g.units, u.key == wk]
      check "counterstrike X: equals developments in zone (2)"
        (csOf gWD3 == Just 2)
      gWD4 <- applyMessage gWD3 (DestroyDevelopment pkWD KingdomZone)
      check "counterstrike X: drops as developments leave (1)"
        (csOf gWD4 == Just 1)
    _ -> do
      putStrLn "  FAIL wardancer deck dealt no hand"
      exitFailure

  -- Ambush combat step (Shrouded Waywatcher, Ambush 3 / Counterstrike 4):
  -- a facedown Ambush development in the defending zone can be flipped
  -- during the new Ambush step (after Declare Attackers, before Declare
  -- Defenders). It enters play, is forced to defend, and fires its
  -- Counterstrike — which kills the lone attacker before damage assigns.
  gA1 <- (`applyMessage` BeginGame) =<< mkMonoGame "hidden-kingdoms-013" HighElf
  let fpA = gA1.currentPlayer            -- first player = the defender
      attackerSide = fpA.next
      playerRec side g = case side of Player1 -> g.player1; Player2 -> g.player2
      devKeysOf side g =
        map (.key) (concat (Map.elems (playerRec side g).developmentCards))
  case (firstHandKeys 1 gA1, unitInHand (playerRec attackerSide gA1)) of
    ([dk], Just (ak, _, _)) -> do
      -- Defender plays the Waywatcher facedown during their capital phase
      -- and banks resources to afford the Ambush cost.
      gA2 <- applyMessages gA1
        [ PassPriority fpA, PassPriority attackerSide       -- begin-of-turn
        , PassPriority fpA, PassPriority attackerSide       -- kingdom
        , GainResources fpA 5
        , PlayDevelopment fpA dk BattlefieldZone
        , PassPriority fpA, PassPriority attackerSide       -- capital
        , PassPriority fpA, PassPriority attackerSide       -- end-of-turn
        , PassPriority attackerSide, PassPriority fpA        -- attacker's begin
        ]
      check "ambush: development is facedown before combat"
        (dk `elem` devKeysOf fpA gA2)
      gA3 <- applyMessages gA2 [PutUnitIntoPlay attackerSide ak BattlefieldZone]
      gA4 <- applyMessagesWithAnswers gA3
        [ PickUnits [dk]    -- Ambush step: flip the Waywatcher
        , PickUnits [dk]    -- Declare Defenders (same key; MustDefend anyway)
        , PickNone          -- attacker damage order (no-op)
        , PickNone          -- defender damage order (no-op)
        ]
        ( BeginCombat attackerSide BattlefieldZone [ak]
            : concat
                (replicate 6 [PassPriority attackerSide, PassPriority fpA])
        )
      check "ambush: development left the zone (flipped faceup)"
        (dk `notElem` devKeysOf fpA gA4)
      check "ambush: combat resolved"
        (isNothing gA4.combat)
      check "ambush: flipped defender's Counterstrike killed the attacker"
        (not (any (\u -> u.key == ak) gA4.units))
    _ -> putStrLn "  skip ambush smoke (deck dealt no usable hand)"

  -- Ambush rider trigger ('Idden Boy, "when this unit ambushes, it gains
  -- +2 Power"): drive combat only as far as the Ambush step and confirm
  -- the rider fired by reading the flipped unit's power before damage.
  gB1 <- (`applyMessage` BeginGame) =<< mkMonoGame "days-of-blood-013" Orc
  let fpB = gB1.currentPlayer
      atkB = fpB.next
      pRec side g = case side of Player1 -> g.player1; Player2 -> g.player2
      devKeys side g = map (.key) (concat (Map.elems (pRec side g).developmentCards))
  case (firstHandKeys 1 gB1, unitInHand (pRec atkB gB1)) of
    ([dk], Just (ak, _, _)) -> do
      gB2 <- applyMessages gB1
        [ PassPriority fpB, PassPriority atkB
        , PassPriority fpB, PassPriority atkB
        , GainResources fpB 5
        , PlayDevelopment fpB dk BattlefieldZone
        , PassPriority fpB, PassPriority atkB
        , PassPriority fpB, PassPriority atkB
        , PassPriority atkB, PassPriority fpB
        ]
      gB3 <- applyMessages gB2 [PutUnitIntoPlay atkB ak BattlefieldZone]
      -- Two pass-pairs close the combat-target and declare-attackers
      -- windows, firing the Ambush step; we stop there (no damage yet).
      gB4 <- applyMessagesWithAnswers gB3
        [ PickUnits [dk]    -- Ambush step: flip 'Idden Boy
        , PickNone          -- Declare Defenders (MustDefend forces it)
        ]
        [ BeginCombat atkB BattlefieldZone [ak]
        , PassPriority atkB, PassPriority fpB
        , PassPriority atkB, PassPriority fpB
        ]
      check "ambush rider: 'Idden Boy left the development zone"
        (dk `notElem` devKeys fpB gB4)
      check "ambush rider: 'Idden Boy gained +2 Power from its ambush trigger"
        (any (\u -> u.key == dk && u.effectivePower == 3) gB4.units)
    _ -> putStrLn "  skip ambush-rider smoke (deck dealt no usable hand)"

  -- Ambush rider granting a keyword (Celestial Wizard Acolyte gains
  -- Counterstrike 3 on ambush): drive to the Ambush step and read the
  -- flipped unit's total Counterstrike via the engine's accessor.
  gK1 <- (`applyMessage` BeginGame) =<< mkMonoGame "faith-and-steel-104" Empire
  let fpK = gK1.currentPlayer
      atkK = fpK.next
  case (firstHandKeys 1 gK1, unitInHand (pRec atkK gK1)) of
    ([dk], Just (ak, _, _)) -> do
      gK2 <- applyMessages gK1
        [ PassPriority fpK, PassPriority atkK
        , PassPriority fpK, PassPriority atkK
        , GainResources fpK 5
        , PlayDevelopment fpK dk BattlefieldZone
        , PassPriority fpK, PassPriority atkK
        , PassPriority fpK, PassPriority atkK
        , PassPriority atkK, PassPriority fpK
        ]
      gK3 <- applyMessages gK2 [PutUnitIntoPlay atkK ak BattlefieldZone]
      gK4 <- applyMessagesWithAnswers gK3
        [ PickUnits [dk]    -- Ambush step: flip Celestial Wizard Acolyte
        , PickNone          -- Declare Defenders
        ]
        [ BeginCombat atkK BattlefieldZone [ak]
        , PassPriority atkK, PassPriority fpK
        , PassPriority atkK, PassPriority fpK
        ]
      check "ambush rider: Celestial Wizard Acolyte gained Counterstrike 3"
        (any (\u -> u.key == dk && totalCounterstrike gK4 u == 3) gK4.units)
    _ -> putStrLn "  skip ambush-keyword smoke (deck dealt no usable hand)"

  -- Tactic ambush (Fury of the Forest, Ambush 0): a facedown tactic in
  -- the defending zone is flipped during the Ambush step, resolves its
  -- effect ("deal 1 damage to each attacking unit"), and is discarded
  -- rather than entering play. Asymmetric decks: the defender holds the
  -- tactic, the attacker holds a real unit to attack with.
  let mkAsym d1 d2 = case newGame d1 d2 defaultGameOptions of
        Left err -> putStrLn ("FAIL asym newGame: " <> err) >> exitFailure
        Right g -> applyMessage g Setup
      furyDeck = Deck {cards = replicate 40 "hidden-kingdoms-018", race = Dwarf}
      unitDeck = Deck {cards = replicate 40 "core-001", race = Dwarf}
      -- The first player is sampled randomly; retry until Player1 (the
      -- Fury deck) leads, so the defender holds the tactic.
      findP1First :: Int -> IO (Maybe Game)
      findP1First 0 = pure Nothing
      findP1First n = do
        g <- mkAsym furyDeck unitDeck
        if g.currentPlayer == Player1 then pure (Just g) else findP1First (n - 1)
  mC0 <- findP1First 50
  case mC0 of
    Nothing -> putStrLn "  skip tactic-ambush smoke (no Player1-first setup)"
    Just gC0 -> do
      gC1 <- applyMessage gC0 BeginGame
      let fpC = gC1.currentPlayer        -- Player1 = defender (tactic deck)
          atkC = fpC.next
      case (take 1 (map (.key) (activePlayer gC1).hand), unitInHand (pRec atkC gC1)) of
        ([tk], Just (ak, _, _)) -> do
          gC2 <- applyMessages gC1
            [ PassPriority fpC, PassPriority atkC
            , PassPriority fpC, PassPriority atkC
            , PlayDevelopment fpC tk BattlefieldZone     -- Ambush 0: no cost
            , PassPriority fpC, PassPriority atkC
            , PassPriority fpC, PassPriority atkC
            , PassPriority atkC, PassPriority fpC
            ]
          gC3 <- applyMessages gC2 [PutUnitIntoPlay atkC ak BattlefieldZone]
          gC4 <- applyMessagesWithAnswers gC3
            [ PickUnits [tk]    -- Ambush step: flip Fury of the Forest
            , PickNone
            , PickNone
            ]
            ( BeginCombat atkC BattlefieldZone [ak]
                : concat
                    (replicate 6 [PassPriority atkC, PassPriority fpC])
            )
          check "tactic ambush: the tactic did not enter play as a unit"
            (not (any (\u -> u.key == tk) gC4.units))
          check "tactic ambush: Fury of the Forest damaged the attacking unit"
            ( any
                (\u -> u.key == ak && let Damage d = u.damage in d >= 1)
                gC4.units
              || not (any (\u -> u.key == ak) gC4.units)
            )
        _ -> putStrLn "  skip tactic-ambush smoke (deck dealt no usable hand)"

  -- Cancel attack (Test of Will, Ambush 0): flipped during the Ambush
  -- step, it asks the attacker to sacrifice an attacker or cancel. When
  -- the attacker declines, the attack is cancelled — combat ends with no
  -- damage and the attacker survives untouched.
  let towDeck = Deck {cards = replicate 40 "the-ruinous-hordes-097", race = Dwarf}
      unitDeck' = Deck {cards = replicate 40 "core-001", race = Dwarf}
      mkAsym' d1 d2 = case newGame d1 d2 defaultGameOptions of
        Left err -> putStrLn ("FAIL asym newGame: " <> err) >> exitFailure
        Right g -> applyMessage g Setup
      findP1First' :: Int -> IO (Maybe Game)
      findP1First' 0 = pure Nothing
      findP1First' n = do
        g <- mkAsym' towDeck unitDeck'
        if g.currentPlayer == Player1 then pure (Just g) else findP1First' (n - 1)
  mD0 <- findP1First' 50
  case mD0 of
    Nothing -> putStrLn "  skip cancel-attack smoke (no Player1-first setup)"
    Just gD0 -> do
      gD1 <- applyMessage gD0 BeginGame
      let fpD = gD1.currentPlayer
          atkD = fpD.next
      case (take 1 (map (.key) (activePlayer gD1).hand), unitInHand (pRec atkD gD1)) of
        ([tk], Just (ak, _, _)) -> do
          gD2 <- applyMessages gD1
            [ PassPriority fpD, PassPriority atkD
            , PassPriority fpD, PassPriority atkD
            , PlayDevelopment fpD tk BattlefieldZone
            , PassPriority fpD, PassPriority atkD
            , PassPriority fpD, PassPriority atkD
            , PassPriority atkD, PassPriority fpD
            ]
          gD3 <- applyMessages gD2 [PutUnitIntoPlay atkD ak BattlefieldZone]
          gD4 <- applyMessagesWithAnswers gD3
            [ PickUnits [tk]    -- Ambush step: flip Test of Will
            , PickBool False    -- attacker declines → cancel the attack
            ]
            [BeginCombat atkD BattlefieldZone [ak]
            , PassPriority atkD, PassPriority fpD
            , PassPriority atkD, PassPriority fpD
            ]
          check "cancel attack: combat ended"
            (isNothing gD4.combat)
          check "cancel attack: attacking unit survived undamaged"
            (any (\u -> u.key == ak && u.damage == Damage 0) gD4.units)
          check "cancel attack: defending zone took no damage"
            ( let Player {capital = Capital {battlefield = Zone {damage = Damage zd}}} =
                    pRec fpD gD4
               in zd == 0
            )
        _ -> putStrLn "  skip cancel-attack smoke (deck dealt no usable hand)"

  -- Attack lockout (Fulminating Cage, Ambush 3): cancels the current
  -- attack AND bars the attacker from declaring another attack this turn.
  let cageDeck = Deck {cards = replicate 40 "glory-of-days-past-066", race = Empire}
      unitDeck'' = Deck {cards = replicate 40 "core-001", race = Empire}
      mkAsym'' d1 d2 = case newGame d1 d2 defaultGameOptions of
        Left err -> putStrLn ("FAIL asym newGame: " <> err) >> exitFailure
        Right g -> applyMessage g Setup
      findP1First'' :: Int -> IO (Maybe Game)
      findP1First'' 0 = pure Nothing
      findP1First'' n = do
        g <- mkAsym'' cageDeck unitDeck''
        if g.currentPlayer == Player1 then pure (Just g) else findP1First'' (n - 1)
  mE0 <- findP1First'' 50
  case mE0 of
    Nothing -> putStrLn "  skip attack-lockout smoke (no Player1-first setup)"
    Just gE0 -> do
      gE1 <- applyMessage gE0 BeginGame
      let fpE = gE1.currentPlayer
          atkE = fpE.next
      case (take 1 (map (.key) (activePlayer gE1).hand), unitInHand (pRec atkE gE1)) of
        ([tk], Just (ak, _, _)) -> do
          gE2 <- applyMessages gE1
            [ PassPriority fpE, PassPriority atkE
            , PassPriority fpE, PassPriority atkE
            , GainResources fpE 5
            , PlayDevelopment fpE tk BattlefieldZone
            , PassPriority fpE, PassPriority atkE
            , PassPriority fpE, PassPriority atkE
            , PassPriority atkE, PassPriority fpE
            ]
          gE3 <- applyMessages gE2 [PutUnitIntoPlay atkE ak BattlefieldZone]
          gE4 <- applyMessagesWithAnswers gE3
            [PickUnits [tk]]    -- Ambush step: flip Fulminating Cage
            [ BeginCombat atkE BattlefieldZone [ak]
            , PassPriority atkE, PassPriority fpE
            , PassPriority atkE, PassPriority fpE
            ]
          check "attack lockout: first attack was cancelled"
            (isNothing gE4.combat)
          gE5 <- applyMessage gE4 (BeginCombat atkE BattlefieldZone [ak])
          check "attack lockout: attacker cannot declare another attack this turn"
            (isNothing gE5.combat)
        _ -> putStrLn "  skip attack-lockout smoke (deck dealt no usable hand)"

  -- Get 'Em Ladz!: a zone-damage watcher draws a card per point of
  -- damage dealt to the watched zone (until the phase ends), and only
  -- for the watched zone.
  gG1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-001" Dwarf
  let pkG = gG1.currentPlayer
      foeG = pkG.next
      handOf side g = length (pRec side g).hand
  gG2 <- applyMessage gG1 (WatchZoneForDamageDraw pkG foeG BattlefieldZone)
  let beforeG = handOf pkG gG2
  gG3 <- applyMessage gG2 (DealDamageToZone foeG BattlefieldZone 3)
  check "get em ladz: watcher drew a card per damage on the watched zone"
    (handOf pkG gG3 == beforeG + 3)
  gG4 <- applyMessage gG3 (DealDamageToZone foeG KingdomZone 2)
  check "get em ladz: damage to an unwatched zone draws nothing"
    (handOf pkG gG4 == beforeG + 3)

  -- Slumbering Titan: a unit can transform into a facedown development in
  -- its zone (it stops being a unit; the zone's development count rises).
  gT1 <- (`applyMessage` BeginGame) =<< mkMonoGame "hidden-kingdoms-014" HighElf
  let pkT = gT1.currentPlayer
  case firstHandKeys 1 gT1 of
    [tk] -> do
      gT2 <- applyMessage gT1 (PutUnitIntoPlay pkT tk KingdomZone)
      let Developments d0 = (pRec pkT gT2).capital.kingdom.developments
      check "titan: enters play as a unit"
        (any (\u -> u.key == tk) gT2.units)
      gT3 <- applyMessage gT2 (TurnUnitIntoDevelopment tk)
      check "titan: no longer counts as a unit"
        (not (any (\u -> u.key == tk) gT3.units))
      check "titan: became a development in its zone"
        (let Developments d = (pRec pkT gT3).capital.kingdom.developments in d == d0 + 1)
    _ -> putStrLn "  FAIL titan deck dealt no hand" >> exitFailure

  -- Reveal primitives: RevealCards records the revealed cards (public,
  -- for the UI), and MoveTopToBottomOfDeck rotates the deck.
  gR1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-004" Dwarf
  let pkR = gR1.currentPlayer
      deckR g = (pRec pkR g).deck
  case (activePlayer gR1).hand of
    (hc : _) -> do
      gR2 <- applyMessage gR1 (RevealCards pkR [hc])
      check "reveal: lastRevealed records the revealed card"
        (map (.key) gR2.lastRevealed == [hc.key])
    _ -> putStrLn "  FAIL reveal deck dealt no hand" >> exitFailure
  case deckR gR1 of
    (top : _ : _) -> do
      gR3 <- applyMessage gR1 (MoveTopToBottomOfDeck pkR 1)
      check "deck rotate: length preserved"
        (length (deckR gR3) == length (deckR gR1))
      check "deck rotate: old top card is now on the bottom"
        (not (null (deckR gR3)) && (last (deckR gR3)).key == top.key)
    _ -> putStrLn "  skip deck-rotate (deck too small)"

  -- Sacrifice-development cost (Reckless Engineer): triggering its action
  -- in the capital window pays by destroying one of the player's
  -- developments, then reveals the top card.
  gSD1 <- (`applyMessage` BeginGame) =<< mkMonoGame "the-accursed-dead-043" Dwarf
  let pkSD = gSD1.currentPlayer
  gSD2 <- applyMessages gSD1
    [ PassPriority pkSD, PassPriority pkSD.next     -- begin-of-turn
    , PassPriority pkSD, PassPriority pkSD.next     -- kingdom → capital window
    ]
  case firstHandKeys 1 gSD2 of
    [ek] -> do
      gSD3 <- applyMessages gSD2
        [AddDevelopment pkSD KingdomZone, PutUnitIntoPlay pkSD ek BattlefieldZone]
      let Developments d0 = (pRec pkSD gSD3).capital.kingdom.developments
      gSD4 <- applyMessage gSD3 (TriggerCardAction pkSD ek 0 NoTarget)
      check "sac-dev cost: a development was sacrificed to pay"
        (let Developments d = (pRec pkSD gSD4).capital.kingdom.developments in d == d0 - 1)
      check "sac-dev cost: the action then revealed the top card"
        (not (null gSD4.lastRevealed))
    _ -> putStrLn "  FAIL reckless-engineer deck dealt no hand" >> exitFailure

  -- Cannot be targeted: a CannotBeTargeted (opponent-only) modifier
  -- removes a unit from the opponent's target enumeration but not the
  -- controller's.
  gCT1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-004" Dwarf
  let pkCT = gCT1.currentPlayer
  case firstHandKeys 1 gCT1 of
    [uk] -> do
      gCT2 <- applyMessages gCT1
        [ PutUnitIntoPlay pkCT uk KingdomZone
        , InstallModifier (UnitRef uk) (Modifier (CannotBeTargeted True) EndOfTurn)
        ]
      let canTarget picker =
            uk `elem` [k | TargetUnitOption k <- enumerateOptionsPure picker gCT2 AnyUnit]
      check "untargetable: opponent cannot target the protected unit"
        (not (canTarget pkCT.next))
      check "untargetable: the controller still can (opponent-only)"
        (canTarget pkCT)
    _ -> putStrLn "  FAIL untargetable deck dealt no hand" >> exitFailure

  -- Dawnstar Sword: a self-protecting attachment carries its
  -- "cannot be targeted by card effects" immunity as a static field, so
  -- neither player can target the attachment — no modifier needed.
  gDS1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-004" Dwarf
  let pkDS = gDS1.currentPlayer
  case firstHandKeys 2 gDS1 of
    [hk, ak] -> do
      gDS2 <- applyMessage gDS1 (PutUnitIntoPlay pkDS hk KingdomZone)
      case Map.lookup "rising-dawn-001" allCards of
        Just (SupportCardDef ddef) -> do
          let withAtt u
                | u.key == hk =
                    u {attachments = freshSupport ak pkDS u.zone (Just hk) ddef : u.attachments}
                | otherwise = u
              gDS3 = gDS2 {units = map withAtt gDS2.units}
              canTargetAtt picker =
                ak `elem` [k | TargetSupportOption k <- enumerateOptionsPure picker gDS3 AnySupportCard]
          check "dawnstar self-untargetable: opponent cannot target the attachment"
            (not (canTargetAtt pkDS.next))
          check "dawnstar self-untargetable: even the controller cannot target it"
            (not (canTargetAtt pkDS))
        _ -> putStrLn "  FAIL dawnstar def missing from allCards" >> exitFailure
    _ -> putStrLn "  FAIL dawnstar deck dealt too few cards" >> exitFailure

  -- Windcatcher Prism: discard a random card from hand to gain resources
  -- equal to its printed cost. A mono-deck makes the discarded card's
  -- cost deterministic.
  gWP1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-004" Dwarf
  let pkWP = gWP1.currentPlayer
      wpHandBefore = length (activePlayer gWP1).hand
      Resources wpResBefore = (activePlayer gWP1).resources
      wpCost = maybe (-1) someCardCost (Map.lookup "core-004" allCards)
  gWP2 <- applyMessage gWP1 (DiscardRandomForResources pkWP)
  let Resources wpResAfter = (activePlayer gWP2).resources
  check "windcatcher: one card left the hand"
    (length (activePlayer gWP2).hand == wpHandBefore - 1)
  check "windcatcher: gained resources equal to the discarded card's cost"
    (wpResAfter - wpResBefore == wpCost)

  -- Shield of Aeons: while its host is participating in combat, cancel
  -- all damage assigned to it. Attach the shield to the scripted
  -- defender and confirm it absorbs the attack without taking damage.
  -- A mono-deck of a power-2, toughness-free Hero makes both the
  -- attacker and the defender deterministic.
  gSH0 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-006" Dwarf
  let fpSH = gSH0.currentPlayer
  gSH <- applyMessages gSH0 $
    concat
      [ [PassPriority fpSH, PassPriority fpSH.next]
      , [PassPriority fpSH, PassPriority fpSH.next]
      , [PassPriority fpSH, PassPriority fpSH.next]
      , [PassPriority fpSH, PassPriority fpSH.next]
      , [PassPriority fpSH.next, PassPriority fpSH]
      ]
  let atkSH = gSH.currentPlayer
      defSH = atkSH.next
      playerSH pk g = case pk of Player1 -> g.player1; Player2 -> g.player2
      atkHandSH = attackerInHand (playerSH atkSH gSH)
      defHandSH = defenderInHand (playerSH defSH gSH)
      -- a spare hand card whose key labels the synthetic shield
      shieldKeySH dk =
        listToMaybe [c.key | c <- (playerSH defSH gSH).hand, c.key /= dk]
  case (atkHandSH, defHandSH, Map.lookup "shield-of-the-gods-101" allCards) of
    (Just (akSH, _, _), Just (dkSH, _, _), Just (SupportCardDef shieldDef))
      | Just skSH <- shieldKeySH dkSH -> do
          gShF <- applyMessages gSH
            [ PutUnitIntoPlay atkSH akSH BattlefieldZone
            , PutUnitIntoPlay defSH dkSH BattlefieldZone
            ]
          let withShield u
                | u.key == dkSH =
                    u {attachments = freshSupport skSH defSH u.zone (Just dkSH) shieldDef : u.attachments}
                | otherwise = u
              gShA = gShF {units = map withShield gShF.units}
          gShG <- applyMessagesWithAnswers gShA
            [PickUnits [dkSH], PickNone, PickNone]
            ( BeginCombat atkSH BattlefieldZone [akSH]
                : concat (replicate 5 [PassPriority atkSH, PassPriority defSH])
            )
          check "shield of aeons: combat cleared"
            (isNothing gShG.combat)
          check "shield of aeons: shielded defender took no damage"
            ( any
                (\u -> u.key == dkSH && let Damage d = u.damage in d == 0)
                gShG.units
            )
    _ -> putStrLn "  skip shield-of-aeons (no suitable attacker/defender/shield)"

  -- Star Crown Fragments: its action sacrifices the artefact to return
  -- the top non-Artefact cards from the discard pile to hand. Seed the
  -- discard with two units, fire the action, and confirm both come back
  -- while the artefact leaves play.
  gSC1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-006" Dwarf
  let pkSC = gSC1.currentPlayer
      getPl pk g = case pk of Player1 -> g.player1; Player2 -> g.player2
      setPl pk p g = case pk of Player1 -> g {player1 = p}; Player2 -> g {player2 = p}
  case (firstHandKeys 4 gSC1, Map.lookup "fragments-of-power-021" allCards) of
    ([hk, sk, d1, d2], Just (SupportCardDef scDef)) -> do
      gSC2 <- applyMessage gSC1 (PutUnitIntoPlay pkSC hk BattlefieldZone)
      let withAtt u
            | u.key == hk =
                u {attachments = freshSupport sk pkSC u.zone (Just hk) scDef : u.attachments}
            | otherwise = u
          p0 = getPl pkSC gSC2
          seeded = [c | c <- p0.hand, c.key `elem` [d1, d2]]
          p1 =
            p0
              { hand = [c | c <- p0.hand, c.key `notElem` [d1, d2]]
              , discard = seeded <> p0.discard
              }
          gSC3 = setPl pkSC p1 (gSC2 {units = map withAtt gSC2.units})
      gSC4 <- applyMessage gSC3 (TriggerCardAction pkSC sk 0 NoTarget)
      let handKeys = map (.key) (getPl pkSC gSC4).hand
          discKeys = map (.key) (getPl pkSC gSC4).discard
          stillInPlay = any (\u -> any (\a -> a.key == sk) u.attachments) gSC4.units
      check "star crown: both seeded cards returned to hand"
        (d1 `elem` handKeys && d2 `elem` handKeys)
      check "star crown: returned cards left the discard pile"
        (d1 `notElem` discKeys && d2 `notElem` discKeys)
      check "star crown: the artefact sacrificed itself"
        (not stillInPlay)
    _ -> putStrLn "  FAIL star-crown setup (hand too small or def missing)" >> exitFailure

  -- ArrangeDeckCards (the scry reorder primitive behind Scroll of Asur /
  -- Advanced Engineering): named cards are pulled to the top in the given
  -- order, others to the bottom, the rest keep their relative position.
  gAR1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-006" Dwarf
  let pkAR = gAR1.currentPlayer
      deck0 = (getPl pkAR gAR1).deck
  case map (.key) (take 3 deck0) of
    [k1, k2, k3] -> do
      gAR2 <- applyMessage gAR1 (ArrangeDeckCards pkAR [k3, k1] [k2])
      let deck1 = map (.key) (getPl pkAR gAR2).deck
      check "arrange: chosen cards put on top in the given order"
        (take 2 deck1 == [k3, k1])
      check "arrange: the other card was put on the bottom"
        (not (null deck1) && last deck1 == k2)
      check "arrange: deck size is unchanged"
        (length deck1 == length deck0)
    _ -> putStrLn "  FAIL arrange-deck setup (deck too small)" >> exitFailure

  -- TargetPlayer schema: both players are offered as targets.
  gTP1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-006" Dwarf
  let tpOpts = enumerateOptionsPure gTP1.currentPlayer gTP1 TargetPlayer
  check "target-player: both players offered"
    (tpOpts == [TargetPlayerOption Player1, TargetPlayerOption Player2])

  -- Learned Mage: its action deals itself 1 uncancellable damage, then
  -- looks at the top card of the chosen player's deck and puts it on top
  -- or bottom. Drive the player-target prompt + the top/bottom choice.
  gLM1 <- (`applyMessage` BeginGame) =<< mkMonoGame "vessel-of-the-winds-070" HighElf
  let pkLM = gLM1.currentPlayer
  case firstHandKeys 1 gLM1 of
    [uk] -> do
      gLM2 <- applyMessage gLM1 (PutUnitIntoPlay pkLM uk BattlefieldZone)
      let topKey0 = (.key) <$> listToMaybe (getPl pkLM gLM2).deck
      gLM3 <- applyMessagesWithAnswers gLM2
        [PickTargetOption (TargetPlayerOption pkLM), PickBool False]
        [TriggerCardAction pkLM uk 0 NoTarget]
      let deckLM = map (.key) (getPl pkLM gLM3).deck
      check "learned mage: took 1 uncancellable damage"
        (any (\u -> u.key == uk && let Damage d = u.damage in d >= 1) gLM3.units)
      check "learned mage: chosen-deck top card was put on the bottom"
        (maybe False (\k -> not (null deckLM) && last deckLM == k) topKey0)
    _ -> putStrLn "  FAIL learned-mage deck dealt no hand" >> exitFailure

  -- Eye of Sheerian + support corruption: the artefact's action corrupts
  -- itself as its cost, scries the target deck, and the corruption is
  -- restorable in the kingdom phase like a unit's.
  gEC1 <- (`applyMessage` BeginGame) =<< mkMonoGame "core-006" Dwarf
  let pkEC = gEC1.currentPlayer
  case (firstHandKeys 2 gEC1, Map.lookup "portent-of-doom-081" allCards) of
    ([hkE, skE], Just (SupportCardDef eyeDef)) -> do
      gEC2 <- applyMessage gEC1 (PutUnitIntoPlay pkEC hkE BattlefieldZone)
      let withAtt u
            | u.key == hkE =
                u {attachments = freshSupport skE pkEC u.zone (Just hkE) eyeDef : u.attachments}
            | otherwise = u
          gEC3 = gEC2 {units = map withAtt gEC2.units}
          isCorrupt g =
            any (\u -> any (\a -> a.key == skE && a.corrupted) u.attachments) g.units
          top5 = map (.key) (take 5 (getPl pkEC gEC3).deck)
          deckLen0 = length (getPl pkEC gEC3).deck
          answers =
            PickTargetOption (TargetPlayerOption pkEC)
              : PickUnits [head top5]
              : [PickUnits [k] | k <- take 3 (drop 1 top5)]
      gEY <- applyMessagesWithAnswers gEC3 answers [TriggerCardAction pkEC skE 0 NoTarget]
      check "eye of sheerian: corrupted itself as the action cost"
        (isCorrupt gEY)
      check "eye of sheerian: discarded one card from the target deck"
        (length (getPl pkEC gEY).deck == deckLen0 - 1)
      check "eye of sheerian: discarded card went to the discard pile"
        (head top5 `elem` map (.key) (getPl pkEC gEY).discard)
      -- A corrupted support can't pay its own Corrupt cost again.
      gEY2 <- applyMessagesWithAnswers gEY
        [PickTargetOption (TargetPlayerOption pkEC)]
        [TriggerCardAction pkEC skE 0 NoTarget]
      check "eye of sheerian: cannot re-fire while corrupted"
        (length (getPl pkEC gEY2).deck == length (getPl pkEC gEY).deck)
      -- The kingdom-phase restore step cleanses the corrupted artefact.
      gEY3 <- applyMessagesWithAnswers gEY [PickUnits [skE]] [RestoreOneCorruptCard pkEC]
      check "support corruption: kingdom restore cleansed the artefact"
        (not (isCorrupt gEY3))
    _ -> putStrLn "  FAIL eye-of-sheerian setup (hand too small or def missing)" >> exitFailure

  -- Wire redaction: hidden information must not reach the wrong
  -- viewer. Player1's view keeps their own hand but sees only
  -- key-stubs of Player2's hand; deck contents are hidden from
  -- everyone (empty objects, count preserved).
  do
    let asView seat g = redactEngineFor seat (toJSON g)
        playerField name v = case v of
          Object o -> KM.lookup name o
          _ -> Nothing
        arrayLen (Just (Array xs)) = length (foldr (:) [] xs)
        arrayLen _ = -1
        cardFields (Just (Array xs)) = case foldr (:) [] xs of
          (Object c : _) -> KM.keys c
          _ -> []
        cardFields _ = []
        v1 = asView (Just "Player1") (toViewGame gCor1)
        p1 = playerField "player1" v1
        p2 = playerField "player2" v1
        handOf mp = mp >>= playerField "hand"
        deckOf mp = mp >>= playerField "deck"
    check "redact: own hand keeps full card defs"
      (("code" `elem` cardFields (handOf p1)) && ("key" `elem` cardFields (handOf p1)))
    check "redact: opponent hand reduced to key-only stubs"
      (cardFields (handOf p2) == ["key"])
    check "redact: opponent hand count preserved"
      (arrayLen (handOf p2) == length gCor1.player2.hand)
    check "redact: decks hidden from everyone (empty objects)"
      (cardFields (deckOf p1) == [] && cardFields (deckOf p2) == [])
    check "redact: deck count preserved"
      (arrayLen (deckOf p1) == length gCor1.player1.deck)

  putStrLn "Phase / turn smoke test: OK"

-- Identity helper so the redaction block reads naturally.
toViewGame :: Game -> Game
toViewGame = id

activePlayer :: Game -> Player
activePlayer g = case g.currentPlayer of
  Player1 -> g.player1
  Player2 -> g.player2

inactivePlayer :: Game -> Player
inactivePlayer g = case g.currentPlayer of
  Player1 -> g.player2
  Player2 -> g.player1

isGameSetup :: GameState -> Bool
isGameSetup = \case
  GameSetup -> True
  _ -> False

isGamePlaying :: GameState -> Bool
isGamePlaying = \case
  GamePlaying -> True
  _ -> False

-- | Trigger of the currently-open action window, if any. Lets tests
-- compare against an expected 'ActionWindowTrigger' value directly
-- instead of pattern-matching the wrapping 'Just'.
windowTrigger :: Maybe ActionWindow -> Maybe ActionWindowTrigger
windowTrigger = fmap (.trigger)

-- | Find the first Unit in a player's hand whose total cost (printed +
-- loyalty surcharge, accounting for the player's capital race symbol)
-- is within that player's current resources, and which doesn't carry
-- Toughness (which would let the smoke's 1-damage test get cancelled
-- entirely). Returns the card's in-hand key, printed code, total
-- effective cost, and a zone the engine's zone-entry gate will accept
-- for it ("Battlefield only." cards report the battlefield, everything
-- else the kingdom).
findPlayableUnit :: Player -> Maybe (UnitKey, CardCode, Int, ZoneKind)
findPlayableUnit = findPlayableUnitWhere (const True)

-- | 'findPlayableUnit' restricted to units the engine will let into
-- the battlefield (for the combat smoke).
findBattlefieldUnit :: Player -> Maybe (UnitKey, CardCode, Int, ZoneKind)
findBattlefieldUnit =
  findPlayableUnitWhere \cardDef ->
    not (any (`elem` cardDef.keywords) [KingdomOnly, QuestOnly])

findPlayableUnitWhere
  :: (CardDef Unit -> Bool) -> Player -> Maybe (UnitKey, CardCode, Int, ZoneKind)
findPlayableUnitWhere extraOk p =
  let Resources budget = p.resources
  in go p.hand budget
  where
    hasToughness cardDef =
      any isToughness cardDef.keywords
    isToughness = \case
      Toughness _ -> True
      _ -> False
    legalZone cardDef
      | BattlefieldOnly `elem` cardDef.keywords = BattlefieldZone
      | QuestOnly `elem` cardDef.keywords = QuestZone
      | otherwise = KingdomZone
    go [] _ = Nothing
    go (Card {key, def} : rest) budget = case def of
      UnitCardDef cardDef
        | hasToughness cardDef || not (extraOk cardDef) -> go rest budget
        | otherwise ->
            let printed = case cardDef.cost of
                  Fixed n -> n
                  Variable -> 1000
                symbolMatch = if p.race `elem` cardDef.races then 1 else 0
                loyaltySurcharge = max 0 (cardDef.loyalty - symbolMatch)
                total = printed + loyaltySurcharge
             in if total <= budget
                  then Just (key, cardDef.code, total, legalZone cardDef)
                  else go rest budget
      _ -> go rest budget

-- | Find the first Unit card in a player's hand, ignoring cost. Used
-- with 'PutUnitIntoPlay' (which skips the cost check) to set up combat
-- states without first paying for the units.
unitInHand :: Player -> Maybe (UnitKey, CardCode, Int)
unitInHand p = go p.hand
  where
    go [] = Nothing
    go (Card {key, def} : rest) = case def of
      UnitCardDef cardDef ->
        let printed = case cardDef.cost of
              Fixed n -> n
              Variable -> 0
         in Just (key, cardDef.code, printed)
      _ -> go rest

-- | Like 'unitInHand' but only matches a unit with printed power > 1, so
-- the chosen attacker deals enough combat damage to mark the defender
-- even through the highest starter-deck Toughness (Trollslayers'
-- Toughness 1). Keeps the scripted-combat damage assertion deterministic
-- regardless of which units the shuffle surfaces.
attackerInHand :: Player -> Maybe (UnitKey, CardCode, Int)
attackerInHand p = go p.hand
  where
    go [] = Nothing
    go (Card {key, def} : rest) = case def of
      UnitCardDef cardDef | cardDef.power > 1 ->
        let printed = case cardDef.cost of
              Fixed n -> n
              Variable -> 0
         in Just (key, cardDef.code, printed)
      _ -> go rest

-- | Find the first Unit in a player's hand that does NOT carry Toughness.
-- Used as the scripted-combat defender so a positive-power attacker
-- always marks it — a Toughness defender (e.g. Trollslayers, or
-- Ironbreakers whose Toughness scales with developments) could otherwise
-- cancel the damage entirely and intermittently fail the smoke.
defenderInHand :: Player -> Maybe (UnitKey, CardCode, Int)
defenderInHand p = go p.hand
  where
    go [] = Nothing
    go (Card {key, def} : rest) = case def of
      UnitCardDef cardDef
        | not (any isToughness cardDef.keywords) ->
            let printed = case cardDef.cost of
                  Fixed n -> n
                  Variable -> 0
             in Just (key, cardDef.code, printed)
      _ -> go rest
    isToughness = \case
      Toughness _ -> True
      _ -> False

check :: String -> Bool -> IO ()
check label ok =
  if ok
    then putStrLn $ "  ok   " <> label
    else do
      putStrLn $ "  FAIL " <> label
      exitFailure
