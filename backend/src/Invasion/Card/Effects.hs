{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Invasion.Card.Effects (module Invasion.Card.Effects) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.Writer.Strict (Writer, execWriter, tell)
import Data.Map.Strict qualified as Map
import Data.Monoid (Sum (..))
import Invasion.CardDef
import Invasion.Card.Types
import {-# SOURCE #-} Invasion.Engine (HasPromptIO (..))
import Invasion.Entity (LegendDetails (..), QuestDetails (..), SupportDetails (..), UnitDetails (..))
import Invasion.Capital
import Invasion.Game
import Invasion.Message
import Invasion.Modifier
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (HasQueue (..), push)

-- | Per-kind extractors for 'SomeCardDef'. Used by engine helpers like
-- 'takeFromPile' so a single take-from-pile loop can be parameterised
-- by which card kind it expects to find.
asUnit :: SomeCardDef -> Maybe (CardDef Unit)
asUnit = \case
  UnitCardDef cd -> Just cd
  _ -> Nothing

asSupport :: SomeCardDef -> Maybe (CardDef Support)
asSupport = \case
  SupportCardDef cd -> Just cd
  _ -> Nothing

asQuest :: SomeCardDef -> Maybe (CardDef Quest)
asQuest = \case
  QuestCardDef cd -> Just cd
  _ -> Nothing

asTactic :: SomeCardDef -> Maybe (CardDef Tactic)
asTactic = \case
  TacticCardDef cd -> Just cd
  _ -> Nothing

asLegend :: SomeCardDef -> Maybe (CardDef Legend)
asLegend = \case
  LegendCardDef cd -> Just cd
  _ -> Nothing

-- | Convenience: lift a definition to a 'Card' with the given key.
mkCard :: UnitKey -> SomeCardDef -> Card
mkCard k d = Card {key = k, def = d}

-- | "Draw a card." Hides the @Draw (Drawing StandardDraw pk)@ ceremony
-- so card bodies read like the printed text.
drawCard :: HasQueue Message m => PlayerKey -> m ()
drawCard pk = push (Draw (Drawing StandardDraw pk))

-- | "Draw a card from the bottom of your deck." (Restless Corpse.)
drawFromBottom :: HasQueue Message m => PlayerKey -> m ()
drawFromBottom pk = push (DrawFromBottom pk)

-- | "Place the top N cards of your deck facedown in this zone as
-- developments." (Spellweaver.)
placeTopAsDevelopments :: HasQueue Message m => PlayerKey -> ZoneKind -> Int -> m ()
placeTopAsDevelopments pk zone n = push (PlaceTopAsDevelopments pk zone n)

-- | "Turn this card into a facedown development." Transforms an in-play
-- unit into a development in its current zone (Slumbering Titan).
turnUnitIntoDevelopment :: HasQueue Message m => UnitKey -> m ()
turnUnitIntoDevelopment k = push (TurnUnitIntoDevelopment k)

-- | "Until the end of the phase, @watcher@ draws a card for each damage
-- dealt to (@owner@, @zone@)." (Get 'Em Ladz!)
watchZoneForDamageDraw
  :: HasQueue Message m => PlayerKey -> PlayerKey -> ZoneKind -> m ()
watchZoneForDamageDraw watcher owner zone =
  push (WatchZoneForDamageDraw watcher owner zone)

-- | "Shuffle your deck." Hides the @push (ShuffleDeck pk)@ ceremony.
shuffleDeck :: HasQueue Message m => PlayerKey -> m ()
shuffleDeck pk = push (ShuffleDeck pk)

-- | "Put a facedown development in [zone]." Bumps the development
-- counter on the named zone of the named player's capital.
addDevelopment :: HasQueue Message m => PlayerKey -> ZoneKind -> m ()
addDevelopment pk zone = push (AddDevelopment pk zone)

-- | "Deal N damage to a target unit."
dealDamage :: HasQueue Message m => UnitKey -> Int -> m ()
dealDamage k n = push (DealDamageToUnit k n)

-- | "Deal N damage to a target capital zone."
dealZoneDamage :: HasQueue Message m => PlayerKey -> ZoneKind -> Int -> m ()
dealZoneDamage pk zone n = push (DealDamageToZone pk zone n)

-- | "Heal N damage from your capital." (Volkmar the Grim, Keystone
-- Forge.)
healCapital :: HasQueue Message m => PlayerKey -> Int -> m ()
healCapital pk n = push (HealCapital pk n)

-- | "Heal N damage from a unit." (Trolls regenerate, Bloodsworn.)
healUnit :: HasQueue Message m => UnitKey -> Int -> m ()
healUnit k n = push (HealUnit k n)

-- | "Deal N uncancellable damage to a unit." (Mark of Chaos's turn-
-- start tick, Orc Shaman's self-damage.)
dealUncancellableDamage :: HasQueue Message m => UnitKey -> Int -> m ()
dealUncancellableDamage k n = push (DealDamageToUnitUncancellable k n)

-- | "Deal N damage to this unit at end of turn." Used by tactics
-- that hand out a temporary buff with a delayed bill (Berserk Fury,
-- Crush 'Em).
queueEoTDamage :: HasQueue Message m => UnitKey -> Int -> m ()
queueEoTDamage k n = push (DeferDamageToUnitUntilEoT k n)

-- | "Sacrifice this unit at end of turn." Used by effects that put a
-- unit into play (or buff it) on the condition that it leaves at the
-- end of the turn (Bray Shaman).
queueEoTSacrifice :: HasQueue Message m => UnitKey -> m ()
queueEoTSacrifice k = push (DeferSacrificeUntilEoT k)

-- | "Destroy a quest."
destroyQuest :: HasQueue Message m => UnitKey -> m ()
destroyQuest k = push (DestroyQuest k)

-- | "Discard a random card from this player's hand." Modeling the
-- engine's discard-at-random reaction (Horror of Tzeentch).
discardRandom :: HasQueue Message m => PlayerKey -> m ()
discardRandom pk = push (DiscardRandomFromHand pk)

-- | "Discard a card at random from your hand to gain resources equal to
-- the printed cost of the discarded card." (Windcatcher Prism.)
discardRandomForResources :: HasQueue Message m => PlayerKey -> m ()
discardRandomForResources pk = push (DiscardRandomForResources pk)

-- | "Cancel up to N damage assigned to a unit." (Defenders of the
-- Faith.)
cancelDamageOnUnit :: HasQueue Message m => UnitKey -> Int -> m ()
cancelDamageOnUnit k n = push (CancelAssignedDamageOnUnit k n)

-- | "Put a unit into play from hand or discard in the named zone.
-- Origin tells the engine which pile to remove from."
data PlayUnitOrigin = FromHand | FromDiscard | FromDeck

putUnitIntoPlay
  :: HasQueue Message m
  => PlayerKey -> PlayUnitOrigin -> UnitKey -> ZoneKind -> m ()
putUnitIntoPlay pk origin uk z = push $ case origin of
  FromHand -> PutUnitIntoPlay pk uk z
  FromDiscard -> PutUnitIntoPlayFromDiscard pk uk z
  FromDeck -> PutUnitIntoPlayFromDeck pk uk z

-- | "Place or remove N resource tokens on this support card."
adjustSupportTokens :: HasQueue Message m => UnitKey -> Int -> m ()
adjustSupportTokens k n = push (AdjustSupportTokens k n)

-- | "Place or remove N resource tokens on this unit." (Units can carry
-- resource tokens too — Shadowlands Hunter, the Capital-cycle tokeners.)
adjustUnitTokens :: HasQueue Message m => UnitKey -> Int -> m ()
adjustUnitTokens k n = push (AdjustUnitTokens k n)

-- | "Gain N resources." (Burying the Grudge.)
gainResources :: HasQueue Message m => PlayerKey -> Int -> m ()
gainResources pk n = push (GainResources pk n)

-- | "Move all damage from src to dst." (Stubborn Refusal, Valkia.)
moveAllDamage :: HasQueue Message m => UnitKey -> UnitKey -> m ()
moveAllDamage src dst = push (MoveAllDamage src dst)

-- | "Move up to N damage from src to dst." (Douse the Flames.)
moveDamage :: HasQueue Message m => UnitKey -> UnitKey -> Int -> m ()
moveDamage src dst n = push (MoveDamage src dst n)

-- | Relocate an in-play unit to another zone controlled by the same
-- player. No-op if the destination equals its current zone.
moveUnit :: HasQueue Message m => UnitKey -> ZoneKind -> m ()
moveUnit ukey zk = push (MoveUnit ukey zk)

-- | Bounce an in-play unit back to its owner's hand. Fires the
-- standard 'UnitLeftPlay' hook so on-leaves-play handlers see it.
returnUnitToHand :: HasQueue Message m => UnitKey -> m ()
returnUnitToHand ukey = push (ReturnUnitToHand ukey)

-- | Send the top N cards of the named player's deck to their
-- discard pile (the canonical "mill" effect — Infiltrate!).
millFromDeck :: HasQueue Message m => PlayerKey -> Int -> m ()
millFromDeck pk n = push (MillFromDeck pk n)

-- | Discard the entirety of the named player's hand. Used by
-- Will of Tzeentch / Journey to the Gate.
discardHand :: HasQueue Message m => PlayerKey -> m ()
discardHand pk = push (DiscardHand pk)

-- | Recycle the top N of the named player's discard pile back into
-- their deck and shuffle.
recycleDiscard :: HasQueue Message m => PlayerKey -> Int -> m ()
recycleDiscard pk n = push (RecycleDiscard pk n)

-- | "Return the named cards from your discard pile to your hand."
-- (Gift of Life.)
returnFromDiscardToHand :: HasQueue Message m => PlayerKey -> [UnitKey] -> m ()
returnFromDiscardToHand pk keys = push (ReturnCardsFromDiscardToHand pk keys)

-- | Move one development from @from@ to @to@ in this player's capital.
moveDevelopment :: HasQueue Message m => PlayerKey -> ZoneKind -> ZoneKind -> m ()
moveDevelopment pk fromZ toZ = push (MoveDevelopment pk fromZ toZ)

-- | "Player takes N indirect damage." Engine prompts the targeted
-- player to place each point one at a time, respecting the
-- HP-vs-slack cap and skipping burned zones.
indirectDamage :: HasQueue Message m => PlayerKey -> Int -> m ()
indirectDamage pk n = push (IndirectDamage pk n)

-- | Trigger an off-phase attack, e.g. "Wolves of the North". The
-- engine runs the full 5-step combat ladder regardless of the
-- current phase: the per-card hooks ('when this unit attacks',
-- Counterstrike, Scout, post-damage triggers) all fire normally.
-- The attacker key list is filtered by 'eligibleAttacker' on
-- 'BeginCombat', so a non-battlefield or corrupted unit is silently
-- dropped from the attack rather than throwing.
triggerOffPhaseAttack
  :: HasQueue Message m
  => PlayerKey     -- ^ attacker
  -> ZoneKind      -- ^ defender's target zone
  -> [UnitKey]     -- ^ attacking unit keys
  -> m ()
triggerOffPhaseAttack pk zone attackers =
  push (BeginCombat pk zone attackers)

-- | Sigmar's Intervention: redirect the in-flight attack to a different
-- zone of the defender's capital.
redirectAttackZone :: HasQueue Message m => ZoneKind -> m ()
redirectAttackZone zk = push (RedirectAttackZone zk)

-- | "Cancel the current attack." Ends the combat immediately with no
-- damage and no post-combat effects (Test of Will, Fulminating Cage).
-- Safe to call when no combat is in progress (no-op).
cancelAttack :: HasQueue Message m => m ()
cancelAttack = push CancelAttack

-- | "That player cannot declare another attack this turn." (Fulminating
-- Cage.) Persists until the player's next 'BeginTurn'.
blockAttacksThisTurn :: HasQueue Message m => PlayerKey -> m ()
blockAttacksThisTurn pk = push (BlockAttacksThisTurn pk)

-- | Pop one development from the named player's zone. Discards the
-- facedown card to that player's pile.
destroyDevelopment :: HasQueue Message m => PlayerKey -> ZoneKind -> m ()
destroyDevelopment pk zk = push (DestroyDevelopment pk zk)

-- | Flip a facedown development. If a unit, put it into play in the
-- zone and queue an end-of-turn sacrifice; otherwise discard.
flipDevelopment :: HasQueue Message m => PlayerKey -> ZoneKind -> m ()
flipDevelopment pk zk = push (FlipDevelopment pk zk)

-- | "Place / remove N resource tokens on this quest."
addQuestToken :: HasQueue Message m => UnitKey -> Int -> m ()
addQuestToken k n = push (AdjustQuestTokens k n)

-- | "Destroy a unit."
destroyUnit :: HasQueue Message m => UnitKey -> m ()
destroyUnit k = push (DestroyUnit k)

-- | "Destroy a support card or development."
destroySupport :: HasQueue Message m => UnitKey -> m ()
destroySupport k = push (DestroySupport k)

-- | "Corrupt a target unit."
corrupt :: HasQueue Message m => UnitKey -> m ()
corrupt k = push (CorruptUnit k)

-- | "Spend N resources."
payResources :: HasQueue Message m => PlayerKey -> Int -> m ()
payResources pk n = push (SpendResources pk n)

-- | "Draw N cards."
drawCards :: HasQueue Message m => PlayerKey -> Int -> m ()
drawCards pk n = replicateM_ n (drawCard pk)

-- | "Deal N damage to each enemy unit (of the given controller) in
-- this zone."
damageEachEnemyInZone
  :: HasQueue Message m => PlayerKey -> ZoneKind -> Int -> m ()
damageEachEnemyInZone pk z n =
  push (DealDamageToEachEnemyUnitInZone pk z n)

-- | "Deal N damage to each attacking and each defending unit."
damageEachUnitInCombat :: HasQueue Message m => Int -> m ()
damageEachUnitInCombat n = push (DealDamageToEachUnitInCombat n)

-- | "Attach <departing card> facedown to this unit as an experience."
-- Mirror of 'destroyUnit' for Skulltaker-style trigger bodies.
attachExperience :: HasQueue Message m => UnitKey -> CardCode -> m ()
attachExperience hostKey code =
  push (AttachExperience hostKey code)

-- ---------------------------------------------------------------------
-- Composite prompt helpers
--
-- These wrap multi-step prompt + effect patterns that recur across
-- many cards (pay-to-do-X, reveal-from-hand-then-do-X, "each player
-- must …", forced sacrifice). Each is a thin shell over 'askPrompt'
-- so card bodies can state intent in one line.
-- ---------------------------------------------------------------------

-- | "You may spend N resources to do X." Gates on actually having N
-- resources, asks a yes/no, debits on confirm, then runs the body.
--
-- > mayPay self.controller 1 "Spend 1 resource to attach this unit?" do
-- >   attachExperience self.key code
mayPay
  :: (HasGame m, HasPromptIO m, HasQueue Message m)
  => PlayerKey -> Int -> Text -> m () -> m ()
mayPay pk n prompt body = do
  me <- playerOf pk <$> getGame
  let Resources r = me.resources
  when (r >= n) $
    may pk prompt do
      payResources pk n
      body

-- | "Reveal a card matching this predicate from your hand to do X."
-- Skips silently if the player has no matching card. The reveal is
-- modeled today as a 1-card pick from the matching subset; the body
-- receives the revealed 'Card' so cards that care about its identity
-- (e.g. its name in a log message) can access it.
--
-- > revealFromHand self.controller chaosCard
-- >   "Reveal a Chaos legend or unit to deal 2 damage." \_revealed ->
-- >     withTarget self.controller AnyUnit \k -> dealDamage k 2
revealFromHand
  :: (HasGame m, HasPromptIO m)
  => PlayerKey -> (Card -> Bool) -> Text -> (Card -> m ()) -> m ()
revealFromHand pk pred desc body = do
  me <- playerOf pk <$> getGame
  let candidates = filter pred me.hand
  unless (null candidates) $
    chooseFromCards pk 0 1 candidates desc \chosen ->
      for_ chosen body

-- | "Each player does X." Runs the body once for each 'PlayerKey' in
-- turn-priority order (Player1 first), so prompts emitted by the body
-- arrive deterministically.
eachPlayer :: Applicative f => (PlayerKey -> f ()) -> f ()
eachPlayer body = body Player1 *> body Player2

-- | "Sacrifice one of your units in this zone." Forced (non-optional)
-- prompt; if the player has no unit there the prompt resolves with no
-- pick and the effect is silently a no-op (matching the rulebook:
-- "if able"). Used by Bloodthirster, Zhufbar Engineers.
mustSacrificeInZone
  :: (HasGame m, HasPromptIO m, HasQueue Message m)
  => PlayerKey -> ZoneKind -> Text -> m ()
mustSacrificeInZone pk zone desc = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseSacrifice { zone, optional = False, description = desc }
    , callback = CallbackInlinePrompt
    }
  case answer of
    PickUnits (chosen : _) ->
      withUnit chosen \u ->
        when (u.controller == pk) $ destroyUnit chosen
    _ -> pure ()

instance HasRaces UnitDetails where
  racesOf u = u.cardDef.races

instance HasRaces SupportDetails where
  racesOf s = s.cardDef.races

instance HasRaces QuestDetails where
  racesOf q = q.cardDef.races

instance HasRaces LegendDetails where
  racesOf l = l.cardDef.races

instance HasRaces DepartedUnit where
  racesOf du = du.cardDef.races

-- | "Choose a number between lo and hi." Returns the picked amount,
-- falling back to @lo@ on a declined / out-of-range answer. Skips the
-- prompt entirely when there's nothing to choose (@lo == hi@).
--
-- > n <- chooseAmount pk 1 2 "Move how many developments?"
chooseAmount
  :: (HasGame m, HasPromptIO m)
  => PlayerKey -> Int -> Int -> Text -> m Int
chooseAmount pk lo hi desc
  | hi <= lo = pure lo
  | otherwise = do
      ans <- askPrompt Prompt
        { player = pk
        , kind = ChooseAmount {minAmount = lo, maxAmount = hi, description = desc}
        , callback = CallbackInlinePrompt
        }
      pure case ans of
        PickAmount k | k >= lo && k <= hi -> k
        _ -> lo

-- | "May …" — yes/no prompt that gates an effect. Mirrors card text
-- "You may put that card into this zone."
--
-- > may pk "Put that card into this zone?" $
-- >   playSupportFromDeck pk card.key zone
may
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> Text -> m () -> m ()
may pk prompt action = do
  yes <- askYesNo pk prompt
  when yes action

-- | "Sacrifice one of your units." Prompts the firing player for a
-- unit they control, destroys it, and runs the continuation with
-- the sacrificed unit's key. The continuation is skipped if no
-- valid sacrifice is chosen.
--
-- > sacrificeOwnUnit pk "Sacrifice a unit." \_sacrificed -> ...
sacrificeOwnUnit
  :: (HasPromptIO m, HasGame m, HasQueue Message m)
  => PlayerKey -> Text -> (UnitKey -> m ()) -> m ()
sacrificeOwnUnit pk desc cont = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseUnits
        { filterSpec = AnyOwnUnit
        , minPick = 1
        , maxPick = 1
        , description = desc
        }
    , callback = CallbackInlinePrompt
    }
  case answer of
    PickUnits (chosen : _) -> do
      destroyUnit chosen
      cont chosen
    _ -> pure ()

-- | "Pick exactly one of these units" — a FORCED choice (the player
-- cannot decline while candidates exist). Used by effects whose text
-- compels a pick: "he must return one of his attacking units"
-- (Tyriel), "destroy a unit that does not share …" (Zealot Hunter),
-- "the unit with the lowest printed cost must be sacrificed" (Easy
-- Pickin's). Silently a no-op with no candidates.
forcePickUnit
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> [UnitKey] -> Text -> (UnitKey -> m ()) -> m ()
forcePickUnit _ [] _ _ = pure ()
forcePickUnit pk candidates desc k = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseUnits
        { filterSpec = UnitsFromList candidates
        , minPick = 1
        , maxPick = 1
        , description = desc
        }
    , callback = CallbackInlinePrompt
    }
  case answer of
    PickUnits (chosen : _) | chosen `elem` candidates -> k chosen
    -- Declined / illegal answer on a forced pick: fall back to the
    -- first candidate so the printed "must" still resolves.
    _ -> k (head candidates)

-- | @hasTrait t x@ — does the in-play card (or hand card def) carry
-- the printed trait? Works across kinds via the @cardDef@ field.
hasTrait
  :: forall k a. HasField "cardDef" a (CardDef k)
  => Trait -> a -> Bool
hasTrait t x = t `elem` x.cardDef.traits

-- | "Pick up to N from these specific units." Fires a prompt
-- restricted to the supplied candidate list (typically computed by
-- the card — e.g. "the attacking units" / "the units in this
-- zone"). Player may pick 0..N; the continuation runs with whatever
-- they chose (possibly empty).
--
-- > chooseUpTo pk 2 cs.attackers \chosen ->
-- >   traverse_ destroyUnit chosen
chooseUpTo
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> Int -> [UnitKey] -> ([UnitKey] -> m ()) -> m ()
chooseUpTo pk maxN candidates k = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseUnits
        { filterSpec = UnitsFromList candidates
        , minPick = 0
        , maxPick = maxN
        , description =
            "Choose up to " <> tshow maxN <> " unit"
              <> (if maxN == 1 then "" else "s") <> "."
        }
    , callback = CallbackInlinePrompt
    }
  case answer of
    PickUnits chosen -> k chosen
    _ -> k []

-- | "Pick between min and max cards from this list." Used when the
-- candidate set is a list of cards (not in-play unit keys) — e.g.
-- "search the top five cards of your deck for a support card with
-- cost 2 or lower". The engine embeds the actual card data in the
-- prompt so the prompted player's client can render the choices.
--
-- > chooseFromCards pk 0 1 matches "Pick a support to put into play."
-- >   \chosen -> for_ chosen \c -> playSupportFromDeck pk c.key zone
chooseFromCards
  :: (HasPromptIO m, HasGame m)
  => PlayerKey
  -> Int
  -> Int
  -> [Card]
  -> Text
  -> ([Card] -> m ())
  -> m ()
chooseFromCards pk minN maxN cards desc k = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseFromCards
        { cards
        , minPick = minN
        , maxPick = maxN
        , description = desc
        }
    , callback = CallbackInlinePrompt
    }
  let chosen = case answer of
        PickUnits keys -> [c | c <- cards, c.key `elem` keys]
        _ -> []
  k chosen

-- | "Discard a card from your hand with X loyalty to …" Prompts the
-- player to discard exactly one card from hand, then runs the body
-- with that card's printed loyalty as X. The whole-line idiom shared
-- by Storm of Change, Inflame, Snotling Ambush, Leave No Trace, Call
-- of the Kraken, and Doubling of the Guard. Pair with a
-- 'playableWhen' that requires a non-empty hand.
discardForLoyalty
  :: (HasGame m, HasQueue Message m, HasPromptIO m)
  => PlayerKey -> (Int -> m ()) -> m ()
discardForLoyalty pk body = do
  me <- playerOf pk <$> getGame
  chooseFromCards pk 1 1 me.hand "Discard a card from your hand for its loyalty (X)." \case
    [c] -> do
      push (DiscardCardsFromHand pk [c.key])
      body (someCardLoyalty c.def)
    _ -> pure ()

-- ---------------------------------------------------------------------
-- "Then" chains
--
-- Card text often uses "Then" to chain effects: "Effect A. Then, effect
-- B." Per the rules, B fires only if A resolved successfully. For
-- simple cases the preceding step returns 'Bool' and 'then_' gates
-- the chain. Step verbs that own their own block (like
-- 'searchTopOfDeck') gate the body on resolution internally — the
-- body itself is the "Then" chain.
-- ---------------------------------------------------------------------

-- | "Then, …" chain. Fires the chained body only if the preceding
-- step reports success ('True').
then_ :: Applicative f => Bool -> f () -> f ()
then_ = when

-- ---------------------------------------------------------------------
-- Searching the deck
--
-- 'searchTopOfDeck' runs a callback over the top N cards of a
-- player's deck. The callback receives a 'SearchResult' handle
-- exposing the cards. The body runs only if the search itself
-- resolved (i.e. no interrupt prevented it), so anything written
-- after the find verbs IS the card text's "Then" chain.
-- ---------------------------------------------------------------------

-- | Handle threaded through a 'searchTopOfDeck' block. Today carries
-- just the cards looked at; future fields can carry depth searched,
-- which matched, etc.
data SearchResult = SearchResult
  { cards :: [Card]
  }

-- | "On finding a support card matching the predicate, run the
-- callback." Looks at the supplied card list (typically
-- @result.cards@) and fires 'action' with the first match. Does
-- nothing if no match exists.
onFindSupport
  :: Monad m
  => (CardDef Support -> Bool)
  -> [Card]
  -> (Card -> m ())
  -> m ()
onFindSupport pred cards action =
  whenJust (pickSupportFrom cards pred) action

-- | "Search the top N cards of pk's deck." Runs the body with a
-- 'SearchResult' handle. The body fires only if the search resolved
-- (today: always; future: gated on interrupt state once we model
-- "cannot search" effects), so any statement after the find verbs
-- inside the body is the card text's "Then …" chain.
--
-- The depth is widened by in-play 'searchDepthBonus' supports the
-- searching player controls (Scout Camp: +1 per copy).
searchTopOfDeck
  :: HasGame m
  => PlayerKey -> Int -> (SearchResult -> m ()) -> m ()
searchTopOfDeck pk n body = do
  let resolved = True  -- TODO: gate on interrupt state once we have it
  when resolved $ do
    g <- getGame
    let bonus =
          sum
            [ s.cardDef.extras.searchDepthBonus g s pk
            | s <- g.supports
            ]
    player <- getPlayer pk
    body SearchResult {cards = take (n + bonus) player.deck}

-- | "Reveal the top N cards of your deck, then act on them." Surfaces
-- the cards to both players (records them in 'Game.lastRevealed' for the
-- UI) and runs @body@ with the same cards. The cards are NOT removed —
-- the body decides what happens next (shuffle, put into play, leave on
-- top, move to bottom, …), exactly like 'searchTopOfDeck' but public.
revealTopOfDeck
  :: (HasGame m, HasQueue Message m)
  => PlayerKey -> Int -> (SearchResult -> m ()) -> m ()
revealTopOfDeck pk n body =
  searchTopOfDeck pk n \result -> do
    push (RevealCards pk result.cards)
    body result

-- | "Put the top N cards of your deck on the bottom." (Comet of
-- Casandora.)
moveTopToBottomOfDeck :: HasQueue Message m => PlayerKey -> Int -> m ()
moveTopToBottomOfDeck pk n = push (MoveTopToBottomOfDeck pk n)

-- | "Return these cards to the top (and these to the bottom) of your
-- deck in this order." Drives the scry "in any order" effects after the
-- player has chosen an order (Scroll of Asur, Advanced Engineering).
arrangeDeckCards
  :: HasQueue Message m => PlayerKey -> [UnitKey] -> [UnitKey] -> m ()
arrangeDeckCards pk top bot = push (ArrangeDeckCards pk top bot)

-- | "Put these cards back in any order (top first)." Asks the player to
-- order @cards@ one at a time via repeated single-card picks — the first
-- pick becomes the new top — then runs @k@ with the chosen ordering of
-- keys. Reuses 'chooseFromCards' so it needs no dedicated ordering
-- prompt. A single remaining card is placed without a redundant prompt.
chooseOrdering
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> [Card] -> Text -> ([UnitKey] -> m ()) -> m ()
chooseOrdering pk cards desc k = go cards []
  where
    go [] acc = k (reverse acc)
    go [c] acc = k (reverse (c.key : acc))
    go remaining acc =
      chooseFromCards pk 1 1 remaining desc \case
        (c : _) -> go [x | x <- remaining, x.key /= c.key] (c.key : acc)
        [] -> k (reverse acc <> map (.key) remaining)

-- | "Put that support card into play [in the given zone]." Plays the
-- named support directly from the player's deck.
playSupportFromDeck
  :: HasQueue Message m
  => PlayerKey -> UnitKey -> ZoneKind -> m ()
playSupportFromDeck pk key zone = push (PlaySupportFromDeck pk key zone)

-- | "Ask the named player a yes/no question and return their answer."
-- Defaults to 'False' if no client is attached (test / debug paths).
askYesNo
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> Text -> m Bool
askYesNo pk description = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseYesNo {description}
    , callback = CallbackInlinePrompt
    }
  pure $ case answer of
    PickBool True -> True
    _ -> False

-- | Predicate over a card definition: does its printed cost meet the
-- given cap? 'Variable' costs are treated as failing the predicate
-- (no card in the core set uses Variable on support, but be safe).
costAtMost :: Int -> CardDef k -> Bool
costAtMost n cd = case cd.cost of
  Fixed v -> v <= n
  _ -> False

-- | "Pick a support card from this list matching a predicate." Used
-- when searching the top of a deck (or any card list) for a support.
-- Returns the first match, or 'Nothing'.
pickSupportFrom :: [Card] -> (CardDef Support -> Bool) -> Maybe Card
pickSupportFrom cs p = listToMaybe (filterSupportsIn cs p)

-- | All supports in the list matching a predicate over their card
-- definition. Used when the player should pick from every matching
-- candidate (e.g. "search ... for a support card with cost 2 or
-- lower").
filterSupportsIn :: [Card] -> (CardDef Support -> Bool) -> [Card]
filterSupportsIn cs p =
  [ c
  | c <- cs
  , Just cd <- [asSupport c.def]
  , p cd
  ]

-- | A modifier declaration that's not yet been installed. Produced by
-- verbs like 'buffPower' and consumed by 'until', so card bodies read
-- like the printed text:
--
-- > until EndOfTurn $ buffPower target 1
data PendingBuff = PendingBuff UnitKey ModifierDetails

-- | "Target unit gains +N power." Produces a 'PendingBuff' that's
-- installed by wrapping with 'until' to pick a scope.
buffPower :: UnitKey -> Int -> PendingBuff
buffPower target n = PendingBuff target (GainPower n)

-- | "Target unit gets -N hit points." Negative HP is clamped to 1
-- in the engine. Vile Sorceress, Horrific Mutation.
debuffHP :: UnitKey -> Int -> PendingBuff
debuffHP target n = PendingBuff target (LoseHitPoints n)

-- | "Target unit gets +N hit points." We Need Your Blood.
buffHP :: UnitKey -> Int -> PendingBuff
buffHP target n = PendingBuff target (GainHitPoints n)

-- | "Cancel the next N damage that would be dealt to target unit."
-- Steel's Bane.
damageShield :: UnitKey -> Int -> PendingBuff
damageShield target n = PendingBuff target (DamageShield n)

-- | "The next N damage dealt to [target] are redirected to [dst]."
-- Blessing of Valaya.
redirectNextDamage :: UnitKey -> Int -> UnitKey -> PendingBuff
redirectNextDamage target n dst = PendingBuff target (RedirectShield n dst)

-- | "This unit loses all Toughness." Morathi's Pegasus.
loseAllToughness :: UnitKey -> PendingBuff
loseAllToughness target = PendingBuff target LoseAllToughness

-- | "Actions targeting this unit cost an additional N resources."
-- Iron Discipline.
imposeTargetTax :: UnitKey -> Int -> PendingBuff
imposeTargetTax target n = PendingBuff target (TargetTaxBonus n)

-- | "This unit deals +N damage in combat." Naggaroth Spearmen.
buffCombatDamage :: UnitKey -> Int -> PendingBuff
buffCombatDamage target n = PendingBuff target (GainCombatDamage n)

-- | "Target unit gains Toughness N." Produces a 'PendingBuff' folded
-- into 'totalToughness' for the buff's scope (Fearless in Battle).
buffToughness :: UnitKey -> Int -> PendingBuff
buffToughness target n = PendingBuff target (GainToughness n)

-- | "Target unit gains Counterstrike N." Folded into
-- 'totalCounterstrike' for the buff's scope (Celestial Wizard Acolyte).
buffCounterstrike :: UnitKey -> Int -> PendingBuff
buffCounterstrike target n = PendingBuff target (GainCounterstrike n)

-- | "Target unit must defend this turn, if able." Animosity,
-- Alluring Daemonettes.
mustDefend :: UnitKey -> PendingBuff
mustDefend target = PendingBuff target MustDefend

-- | "This card cannot be targeted by card effects." @opponentOnly@ True
-- blocks only the opponent (Shield of Saphery, Ghostly Apparition);
-- False blocks every player (the self-protecting attachments). Wrap with
-- 'until' for the duration ('EndOfTurn' for the tactics, 'Permanent' for
-- attachments).
untargetable :: Bool -> UnitKey -> PendingBuff
untargetable opponentOnly target = PendingBuff target (CannotBeTargeted opponentOnly)

-- | "Target unit cannot attack." Franz's Decree.
disableAttack :: UnitKey -> PendingBuff
disableAttack target = PendingBuff target CannotAttack

-- | "Target unit cannot defend." Franz's Decree.
disableDefend :: UnitKey -> PendingBuff
disableDefend target = PendingBuff target CannotDefend

-- | "Target unit cannot be corrupted." Blessing of Isha.
shieldFromCorruption :: UnitKey -> PendingBuff
shieldFromCorruption target = PendingBuff target CannotBeCorrupted

-- | Install a 'PendingBuff' for the named 'ModifierScope'. Mirrors the
-- card text "until end of turn" / "while X is in play".
--
-- > until EndOfTurn $ buffPower target 1
until :: HasQueue Message m => ModifierScope -> PendingBuff -> m ()
until scope (PendingBuff target details) =
  push (InstallModifier (UnitRef target) (Modifier details scope))

-- | Look up an in-play unit by its 'UnitKey'. Mirror of the Engine-side
-- helper so card receive bodies can resolve their attachment hosts
-- without importing 'Invasion.Engine'.
findUnit :: UnitKey -> Game -> Maybe UnitDetails
findUnit k g = find ((== k) . (.key)) g.units

-- | Continuation-style unit lookup. Mirror of 'withQuest'.
withUnit
  :: HasGame m => UnitKey -> (UnitDetails -> m ()) -> m ()
withUnit k k' = do
  g <- getGame
  whenJust (findUnit k g) k'

-- | Look up an in-play free-standing support by key.
findSupport :: UnitKey -> Game -> Maybe SupportDetails
findSupport k g = find ((== k) . (.key)) g.supports

-- | Continuation-style support lookup. Mirror of 'withQuest'.
withSupport
  :: HasGame m => UnitKey -> (SupportDetails -> m ()) -> m ()
withSupport k k' = do
  g <- getGame
  whenJust (findSupport k g) k'

-- | Look up an in-play quest by key. Pure form for use inside list
-- comprehensions, guards, and other pure contexts.
findQuest :: UnitKey -> Game -> Maybe QuestDetails
findQuest k g = find ((== k) . (.key)) g.quests

-- | Continuation-style quest lookup. Runs the body with the quest
-- record if it exists; skips silently otherwise. Reads game state
-- from the surrounding monad so card bodies don't need an explicit
-- @g <- getGame@.
--
-- > withQuest self.key \q ->
-- >   when (isJust q.questingUnit) $ addQuestToken self.key 1
withQuest
  :: HasGame m => UnitKey -> (QuestDetails -> m ()) -> m ()
withQuest k k' = do
  g <- getGame
  whenJust (findQuest k g) k'

-- | Look up an in-play legend by key.
findLegend :: UnitKey -> Game -> Maybe LegendDetails
findLegend k g = find ((== k) . (.key)) g.legends

-- | Continuation-style legend lookup. Mirror of 'withQuest'.
withLegend
  :: HasGame m => UnitKey -> (LegendDetails -> m ()) -> m ()
withLegend k k' = do
  g <- getGame
  whenJust (findLegend k g) k'

-- | All in-play units sitting in the given zone. Reads live game
-- state via 'HasGame'.
unitsInZone :: HasGame m => ZoneKind -> m [UnitDetails]
unitsInZone z = do
  g <- getGame
  pure [u | u <- g.units, u.zone == z]

-- ---------------------------------------------------------------------
-- Playability helpers
--
-- Boolean predicates over 'Game' + 'PlayerKey' that mirror the
-- "Action: …" preconditions printed on tactic cards. Used with
-- 'playableWhen' so the engine refuses to play a tactic that can't
-- meaningfully resolve (no valid target, wrong phase, …).
-- ---------------------------------------------------------------------

-- | Has this unit taken any damage?
isDamaged :: UnitDetails -> Bool
isDamaged u = let Damage d = u.damage in d > 0

-- | Other units sitting in the same zone as the given unit (the unit
-- itself is excluded).
peersInZoneOf :: Game -> UnitDetails -> [UnitDetails]
peersInZoneOf g u = [v | v <- g.units, v.zone == u.zone, v.key /= u.key]

-- | Has the given unit at least one peer in its zone?
hasPeerInZone :: Game -> UnitDetails -> Bool
hasPeerInZone g u = not (null (peersInZoneOf g u))

-- | Is there a combat in progress?
inCombat :: Game -> PlayerKey -> Bool
inCombat g _pk = isJust g.combat

-- | Is there a combat in progress with at least one combatant on this
-- player's side? Attackers if they're attacking, defenders if
-- they're defending.
hasFriendlyCombatant :: Game -> PlayerKey -> Bool
hasFriendlyCombatant g pk = case g.combat of
  Just cs
    | cs.attackingPlayer == pk -> not (null cs.attackers)
    | cs.defendingPlayer == pk -> not (null cs.defenders)
  _ -> False

-- | Is there a combat in progress where the opponent is attacking and
-- has at least one attacker on the board?
hasEnemyAttacker :: Game -> PlayerKey -> Bool
hasEnemyAttacker g pk = case g.combat of
  Just cs | cs.attackingPlayer /= pk -> not (null cs.attackers)
  _ -> False

-- | Does the opponent control any support card in play?
hasEnemySupport :: Game -> PlayerKey -> Bool
hasEnemySupport g pk = any (\s -> s.controller /= pk) g.supports

-- | Does the player's deck have at least N cards?
hasDeckSize :: Int -> Game -> PlayerKey -> Bool
hasDeckSize n g pk = length (playerOf pk g).deck >= n

-- | "Does this player control any in-play card whose printed races
-- don't include @r@?" Backs the mono-faction watchtowers (Chill Sea
-- Watchtower, Outlying Tower), which sacrifice themselves the moment
-- their controller fields an off-faction card. Neutral cards (no race)
-- count as off-faction, matching the printed "non-[Race] card" wording.
controlsNonRaceCard :: Game -> PlayerKey -> Race -> Bool
controlsNonRaceCard g pk r =
  has g.units || has g.supports || has g.quests || has g.legends
  where
    has
      :: ( HasField "controller" a PlayerKey
         , HasField "cardDef" a (CardDef k)
         )
      => [a] -> Bool
    has = any \x -> x.controller == pk && r `notElem` x.cardDef.races

-- | "Does this player control a faceup non-[Race] unit or support
-- card?" The narrower cousin of 'controlsNonRaceCard' used by Mob O'
-- Hutz, whose printed condition only counts units and supports (not
-- quests/legends). In-play units and supports are always faceup, so no
-- separate facedown check is needed.
controlsNonRaceUnitOrSupport :: Game -> PlayerKey -> Race -> Bool
controlsNonRaceUnitOrSupport g pk r = has g.units || has g.supports
  where
    has
      :: ( HasField "controller" a PlayerKey
         , HasField "cardDef" a (CardDef k)
         )
      => [a] -> Bool
    has = any \x -> x.controller == pk && r `notElem` x.cardDef.races

-- | Does the player have at least one non-burned development zone
-- (kingdom or battlefield)?
canDevelop :: Game -> PlayerKey -> Bool
canDevelop g pk =
  let me = playerOf pk g
   in not me.capital.kingdom.burned || not me.capital.battlefield.burned

-- | "If a combat is in progress, run this body with the combat
-- state." Convenience wrapper that hides the @g <- getGame; for_
-- g.combat@ boilerplate.
--
-- > withCombat \cs ->
-- >   when (cs.attackingPlayer /= pk) $
-- >     traverse_ destroyUnit (take 2 cs.attackers)
withCombat :: HasGame m => (CombatState -> m ()) -> m ()
withCombat k = do
  g <- getGame
  for_ g.combat k

-- | The legend currently in play for the given player, if any. Each
-- player may control at most one legend at a time.
legendOf :: PlayerKey -> Game -> Maybe LegendDetails
legendOf pk g = find ((== pk) . (.controller)) g.legends

-- | Total number of currently-burning zones across both capitals. Used
-- by Chaos cards that scale with burning (Bloodcrusher, Lord of Khorne,
-- Rift of Chaos, Durgnar).
burningZoneCount :: Game -> Int
burningZoneCount g =
  length
    [ ()
    | p <- [g.player1, g.player2]
    , z <- p.capital.zones
    , z.burning
    ]

-- | Number of facedown developments in the named unit's zone. Read by
-- Toughness X and a couple of dev-scaling cards (Troll Slayers,
-- Ironbreakers of Ankhor).
devsInZone :: Game -> UnitDetails -> Int
devsInZone g u =
  let player = case u.controller of
        Player1 -> g.player1
        Player2 -> g.player2
      Developments d = case u.zone of
        KingdomZone -> player.capital.kingdom.developments
        QuestZone -> player.capital.quest.developments
        BattlefieldZone -> player.capital.battlefield.developments
   in d

-- | True iff any section of @pk@'s capital is currently burning.
controllerBurning :: Game -> PlayerKey -> Bool
controllerBurning g pk =
  let p = case pk of
        Player1 -> g.player1
        Player2 -> g.player2
   in any (.burning) p.capital.zones

-- | True iff the unit is currently declared as an attacker in the
-- in-flight combat. Returns False outside combat.
unitIsAttacking :: Game -> UnitDetails -> Bool
unitIsAttacking g u = maybe False (elem u.key . (.attackers)) g.combat

-- | True iff the unit is currently declared as a defender. Returns False
-- outside combat.
unitIsDefending :: Game -> UnitDetails -> Bool
unitIsDefending g u = maybe False (elem u.key . (.defenders)) g.combat

-- | True iff the unit is opposed in the current combat: an attacker
-- with at least one defender, or a defender with at least one attacker
-- (Saurus Warriors, Black Dragon Rider).
isOpposed :: Game -> UnitDetails -> Bool
isOpposed g u = case g.combat of
  Just cs
    | u.key `elem` cs.attackers -> not (null cs.defenders)
    | u.key `elem` cs.defenders -> not (null cs.attackers)
  _ -> False

-- | Every in-play support, whether free-standing in 'Game.supports' or
-- attached to a unit. Used when an effect needs to consult every
-- support regardless of attachment status.
allInPlaySupports :: Game -> [SupportDetails]
allInPlaySupports g = g.supports ++ concatMap (.attachments) g.units

-- | "the highest loyalty on a [Race] card you control." Scans every
-- in-play unit and support controlled by @pk@ that carries the race
-- symbol and returns the greatest printed loyalty (0 if none). Shared
-- by the Capital-cycle "X is the highest loyalty …" cards (Savage
-- Forsaken, Ruglud's Armoured Orcs, Runeblades, Priests of Sigmar).
highestLoyaltyControlled :: Race -> Game -> PlayerKey -> Int
highestLoyaltyControlled r g pk =
  maximum $ 0 :
    [u.cardDef.loyalty | u <- g.units, u.controller == pk, r `elem` u.cardDef.races]
    ++ [s.cardDef.loyalty | s <- allInPlaySupports g, s.controller == pk, r `elem` s.cardDef.races]

-- ---------------------------------------------------------------------
-- Static-effect builder monad
--
-- 'EffectM' wraps a Writer over 'ActiveEffect' with read access to
-- the current 'Game'. Used by zone-gated constant/effects bodies.
-- ---------------------------------------------------------------------

-- | Builder monad for static-effect declarations. Wraps a Writer over
-- 'ActiveEffect' with read access to the current 'Game'; verbs like
-- 'gainPower' emit contributions, and lookup helpers like 'zoneOf'
-- read fresh game state on demand.
newtype EffectM a = EffectM (ReaderT Game (Writer ActiveEffect) a)
  deriving newtype (Functor, Applicative, Monad)

instance HasGame EffectM where
  getGame = EffectM ask

-- | Drain an 'EffectM' block against the current game to the
-- 'ActiveEffect' it produced.
execEffectM :: Game -> EffectM () -> ActiveEffect
execEffectM g (EffectM r) = execWriter (runReaderT r g)

-- | "This unit gains +N power." Emits a power-bonus contribution
-- inside an 'EffectM' block. The 'self' argument mirrors the printed
-- card text ("This unit ...") and is not consulted internally — the
-- surrounding host already knows which unit it's evaluating.
gainPower :: UnitDetails -> Int -> EffectM ()
gainPower _self n = EffectM (tell (ActiveEffect (Sum n)))

-- | The owner of an in-play card, as a full 'Player' record.
playerOf :: PlayerKey -> Game -> Player
playerOf Player1 g = g.player1
playerOf Player2 g = g.player2

-- | Look up the current 'Zone' record for a unit (off its
-- controller's capital). Reads game state via 'HasGame' so the
-- result is always fresh.
zoneOf :: HasGame m => UnitDetails -> m Zone
zoneOf u = do
  g <- getGame
  let owner = playerOf u.controller g
  pure $ case u.zone of
    KingdomZone -> owner.capital.kingdom
    QuestZone -> owner.capital.quest
    BattlefieldZone -> owner.capital.battlefield

-- | Continuation-style 'zoneOf'. Runs the body with the unit's
-- current zone record. Mirrors 'withQuest' / 'withUnit' /
-- 'withSupport' / 'withLegend' / 'withCombat'.
--
-- > withZoneOf self \z ->
-- >   when (z.developments >= 2) $ gainPower self 2
withZoneOf
  :: HasGame m => UnitDetails -> (Zone -> m ()) -> m ()
withZoneOf u k = zoneOf u >>= k

-- | Look up the 'History' bucket for a scope (counts of units
-- discarded, attackers declared, damage taken, etc. since the
-- scope's last reset). Mirrors 'zoneOf' for zone records.
historyOf :: HasGame m => Scope -> m History
historyOf s = do
  g <- getGame
  pure (Map.findWithDefault mempty s g.history)

-- | Continuation-style 'historyOf'.
--
-- > withHistory ThisTurn \h ->
-- >   when (h.unitsDiscarded > 0) $
-- >     gainResources pk h.unitsDiscarded
withHistory
  :: HasGame m => Scope -> (History -> m ()) -> m ()
withHistory s k = historyOf s >>= k

-- | True iff any section of this player's capital is currently
-- burning. The 'Player'-shaped counterpart to 'controllerBurning'.
capitalBurning :: Player -> Bool
capitalBurning p = any (.burning) p.capital.zones

-- | Is the named section of the named player's capital currently
-- burning? Used by burning-zone-targeting effects (Embers to Inferno).
zoneBurning :: Game -> PlayerKey -> ZoneKind -> Bool
zoneBurning g pk zk =
  let p = playerOf pk g
   in case zk of
        KingdomZone -> p.capital.kingdom.burning
        QuestZone -> p.capital.quest.burning
        BattlefieldZone -> p.capital.battlefield.burning

-- ---------------------------------------------------------------------
-- Targets
--
-- 'Target a' describes what kind of pick an action needs and is the
-- typed counterpart to the wire-side 'TargetSchema'. The phantom 'a'
-- is the type returned by 'withTarget', so card bodies don't have to
-- pattern-match on a heterogeneous 'ActionTarget'.
-- ---------------------------------------------------------------------

-- | Typed action-target descriptor. Each single-variant constructor
-- pins the return type that 'withTarget' produces; 'Or' merges
-- branches into a flat 'TargetOption' so card bodies pattern-match
-- on the wire-side variant directly rather than nested 'Either'.
data Target a where
  AnyUnit :: Target UnitKey
  AnyCapital :: Target (PlayerKey, ZoneKind)
    -- ^ A capital zone. Standalone 'withTarget' enumerates every
    -- unburned zone across both players and prompts.
  MyDevZone :: Target ZoneKind
    -- ^ One of the controller's own non-burned development zones
    -- (kingdom or battlefield). Used by cards that place developments
    -- in a chosen zone (Wake the Mountain).
  MyAnyZone :: Target ZoneKind
    -- ^ Any of the controller's three zones (burned zones included
    -- in the enumeration so that move-into-burned is the engine's
    -- decision, not the picker's). Used by relocate cards
    -- (Pistoliers, Forced March, Temple of Shallya).
  AnyDevelopmentZone :: Target (PlayerKey, ZoneKind)
    -- ^ A capital zone — either player's — that currently holds
    -- at least one development. Used by destroy-development
    -- effects (Demolition!, Smash-Go-Boom!).
  EnemyDevelopmentZone :: Target (PlayerKey, ZoneKind)
    -- ^ Same as 'AnyDevelopmentZone' but restricted to the
    -- opponent's side.
  UnitMatching :: (PlayerKey -> Game -> UnitDetails -> Bool) -> Target UnitKey
    -- ^ A unit satisfying the supplied predicate. The first argument
    -- is the player making the pick (so predicates can reference
    -- "your" / "an opponent's" via @u.controller@). Use the smart
    -- constructors below (@ownUnit@, @enemyUnit@, @defendingUnit@…)
    -- for common cases.
  TargetPlayer :: Target PlayerKey
    -- ^ A player (either side). Used by "target player"/"any player's
    -- deck" effects (Caradryan, Learned Mage). The picking player is
    -- offered both players.
  AnySupportCard :: Target UnitKey
    -- ^ Any in-play support card — free-standing or attached, either
    -- controller's. Used by "destroy one target support card"
    -- effects (Demolition!).
  SupportMatching
    :: (PlayerKey -> Game -> SupportDetails -> Bool) -> Target UnitKey
    -- ^ An in-play support card (free-standing or attached)
    -- satisfying the predicate. First arg is the picking player.
    -- Used by "destroy one target Attachment card" effects (Vaul's
    -- Unmaking).
  CapitalMatching
    :: (PlayerKey -> (PlayerKey, ZoneKind) -> Bool)
    -> Target (PlayerKey, ZoneKind)
    -- ^ A capital zone satisfying the supplied predicate. Used to
    -- restrict 'AnyCapital'-style picks to e.g. opposing zones.
  Or :: Target a -> Target b -> Target TargetOption
    -- ^ "X or Y." Enumerates both branches' candidates into a
    -- single unified prompt and returns the chosen 'TargetOption'.
    -- The card body pattern-matches on the wire-side variant
    -- (e.g. @TargetUnitOption u@); the loss of compile-time
    -- exhaustiveness vs. nested 'Either' is the price of staying
    -- flat under deeper combinations.

-- | "A unit you control."
ownUnit :: Target UnitKey
ownUnit = UnitMatching \pk _ u -> u.controller == pk

-- | "A unit an opponent controls."
enemyUnit :: Target UnitKey
enemyUnit = UnitMatching \pk _ u -> u.controller /= pk

-- | "A defending unit in the current combat."
defendingUnit :: Target UnitKey
defendingUnit = UnitMatching \_pk g u -> case g.combat of
  Just cs -> u.key `elem` cs.defenders
  Nothing -> False

-- | "An attacking unit in the current combat."
attackingUnit :: Target UnitKey
attackingUnit = UnitMatching \_pk g u -> case g.combat of
  Just cs -> u.key `elem` cs.attackers
  Nothing -> False

-- | "A unit (any controller) matching this predicate."
unitWhere :: (UnitDetails -> Bool) -> Target UnitKey
unitWhere p = UnitMatching \_ _ u -> p u

-- | "An enemy unit matching this predicate."
enemyUnitWhere :: (UnitDetails -> Bool) -> Target UnitKey
enemyUnitWhere p = UnitMatching \pk _ u -> u.controller /= pk && p u

-- | "A capital zone an opponent controls (not burned)."
enemyCapital :: Target (PlayerKey, ZoneKind)
enemyCapital = CapitalMatching \pk (owner, _) -> owner /= pk

-- | Fire a target-selection prompt (or auto-resolve, depending on the
-- target shape) and run a continuation with the chosen pick. If no
-- target can be acquired, the continuation is skipped silently.
--
-- > withTarget pk AnyUnit \t -> until EndOfTurn $ buffPower t 1
withTarget
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> Target a -> (a -> m ()) -> m ()
withTarget pk t k = pickTarget pk t >>= flip whenJust k

-- | Resolve a 'Target' to its concrete pick, or 'Nothing' if the
-- player declined / no valid target exists. 'Or' enumerates every
-- branch's candidates into a single unified prompt and returns the
-- chosen 'TargetOption'; single-variant Targets unwrap the option
-- to their typed value (e.g. 'AnyUnit' → 'UnitKey').
pickTarget
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> Target a -> m (Maybe a)
pickTarget pk = \case
  AnyUnit -> do
    opts <- enumerateOptions pk AnyUnit
    promptForOption pk opts >>= \case
      Just (TargetUnitOption k) -> pure (Just k)
      _ -> pure Nothing
  AnyCapital -> do
    opts <- enumerateOptions pk AnyCapital
    promptForOption pk opts >>= \case
      Just (TargetZoneOption owner z) -> pure (Just (owner, z))
      _ -> pure Nothing
  MyDevZone -> do
    opts <- enumerateOptions pk MyDevZone
    promptForOption pk opts >>= \case
      Just (TargetZoneOption _ z) -> pure (Just z)
      _ -> pure Nothing
  MyAnyZone -> do
    opts <- enumerateOptions pk MyAnyZone
    promptForOption pk opts >>= \case
      Just (TargetZoneOption _ z) -> pure (Just z)
      _ -> pure Nothing
  AnyDevelopmentZone -> do
    opts <- enumerateOptions pk AnyDevelopmentZone
    promptForOption pk opts >>= \case
      Just (TargetZoneOption owner z) -> pure (Just (owner, z))
      _ -> pure Nothing
  EnemyDevelopmentZone -> do
    opts <- enumerateOptions pk EnemyDevelopmentZone
    promptForOption pk opts >>= \case
      Just (TargetZoneOption owner z) -> pure (Just (owner, z))
      _ -> pure Nothing
  UnitMatching p -> do
    opts <- enumerateOptions pk (UnitMatching p)
    promptForOption pk opts >>= \case
      Just (TargetUnitOption k) -> pure (Just k)
      _ -> pure Nothing
  TargetPlayer -> do
    opts <- enumerateOptions pk TargetPlayer
    promptForOption pk opts >>= \case
      Just (TargetPlayerOption p) -> pure (Just p)
      _ -> pure Nothing
  AnySupportCard -> do
    opts <- enumerateOptions pk AnySupportCard
    promptForOption pk opts >>= \case
      Just (TargetSupportOption k) -> pure (Just k)
      _ -> pure Nothing
  SupportMatching p -> do
    opts <- enumerateOptions pk (SupportMatching p)
    promptForOption pk opts >>= \case
      Just (TargetSupportOption k) -> pure (Just k)
      _ -> pure Nothing
  CapitalMatching p -> do
    opts <- enumerateOptions pk (CapitalMatching p)
    promptForOption pk opts >>= \case
      Just (TargetZoneOption owner z) -> pure (Just (owner, z))
      _ -> pure Nothing
  Or a b -> do
    aOpts <- enumerateOptions pk a
    bOpts <- enumerateOptions pk b
    promptForOption pk (aOpts <> bOpts)

-- | Fire a 'ChooseTargetOption' prompt with the supplied options.
-- Returns 'Nothing' if the option list is empty or the player
-- declines.
promptForOption
  :: (HasPromptIO m, HasGame m)
  => PlayerKey -> [TargetOption] -> m (Maybe TargetOption)
promptForOption _ [] = pure Nothing
promptForOption pk opts = do
  answer <- askPrompt Prompt
    { player = pk
    , kind = ChooseTargetOption
        { options = opts
        , description = "Choose a target."
        }
    , callback = CallbackInlinePrompt
    }
  pure $ case answer of
    PickTargetOption chosen | chosen `elem` opts -> Just chosen
    _ -> Nothing

-- | Enumerate every candidate 'TargetOption' a 'Target' offers.
-- 'Or' concatenates its branches; single-variant constructors emit
-- their natural options.
enumerateOptions
  :: HasGame m => PlayerKey -> Target a -> m [TargetOption]
enumerateOptions pk t = do
  g <- getGame
  pure (enumerateOptionsPure pk g t)

-- | Pure version of 'enumerateOptions'. Used by 'hasTarget' (so that
-- 'playableWhen \\g pk -> hasTarget t g pk' avoids running a prompt
-- just to check existence).
-- | Can player @picker@ legally target the entity with this key (whose
-- controller is @controller@) given any 'CannotBeTargeted' modifiers on
-- it? @opponentOnly@ immunity blocks only the controller's opponent;
-- otherwise it blocks everyone. Works for units and supports alike since
-- both are keyed in 'g.modifiers' by 'UnitRef'.
targetableBy :: Game -> PlayerKey -> UnitKey -> PlayerKey -> Bool
targetableBy g picker key controller =
  not (any immune (Map.findWithDefault [] (UnitRef key) g.modifiers))
    && not staticImmune
  where
    immune (Modifier (CannotBeTargeted opponentOnly) _) =
      not opponentOnly || controller /= picker
    immune _ = False
    -- Self-protecting supports (Dawnstar Sword, Helm of Fortune, …) carry
    -- their immunity as a static field rather than a modifier, so it never
    -- needs re-applying and survives across turns.
    staticImmune =
      case find (\s -> s.key == key) (allInPlaySupports g) of
        Just s -> case s.cardDef.extras.selfUntargetable g s of
          Just opponentOnly -> not opponentOnly || s.controller /= picker
          Nothing -> False
        Nothing -> False

enumerateOptionsPure :: PlayerKey -> Game -> Target a -> [TargetOption]
enumerateOptionsPure pk g = \case
  AnyUnit ->
    [TargetUnitOption u.key | u <- g.units, targetableBy g pk u.key u.controller]
  AnyCapital ->
    let zonesOf p =
          [ (p.key, z.kind)
          | z <- p.capital.zones
          , not z.burned
          ]
     in [ TargetZoneOption owner z
        | (owner, z) <- zonesOf g.player1 <> zonesOf g.player2
        ]
  MyDevZone ->
    let me = playerOf pk g
     in [ TargetZoneOption pk z.kind
        | z <- [me.capital.kingdom, me.capital.battlefield]
        , not z.burned
        ]
  MyAnyZone ->
    let me = playerOf pk g
     in [ TargetZoneOption pk z.kind
        | z <- me.capital.zones
        , not z.burned
        ]
  AnyDevelopmentZone ->
    let devsOf p =
          [ TargetZoneOption p.key z.kind
          | z <- p.capital.zones
          , let Developments d = z.developments
          , d > 0
          ]
     in devsOf g.player1 <> devsOf g.player2
  EnemyDevelopmentZone ->
    let opp = pk.next
        opponent = playerOf opp g
     in [ TargetZoneOption opp z.kind
        | z <- opponent.capital.zones
        , let Developments d = z.developments
        , d > 0
        ]
  UnitMatching p ->
    [ TargetUnitOption u.key
    | u <- g.units
    , p pk g u
    , targetableBy g pk u.key u.controller
    ]
  AnySupportCard ->
    [TargetSupportOption s.key | s <- g.supports, targetableBy g pk s.key s.controller]
      <> [ TargetSupportOption a.key
         | u <- g.units
         , a <- u.attachments
         , targetableBy g pk a.key a.controller
         ]
  TargetPlayer ->
    [TargetPlayerOption g.player1.key, TargetPlayerOption g.player2.key]
  SupportMatching p ->
    [ TargetSupportOption s.key
    | s <- allInPlaySupports g
    , p pk g s
    , targetableBy g pk s.key s.controller
    ]
  CapitalMatching p ->
    let zonesOf player =
          [ (player.key, z.kind)
          | z <- player.capital.zones
          , not z.burned
          ]
     in [ TargetZoneOption owner z
        | (owner, z) <- zonesOf g.player1 <> zonesOf g.player2
        , p pk (owner, z)
        ]
  Or a b ->
    enumerateOptionsPure pk g a <> enumerateOptionsPure pk g b

-- | "Does this target offer at least one candidate?" Use inside
-- 'playableWhen' so a tactic can verify there's something to pick
-- before resolving:
--
-- > playableWhen $ hasTarget enemyUnit
hasTarget :: Target a -> Game -> PlayerKey -> Bool
hasTarget t g pk = not (null (enumerateOptionsPure pk g t))

-- | Every unit matching a 'Target UnitKey' as 'UnitDetails'. Useful for
-- "each of your Xs" effects that need to fan out across multiple
-- units without a prompt.
--
-- > mine <- unitsMatching pk ownUnit
-- > for_ mine \u -> until EndOfTurn $ buffPower u.key 1
unitsMatching :: HasGame m => PlayerKey -> Target UnitKey -> m [UnitDetails]
unitsMatching pk t = do
  g <- getGame
  pure
    [ u
    | TargetUnitOption k <- enumerateOptionsPure pk g t
    , Just u <- [findUnit k g]
    ]

-- | "Pick up to N units matching this 'Target' and do something."
-- Combines target enumeration with 'chooseUpTo' so cards can express
-- "corrupt up to 3 target units" / "destroy up to 2 target enemy
-- attackers" in one line.
--
-- > withUpTo pk 3 (unitWhere (not . (.corrupted))) (traverse_ corrupt)
withUpTo
  :: (HasGame m, HasPromptIO m)
  => PlayerKey -> Int -> Target UnitKey -> ([UnitKey] -> m ()) -> m ()
withUpTo pk n t body = do
  candidates <- unitsMatching pk t
  chooseUpTo pk n (map (.key) candidates) body

-- | "Each of your Xs gains N power until end of turn." Buffs every
-- unit matching 'Target UnitKey' with a 'GainPower' modifier scoped
-- to end of turn.
--
-- > whenResolved \self -> buffEachUntilEoT self.controller ownOrcs 2
buffEachUntilEoT
  :: (HasGame m, HasQueue Message m)
  => PlayerKey -> Target UnitKey -> Int -> m ()
buffEachUntilEoT pk t n = do
  mine <- unitsMatching pk t
  for_ mine \u -> until EndOfTurn $ buffPower u.key n
