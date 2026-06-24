{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE NoFieldSelectors #-}

module Invasion.Card.Triggers (module Invasion.Card.Triggers) where

import Control.Monad.Reader (ReaderT, ask, runReaderT)
import Control.Monad.State.Strict (get, lift, modify)
import Invasion.Card.Builder
import Invasion.Card.Effects
import Invasion.CardDef
import {-# SOURCE #-} Invasion.Engine (HasPromptIO (..))
import Invasion.Entity (LegendDetails (..), QuestDetails (..), SupportDetails (..), TacticContext (..), UnitDetails (..))
import Invasion.Game hiding (battlefield)
import Invasion.Message
import Invasion.Player
import Invasion.Prelude
import Invasion.Types
import Queue (HasQueue (..))

-- ---------------------------------------------------------------------
-- Trigger DSL
--
-- These combinators wrap 'onReceive' with message-pattern matchers, so
-- card definitions don't have to spell out @case msg of UnitEnteredPlay
-- pk uk | pk == self.controller …@ boilerplate. Each combinator is
-- thin: it inspects the message, applies a kind-appropriate self-check,
-- and forwards to the supplied handler with the same capability set as
-- 'Receive'. Card bodies can mix and match them; 'onReceive' composes.
-- ---------------------------------------------------------------------

-- | Per-kind matcher for "this in-play card just entered play". Each
-- kind has its own 'Entered' message constructor; the class hides the
-- ceremony from card-side code.
class HasEnteredPlay (k :: CardKind) where
  matchEnteredPlay :: Message -> Maybe (PlayerKey, UnitKey)

instance HasEnteredPlay Unit where
  matchEnteredPlay = \case
    UnitEnteredPlay pk uk -> Just (pk, uk)
    _ -> Nothing

instance HasEnteredPlay Support where
  matchEnteredPlay = \case
    SupportEnteredPlay pk uk -> Just (pk, uk)
    _ -> Nothing

instance HasEnteredPlay Quest where
  matchEnteredPlay = \case
    QuestEnteredPlay pk uk -> Just (pk, uk)
    _ -> Nothing

instance HasEnteredPlay Legend where
  matchEnteredPlay = \case
    LegendEnteredPlay pk uk -> Just (pk, uk)
    _ -> Nothing

-- | Per-kind matcher for "this in-play card is being destroyed".
class HasDestroyMatch (k :: CardKind) where
  matchDestroy :: Message -> Maybe UnitKey

instance HasDestroyMatch Unit where
  matchDestroy = \case
    DestroyUnit uk -> Just uk
    _ -> Nothing

instance HasDestroyMatch Support where
  matchDestroy = \case
    DestroySupport uk -> Just uk
    _ -> Nothing

instance HasDestroyMatch Quest where
  matchDestroy = \case
    DestroyQuest uk -> Just uk
    _ -> Nothing

instance HasDestroyMatch Legend where
  matchDestroy = \case
    DestroyLegend uk -> Just uk
    _ -> Nothing

-- | Common capability set every trigger body has access to. Mirrors the
-- constraints on 'Receive' so handlers may be lifted in and out.
type TriggerM m =
  (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)

-- | "When this card enters play." Fires only for the entry message of
-- the matching kind and only when the controller and key identify this
-- specific in-play instance.
onEnterPlay
  :: forall k
   . ( HasEnteredPlay k
     , HasField "controller" (InPlay k) PlayerKey
     , HasField "key" (InPlay k) UnitKey
     )
  => (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onEnterPlay handler = onReceive $ Receive \msg owner self ->
  case matchEnteredPlay @k msg of
    Just (pk, uk)
      | pk == self.controller && uk == self.key ->
          handler owner self
    _ -> pure ()

-- | "Action: When this unit ambushes, …" Fires when this specific unit
-- enters play via the combat Ambush step ('UnitAmbushed'), distinct from
-- 'onEnterPlay' which fires for every entry. Used by the Ambush rider
-- units (Ded Scary Boy, Iron Defenders, the Days-of-Blood ambushers, …).
onAmbush
  :: HasField "key" UnitDetails UnitKey
  => (forall m. TriggerM m => Player -> UnitDetails -> m ())
  -> CardBuilder Unit ()
onAmbush handler = onReceive $ Receive \msg owner self -> case msg of
  UnitAmbushed pk uk
    | pk == self.controller && uk == self.key -> handler owner self
  _ -> pure ()

-- | "When this in-play unit takes one or more damage." Fires off the
-- 'DealDamageToUnit' message before Toughness is applied; if you
-- need post-cancellation semantics, gate on the new damage amount in
-- the handler. Used by cards like Silver Helm Brigade
-- ("draw a card after taking damage").
onSelfDamaged
  :: HasField "key" UnitDetails UnitKey
  => (forall m. TriggerM m => Player -> UnitDetails -> Int -> m ())
  -> CardBuilder Unit ()
onSelfDamaged handler = onReceive $ Receive \msg owner self -> case msg of
  DealDamageToUnit uk n
    | uk == self.key, n > 0 -> handler owner self n
  _ -> pure ()

-- | "When this card leaves play for any reason." Fires off the
-- narration message ('UnitLeftPlay'), so it covers destruction,
-- sacrifice, AND bounce-to-hand — distinct from 'onSelfDestroyed',
-- which only fires for the destroy event proper. Cards whose text
-- says "after this unit leaves play" want this; cards whose text
-- says "when this unit is destroyed" want 'onSelfDestroyed'.
onSelfLeavesPlay
  :: ( forall m
      . TriggerM m
     => Player -> UnitDetails -> m ()
     )
  -> CardBuilder Unit ()
onSelfLeavesPlay handler = onReceive $ Receive \msg owner self -> case msg of
  UnitLeftPlay du
    | du.key == self.key ->
        -- 'self' is the pre-departure snapshot the engine handed us;
        -- the departed unit info on 'du' matches what cards expect.
        handler owner self
  _ -> pure ()

-- | "When this card is destroyed." Useful for sacrifice-on-destruction
-- reactions (Festering Nurglings, Doombull, …). Pairs with the
-- 'UnitLeftPlay' / 'SupportLeftPlay' /… narration messages, but fires
-- one step earlier — at the actual destroy event — which is what most
-- "when this unit leaves play, …" cards want.
onSelfDestroyed
  :: forall k
   . ( HasDestroyMatch k
     , HasField "key" (InPlay k) UnitKey
     )
  => (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onSelfDestroyed handler = onReceive $ Receive \msg owner self ->
  case matchDestroy @k msg of
    Just uk | uk == self.key -> handler owner self
    _ -> pure ()

-- | "At the beginning of my controller's turn." Cards that further
-- gate on @self.zone == KingdomZone@ etc. should add that check inside
-- the handler body.
onMyTurnBegin
  :: forall k
   . HasField "controller" (InPlay k) PlayerKey
  => (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onMyTurnBegin handler = onReceive $ Receive \msg owner self -> case msg of
  BeginTurn pk | pk == self.controller -> handler owner self
  _ -> pure ()

-- | "At the end of my controller's turn." (e.g. Chaos Spawn.)
onMyTurnEnd
  :: forall k
   . HasField "controller" (InPlay k) PlayerKey
  => (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onMyTurnEnd handler = onReceive $ Receive \msg owner self -> case msg of
  EndTurn pk | pk == self.controller -> handler owner self
  _ -> pure ()

-- | "At the end of phase P on my controller's turn." Used by
-- Valkia the Bloody (end of her quest phase).
onMyPhaseEnd
  :: forall k
   . HasField "controller" (InPlay k) PlayerKey
  => Phase
  -> (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onMyPhaseEnd phase handler = onReceive $ Receive \msg owner self -> case msg of
  EndPhase p
    | p == phase -> do
        -- 'EndPhase' carries no player; phases belong to the player
        -- whose turn it is, so gate on the current player. (The
        -- 'owner' record passed by dispatch is always the
        -- controller's, so comparing it to 'self.controller' would
        -- be vacuous and the trigger would also fire on the
        -- opponent's phases.)
        g <- getGame
        when (g.currentPlayer == self.controller) $ handler owner self
  _ -> pure ()

-- | "At the beginning of phase P on my controller's turn." Used by
-- Temple of Shallya ("at the beginning of your kingdom phase…").
onMyPhaseBegin
  :: forall k
   . HasField "controller" (InPlay k) PlayerKey
  => Phase
  -> (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onMyPhaseBegin phase handler = onReceive $ Receive \msg owner self -> case msg of
  BeginPhase p
    | p == phase -> do
        g <- getGame
        when (g.currentPlayer == self.controller) $ handler owner self
  _ -> pure ()

-- | "When ANY 'BeginTurn' fires." The handler receives the turn owner
-- as its third argument. Use this for cards whose trigger depends on
-- some other player's turn (e.g. Mark of Chaos checks the host's
-- controller).
onAnyTurnBegin
  :: forall k
   . (forall m. TriggerM m => Player -> InPlay k -> PlayerKey -> m ())
  -> CardBuilder k ()
onAnyTurnBegin handler = onReceive $ Receive \msg owner self -> case msg of
  BeginTurn pk -> handler owner self pk
  _ -> pure ()

-- | "When an opponent's unit enters play." Skips self-triggering so a
-- unit's own entry doesn't fire its opponent-watch handler.
onOpponentUnitEnterPlay
  :: forall k
   . ( HasField "controller" (InPlay k) PlayerKey
     , HasField "key" (InPlay k) UnitKey
     )
  => (forall m. TriggerM m => Player -> InPlay k -> UnitKey -> m ())
  -> CardBuilder k ()
onOpponentUnitEnterPlay handler = onReceive $ Receive \msg owner self -> case msg of
  UnitEnteredPlay pk uk
    | pk /= self.controller && uk /= self.key ->
        handler owner self uk
  _ -> pure ()

-- | "When an opponent's unit leaves play." Carries the departing unit's
-- key, zone, and card code so handlers can react to specifics.
onOpponentUnitLeavePlay
  :: forall k
   . HasField "controller" (InPlay k) PlayerKey
  => ( forall m
      . TriggerM m
     => Player -> InPlay k -> UnitKey -> ZoneKind -> CardCode -> m ()
     )
  -> CardBuilder k ()
onOpponentUnitLeavePlay handler = onReceive $ Receive \msg owner self -> case msg of
  UnitLeftPlay du
    | du.controller /= self.controller ->
        handler owner self du.key du.zone du.cardDef.code
  _ -> pure ()

-- | "When one of my OTHER friendly units leaves play." Excludes the
-- card itself, which matters for unit cards that watch their teammates
-- (Dwarf Ranger).
onFriendlyUnitLeavePlay
  :: forall k
   . ( HasField "controller" (InPlay k) PlayerKey
     , HasField "key" (InPlay k) UnitKey
     )
  => ( forall m
      . TriggerM m
     => Player -> InPlay k -> UnitKey -> ZoneKind -> CardCode -> m ()
     )
  -> CardBuilder k ()
onFriendlyUnitLeavePlay handler = onReceive $ Receive \msg owner self -> case msg of
  UnitLeftPlay du
    | du.controller == self.controller, du.key /= self.key ->
        handler owner self du.key du.zone du.cardDef.code
  _ -> pure ()

-- | "When one of my OTHER units enters play." The friendly mirror of
-- 'onOpponentUnitEnterPlay'; skips the host's own entry so a unit's
-- arrival doesn't fire its own watch handler. Body receives the
-- entering unit's key (look it up with 'findUnit' for its zone /
-- traits). Used by Queen Helga and the "when a [type] unit enters
-- play under your control" cards.
onFriendlyUnitEnterPlay
  :: forall k
   . ( HasField "controller" (InPlay k) PlayerKey
     , HasField "key" (InPlay k) UnitKey
     )
  => (forall m. TriggerM m => Player -> InPlay k -> UnitKey -> m ())
  -> CardBuilder k ()
onFriendlyUnitEnterPlay handler = onReceive $ Receive \msg owner self -> case msg of
  UnitEnteredPlay pk uk
    | pk == self.controller && uk /= self.key ->
        handler owner self uk
  _ -> pure ()

-- | "When a unit enters this zone." Fires off 'UnitEnteredPlay' when
-- the entering unit lands in the host's own zone (same controller,
-- same zone kind), skipping the host's own entry. Body receives the
-- entering unit's key. Used by Doom Bearer, Banna Thief, and
-- Bannerman of the Crag.
--
-- Approximation: keys off play / put-into-play entries only. A unit
-- *relocating* into the zone may not re-emit 'UnitEnteredPlay', so
-- those movements don't trigger — the common "enters play here" case
-- is covered.
onUnitEnterMyZone
  :: forall k
   . ( HasField "controller" (InPlay k) PlayerKey
     , HasField "key" (InPlay k) UnitKey
     , HasField "zone" (InPlay k) ZoneKind
     )
  => (forall m. TriggerM m => Player -> InPlay k -> UnitKey -> m ())
  -> CardBuilder k ()
onUnitEnterMyZone handler = onReceive $ Receive \msg owner self -> case msg of
  UnitEnteredPlay pk uk
    | pk == self.controller && uk /= self.key -> do
        g <- getGame
        case findUnit uk g of
          Just u | u.zone == self.zone -> handler owner self uk
          _ -> pure ()
  _ -> pure ()

-- | "When a unit is corrupted." Fires off the 'CorruptUnit' message
-- and hands the body the corrupted unit's key; the body decides
-- whether it cares (e.g. "a [Chaos] unit you control"). Used by The
-- Bleeding Wall and Beastman Shaman.
onUnitCorrupted
  :: forall k
   . (forall m. TriggerM m => Player -> InPlay k -> UnitKey -> m ())
  -> CardBuilder k ()
onUnitCorrupted handler = onReceive $ Receive \msg owner self -> case msg of
  CorruptUnit uk -> handler owner self uk
  _ -> pure ()

-- | "When you play a (non-Attachment) support card from your hand."
-- Fires off the from-hand 'PlaySupport' message only (not the
-- from-deck / from-discard variants), guarded to the host's
-- controller. Body receives the played support's key — look it up
-- with 'findSupport' to filter on race / traits. Used by the
-- "…for War" quest cycle.
onYouPlaySupport
  :: forall k
   . HasField "controller" (InPlay k) PlayerKey
  => (forall m. TriggerM m => Player -> InPlay k -> UnitKey -> m ())
  -> CardBuilder k ()
onYouPlaySupport handler = onReceive $ Receive \msg owner self -> case msg of
  PlaySupport pk uk _zone
    | pk == self.controller -> handler owner self uk
  _ -> pure ()

-- | "When you play a development from your hand." Fires off the
-- 'PlayDevelopment' message (the active player's once-per-turn develop),
-- guarded to the host's controller. Used by the Morrslieb-cycle
-- token-engine units and quests ("When you play a development from your
-- hand, put a resource token …").
onYouPlayDevelopment
  :: forall k
   . HasField "controller" (InPlay k) PlayerKey
  => (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onYouPlayDevelopment handler = onReceive $ Receive \msg owner self -> case msg of
  PlayDevelopment pk _uk _zone
    | pk == self.controller -> handler owner self
  _ -> pure ()

-- | "Action: When you play a development from your hand, put a resource
-- token on this card if a unit is questing here." The development-driven
-- mirror of 'accrueTokenWhileQuesting'; shared by Malekith's Rage,
-- Follow the Portent, and the rest of the Morrslieb token quests.
accrueTokenOnDevelopmentWhileQuesting :: CardBuilder Quest ()
accrueTokenOnDevelopmentWhileQuesting = onYouPlayDevelopment \_owner self ->
  withQuest self.key \q ->
    when (isJust q.questingUnit) $ addQuestToken self.key 1

-- | The shared second half of the "…for War" quest cycle: "When you
-- play a [Race] non-Attachment support card from your hand, <payoff>
-- if a unit is questing here." Filters the played support on race and
-- the non-Attachment clause, then gates the payoff on a unit
-- currently questing on this quest.
onQuestSupportPayoff
  :: Race
  -> (forall m. TriggerM m => InPlay Quest -> m ())
  -> CardBuilder Quest ()
onQuestSupportPayoff r body = onYouPlaySupport \_owner self uk -> do
  g <- getGame
  case findSupport uk g of
    Just s
      | r `elem` s.cardDef.races && Attachment `notElem` s.cardDef.traits ->
          withQuest self.key \q -> when (isJust q.questingUnit) (body self)
    _ -> pure ()

-- | "When this tactic resolves." The handler receives the chosen
-- target carried by 'PlayTactic'; for tactics that ignore the target
-- the third argument can be ignored.
onResolve
  :: ( forall m
      . TriggerM m
     => Player -> TacticContext -> ActionTarget -> m ()
     )
  -> CardBuilder Tactic ()
onResolve handler = onReceive $ Receive \msg owner self -> case msg of
  TacticResolved pk _code target _x
    | pk == self.controller ->
        handler owner self target
  _ -> pure ()

-- | "When this tactic resolves." Slim version that only exposes
-- 'self' — preferred for tactics that don't pre-declare a target via
-- 'tacticTargets' and don't need the 'owner' record. Use 'withTarget'
-- mid-effect for player picks, and 'playerOf self.controller' if you
-- need the full 'Player'.
whenResolved
  :: (forall m. TriggerM m => TacticContext -> m ())
  -> CardBuilder Tactic ()
whenResolved handler = onReceive $ Receive \msg _owner self -> case msg of
  TacticResolved pk _code _target _x | pk == self.controller -> handler self
  _ -> pure ()

-- | "When this tactic resolves, with the chosen enemy support
-- resolved off the engine-supplied 'ActionTarget'." Pairs with
-- 'tacticTargets SupportTargetSchema' (the engine prompts for the
-- support at play time). The handler is skipped silently if no
-- enemy support is in play. Used by Demolition!, Da Big Stomp.
onResolveEnemySupport
  :: (forall m. TriggerM m => PlayerKey -> SupportDetails -> m ())
  -> CardBuilder Tactic ()
onResolveEnemySupport handler = onResolve \_owner self target -> do
  g <- getGame
  let pk = self.controller
  whenJust (resolveEnemySupport pk target g) $ handler pk
  where
    resolveEnemySupport pk t g = case t of
      TargetSupport k | Just s <- findSupport k g, s.controller /= pk -> Just s
      _ -> find ((/= pk) . (.controller)) g.supports

-- | "On combat resolve while this in-play card is one of the
-- attackers." Used by approximate "when this unit damages a zone /
-- enemy" cards (Lokhir, Corsairs, Malekith).
onCombatResolveAsAttacker
  :: forall k
   . HasField "key" (InPlay k) UnitKey
  => (forall m. TriggerM m => Player -> InPlay k -> CombatState -> m ())
  -> CardBuilder k ()
onCombatResolveAsAttacker handler = onReceive $ Receive \msg owner self -> case msg of
  ResolveCombat -> withCombat \cs ->
    when (self.key `elem` cs.attackers) $ handler owner self cs
  _ -> pure ()

-- | "On combat resolve while this in-play card is one of the
-- defenders." The defender-side mirror of 'onCombatResolveAsAttacker',
-- used by "if this unit defends and survives combat, …" cards (Battle
-- Wizard). Combine with an in-play check in the handler for the
-- "survives" clause.
onCombatResolveAsDefender
  :: forall k
   . HasField "key" (InPlay k) UnitKey
  => (forall m. TriggerM m => Player -> InPlay k -> CombatState -> m ())
  -> CardBuilder k ()
onCombatResolveAsDefender handler = onReceive $ Receive \msg owner self -> case msg of
  ResolveCombat -> withCombat \cs ->
    when (self.key `elem` cs.defenders) $ handler owner self cs
  _ -> pure ()

-- | "When this in-play card is declared as part of an attack."
onMyAttackDeclared
  :: forall k
   . ( HasField "controller" (InPlay k) PlayerKey
     , HasField "key" (InPlay k) UnitKey
     )
  => ( forall m
      . TriggerM m
     => Player -> InPlay k -> ZoneKind -> [UnitKey] -> m ()
     )
  -> CardBuilder k ()
onMyAttackDeclared handler = onReceive $ Receive \msg owner self -> case msg of
  BeginCombat attacker zone attackers
    | attacker == self.controller, self.key `elem` attackers ->
        handler owner self zone attackers
  _ -> pure ()

-- | "When this in-play card attacks or defends." Fires when the card is
-- declared as an attacker ('BeginCombat') or locked in as a defender
-- ('DeclareDefenders'). The "Action: When this unit attacks or defends,
-- …" idiom shared by Ludwig Schwarzheim, Maid of Sigmar, Reiksguard
-- Elite, Horn Hold Defender, and Vaedra Bloodsworn.
onMyAttackOrDefend
  :: forall k
   . ( HasField "controller" (InPlay k) PlayerKey
     , HasField "key" (InPlay k) UnitKey
     )
  => (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onMyAttackOrDefend handler = onReceive $ Receive \msg owner self -> case msg of
  BeginCombat attacker _zone attackers
    | attacker == self.controller, self.key `elem` attackers ->
        handler owner self
  DeclareDefenders ks
    | self.key `elem` ks -> handler owner self
  _ -> pure ()

-- | "Action: When this unit attacks or defends, attach 1 experience to
-- it." The whole-line idiom shared by the experience-scaling units
-- (Ludwig Schwarzheim, Maid of Sigmar, Reiksguard Elite). The
-- experience is the host's own code, since facedown experiences only
-- ever matter as a count.
gainsExperienceOnAttackOrDefend
  :: ( HasField "controller" (InPlay k) PlayerKey
     , HasField "key" (InPlay k) UnitKey
     , HasField "cardDef" (InPlay k) (CardDef k)
     )
  => CardBuilder k ()
gainsExperienceOnAttackOrDefend =
  onMyAttackOrDefend \_owner self -> attachExperience self.key self.cardDef.code

-- | "When my zone is attacked by an opponent." Fires off
-- 'BeginCombat'; the support's zone (its 'zone' field) is checked
-- against the combat target. Cauldron of Blood uses this.
onMyZoneAttacked
  :: ( forall m
      . TriggerM m
     => Player -> SupportDetails -> CombatState -> m ()
     )
  -> CardBuilder Support ()
onMyZoneAttacked handler = onReceive $ Receive \msg owner self -> case msg of
  BeginCombat attacker zone attackers
    | attacker /= self.controller, zone == self.zone -> do
        let cs = CombatState
              { attackingPlayer = attacker
              , defendingPlayer = self.controller
              , targetZone = zone
              , targetLegend = Nothing
              , attackers = attackers
              , defenders = []
              , attackerPowerPenalty = 0
              , pendingAssignments = []
              }
        handler owner self cs
  _ -> pure ()

-- | "When the given action window opens." Used by cards (Vicious
-- Marauder) that force their controller's action choice at a specific
-- phase boundary.
onActionWindow
  :: forall k
   . ActionWindowTrigger
  -> (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onActionWindow which handler = onReceive $ Receive \msg owner self -> case msg of
  OpenActionWindow t | t == which -> handler owner self
  _ -> pure ()

-- | "When this quest's token count changes." Receives the delta. Used
-- by Dominion of Chaos to fire on the third token landing.
onMyQuestTokensAdjusted
  :: ( forall m
      . TriggerM m
     => Player -> QuestDetails -> Int -> m ()
     )
  -> CardBuilder Quest ()
onMyQuestTokensAdjusted handler = onReceive $ Receive \msg owner self -> case msg of
  AdjustQuestTokens qk delta
    | qk == self.key ->
        handler owner self delta
  _ -> pure ()

-- | "When the unit this support is attached to is dealt damage."
-- Skips zero-damage hits and unattached supports.
onHostDamaged
  :: ( forall m
      . TriggerM m
     => Player -> SupportDetails -> UnitKey -> Int -> m ()
     )
  -> CardBuilder Support ()
onHostDamaged handler = onReceive $ Receive \msg owner self -> case msg of
  DealDamageToUnit uk n
    | Just hostKey <- self.attachedTo, uk == hostKey, n > 0 ->
        handler owner self hostKey n
  _ -> pure ()

-- | "When the host of this attachment's controller's turn begins."
-- The host record is resolved from the current game and passed to the
-- handler.
onAttachedHostTurnBegin
  :: ( forall m
      . TriggerM m
     => Player -> SupportDetails -> UnitDetails -> m ()
     )
  -> CardBuilder Support ()
onAttachedHostTurnBegin handler = onReceive $ Receive \msg owner self -> case msg of
  BeginTurn pk
    | Just hostKey <- self.attachedTo -> do
        g <- getGame
        case findUnit hostKey g of
          Just host | host.controller == pk -> handler owner self host
          _ -> pure ()
  _ -> pure ()

-- | "When the unit this attachment is on attacks." Fires off
-- 'BeginCombat' when the host is among the declared attackers, and
-- hands the body the resolved host record. The on-attack mirror of
-- 'onAttachedHostTurnBegin'; backs Barbed Whip and Standard of Clar
-- Karond.
onAttachedHostAttack
  :: ( forall m
      . TriggerM m
     => Player -> SupportDetails -> UnitDetails -> m ()
     )
  -> CardBuilder Support ()
onAttachedHostAttack handler = onReceive $ Receive \msg owner self -> case msg of
  BeginCombat _attacker _zone attackers
    | Just hostKey <- self.attachedTo, hostKey `elem` attackers -> do
        g <- getGame
        whenJust (findUnit hostKey g) (handler owner self)
  _ -> pure ()

-- | "When the unit this attachment is on defends." Fires off
-- 'DeclareDefenders' when the host is locked in as a defender. Backs
-- Dragon Armour.
onAttachedHostDefend
  :: ( forall m
      . TriggerM m
     => Player -> SupportDetails -> UnitDetails -> m ()
     )
  -> CardBuilder Support ()
onAttachedHostDefend handler = onReceive $ Receive \msg owner self -> case msg of
  DeclareDefenders ks
    | Just hostKey <- self.attachedTo, hostKey `elem` ks -> do
        g <- getGame
        whenJust (findUnit hostKey g) (handler owner self)
  _ -> pure ()

-- | "When the unit this attachment is on attacks or defends." The union
-- of 'onAttachedHostAttack' and 'onAttachedHostDefend'; backs Moon Staff
-- of Lileath.
onAttachedHostAttackOrDefend
  :: ( forall m
      . TriggerM m
     => Player -> SupportDetails -> UnitDetails -> m ()
     )
  -> CardBuilder Support ()
onAttachedHostAttackOrDefend handler = onReceive $ Receive \msg owner self ->
  let fire hostKeys = case self.attachedTo of
        Just hostKey | hostKey `elem` hostKeys -> do
          g <- getGame
          whenJust (findUnit hostKey g) (handler owner self)
        _ -> pure ()
   in case msg of
        BeginCombat _attacker _zone attackers -> fire attackers
        DeclareDefenders ks -> fire ks
        _ -> pure ()

-- | "Run on every message dispatch this card sees." Used by
-- Northern Wastes' continuous self-check. Avoid unless you really
-- need it; it costs one closure call per message.
onAnyMessage
  :: forall k
   . (forall m. TriggerM m => Player -> InPlay k -> m ())
  -> CardBuilder k ()
onAnyMessage handler = onReceive $ Receive \_msg owner self -> handler owner self

-- | "When you control X, sacrifice this card." Re-checks the predicate
-- whenever the controller's board could have changed (a card entering
-- play, or the start of a turn) and sacrifices the support the moment
-- it holds. Backs the self-sacrificing buildings (the mono-faction
-- watchtowers, Mob O' Hutz).
sacrificeWhenBoardChanges
  :: (Game -> SupportDetails -> Bool) -> CardBuilder Support ()
sacrificeWhenBoardChanges p = onReceive $ Receive \msg _owner self -> do
  let boardChanged = case msg of
        UnitEnteredPlay{} -> True
        SupportEnteredPlay{} -> True
        QuestEnteredPlay{} -> True
        BeginTurn{} -> True
        _ -> False
  when boardChanged do
    g <- getGame
    when (p g self) $ destroySupport self.key

-- | "If you control a non-[Race] card, sacrifice this card." The
-- mono-faction watchtower idiom (Chill Sea Watchtower, Outlying Tower).
sacrificeIfControlsOffFaction :: Race -> CardBuilder Support ()
sacrificeIfControlsOffFaction r =
  sacrificeWhenBoardChanges \g self -> controlsNonRaceCard g self.controller r

-- | Append an 'ActionDef' to the card's static action list. Multiple
-- actions can be declared; the engine surfaces them to the client by
-- index. Used directly by cards that build a record-style action; the
-- slim positional 'action' verb (declared in an 'EffectM' block) is
-- the preferred entry point.
actionDef :: ActionDef k -> CardBuilder k ()
actionDef a = modify \cardDef -> cardDef {actions = cardDef.actions ++ [a]}

-- | Convenience builder for declaring a tactic's target schema. The
-- effect closure isn't used for tactics — the engine fires the
-- card's 'receive' with 'TacticResolved'; the action metadata is
-- there purely for client display and target validation.
tacticTargets :: TargetSchema -> CardBuilder Tactic ()
tacticTargets schema = actionDef ActionDef
  { actionName = "Play"
  , actionCost = 0  -- the actual cost lives on CardDef.cost
  , actionExtraCosts = []
  , actionTarget = schema
  , availableInZone = Nothing
  , actionOpponentOnly = False
  , actionEffect = ActionEffect \_usage -> pure ()
  }

-- ---------------------------------------------------------------------
-- Card-body verbs
--
-- 'forced', 'triggered', 'action', and 'constant' are top-level
-- CardBuilder verbs that mirror the printed card-text categories.
-- Outside a zone wrapper they register their effects unconditionally;
-- inside 'battlefield' / 'quest' / 'kingdom' the wrapper save/restores
-- the registrations and applies a zone gate.
-- ---------------------------------------------------------------------

-- | Read access to the current 'Message' from inside a 'forced'
-- body. Event-dispatch verbs (`onTurnBegin`, `onFriendlyUnitLeave`,
-- …) consult this to decide whether to fire.
class Monad m => HasMessage m where
  getMessage :: m Message

-- | Carrier monad for a 'forced' body. Adds read access to the
-- current 'Message' over the underlying trigger monad while
-- forwarding every other engine capability.
newtype ForcedT m a = ForcedT (ReaderT Message m a)
  deriving newtype (Functor, Applicative, Monad)

instance MonadIO m => MonadIO (ForcedT m) where
  liftIO = ForcedT . liftIO

instance HasGame m => HasGame (ForcedT m) where
  getGame = ForcedT (lift getGame)

instance HasQueue msg m => HasQueue msg (ForcedT m) where
  getQueue = ForcedT (lift getQueue)

instance HasPromptIO m => HasPromptIO (ForcedT m) where
  askPrompt = ForcedT . lift . askPrompt

instance Monad m => HasMessage (ForcedT m) where
  getMessage = ForcedT ask

runForcedT :: ForcedT m a -> Message -> m a
runForcedT (ForcedT r) = runReaderT r

-- | "Forced: …" registration. The body runs every time the host
-- card's 'Receive' is dispatched; inside, event-dispatchers like
-- 'onTurnBegin' / 'onFriendlyUnitLeave' decide whether to fire based
-- on the current 'Message'. This is the only place 'getMessage' is
-- in scope.
--
-- > kingdom $ forced \self ->
-- >   onTurnBegin self.controller $
-- >     healCapital self.controller 1
forced
  :: forall k.
     ( forall m
       . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m, HasMessage m)
      => InPlay k -> m ()
     )
  -> CardBuilder k ()
forced body = onReceive $ Receive \msg _owner self ->
  runForcedT (body self) msg

-- | "Action: When …" marker for action-triggered abilities. Currently
-- a documentation alias.
triggered :: CardBuilder k () -> CardBuilder k ()
triggered = id

-- ---------------------------------------------------------------------
-- Event dispatchers (used inside 'forced' bodies)
-- ---------------------------------------------------------------------

-- | "At the beginning of [pk]'s turn, …" Runs the action only when
-- the current message is 'BeginTurn pk'.
onTurnBegin :: HasMessage m => PlayerKey -> m () -> m ()
onTurnBegin pk action = do
  msg <- getMessage
  case msg of
    BeginTurn k | k == pk -> action
    _ -> pure ()

-- | "At the end of [pk]'s turn, …"
onTurnEnd :: HasMessage m => PlayerKey -> m () -> m ()
onTurnEnd pk action = do
  msg <- getMessage
  case msg of
    EndTurn k | k == pk -> action
    _ -> pure ()

-- | "When one of [pk]'s units leaves play, …" Body receives a
-- 'DepartedUnit' snapshot. Card text that says "one of your OTHER
-- units" should additionally guard on @unit.key /= self.key@ inside
-- the body.
onUnitOfLeavesPlay
  :: HasMessage m
  => PlayerKey -> (DepartedUnit -> m ()) -> m ()
onUnitOfLeavesPlay pk body = do
  msg <- getMessage
  case msg of
    UnitLeftPlay du | du.controller == pk -> body du
    _ -> pure ()

-- | "Action: …" top-level verb. Registers a card action; by default
-- 'availableInZone' is 'Nothing' (always available). Inside a zone
-- wrapper the wrapper patches it to 'Just z'. The effect body
-- receives an 'ActionUsage' record with the firing player, this
-- card, the resolved target, and (empty for plain 'action') the
-- payment receipts.
action
  :: Text
  -> Int
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage k -> m ()
     )
  -> CardBuilder k ()
action name cost effect = actionDef ActionDef
  { actionName = name
  , actionCost = cost
  , actionExtraCosts = []
  , actionTarget = NoTargetSchema
  , availableInZone = Nothing
  , actionOpponentOnly = False
  , actionEffect = ActionEffect effect
  }

-- | Action with non-resource extra costs (sacrifice, discard, …) in
-- addition to a resource cost. The engine validates every extra cost
-- before firing and prompts for any choices needed to pay them; if
-- any extra cost can't be paid the action is rejected before
-- resources are spent. The effect body receives an 'ActionUsage'
-- carrying receipts for the paid extras.
--
-- > battlefield $ actionWith "Volley" 1 [SacrificeUnit] \usage -> ...
actionWith
  :: Text
  -> Int
  -> [ExtraCost]
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage k -> m ()
     )
  -> CardBuilder k ()
actionWith name cost extras effect = actionDef ActionDef
  { actionName = name
  , actionCost = cost
  , actionExtraCosts = extras
  , actionTarget = NoTargetSchema
  , availableInZone = Nothing
  , actionOpponentOnly = False
  , actionEffect = ActionEffect effect
  }

-- | "Only an opponent may trigger this ability." (Morathi's Pegasus.)
-- The opponent of the host's controller pays the cost and fires the
-- effect; the engine refuses the controller's own trigger.
actionOpponent
  :: Text
  -> Int
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage k -> m ()
     )
  -> CardBuilder k ()
actionOpponent name cost effect = actionDef ActionDef
  { actionName = name
  , actionCost = cost
  , actionExtraCosts = []
  , actionTarget = NoTargetSchema
  , availableInZone = Nothing
  , actionOpponentOnly = True
  , actionEffect = ActionEffect effect
  }

-- | "Action: ... target enemy unit." Engine-side target prompt:
-- the player is asked for an enemy unit at activation time, BEFORE
-- the resource cost is paid (so the action can't be activated when
-- no valid target exists). The body receives the chosen 'UnitKey'.
--
-- > actionEnemyUnit "Shoot" 1 \_u k -> dealDamage k 1
actionEnemyUnit, actionFriendlyUnit, actionAnyUnit
  :: Text
  -> Int
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage k -> UnitKey -> m ()
     )
  -> CardBuilder k ()
actionEnemyUnit name cost body =
  actionUnitSchema name cost EnemyUnitTargetSchema body
-- | "Action: ... target unit you control." See 'actionEnemyUnit'.
actionFriendlyUnit name cost body =
  actionUnitSchema name cost FriendlyUnitTargetSchema body
-- | "Action: ... target unit (any player)." See 'actionEnemyUnit'.
actionAnyUnit name cost body =
  actionUnitSchema name cost AnyUnitTargetSchema body

-- | Shared implementation for 'actionEnemyUnit' / 'actionFriendlyUnit'
-- / 'actionAnyUnit'. Pins the schema, leaves zone gate and extra
-- costs at defaults.
actionUnitSchema
  :: Text -> Int -> TargetSchema
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage k -> UnitKey -> m ()
     )
  -> CardBuilder k ()
actionUnitSchema name cost schema body = actionDef ActionDef
  { actionName = name
  , actionCost = cost
  , actionTarget = schema
  , availableInZone = Nothing
  , actionOpponentOnly = False
  , actionExtraCosts = []
  , actionEffect = ActionEffect \u -> case u.target of
      TargetUnit k -> body u k
      _ -> pure ()
  }

-- | "Action: ... target enemy zone." Engine prompts for an opposing
-- capital zone before activation; body receives the @(owner, zone)@
-- pair so card bodies can route messages either at the owner or at
-- the activating player.
actionEnemyZone
  :: Text -> Int
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage k -> (PlayerKey, ZoneKind) -> m ()
     )
  -> CardBuilder k ()
actionEnemyZone name cost body = actionDef ActionDef
  { actionName = name
  , actionCost = cost
  , actionTarget = EnemyZoneTargetSchema
  , availableInZone = Nothing
  , actionOpponentOnly = False
  , actionExtraCosts = []
  , actionEffect = ActionEffect \u -> case u.target of
      TargetZone owner z -> body u (owner, z)
      _ -> pure ()
  }

-- | "Forced: At the beginning of your turn, place 1 resource token
-- on this card if a unit is questing here." The whole-line idiom
-- shared by every token-payoff quest (Defending the Empire, The
-- White Tower, Slaughter at Lustria, Greenskin Rush).
--
-- > forced \self -> accrueTokenWhileQuesting self
accrueTokenWhileQuesting
  :: ( HasMessage m, HasGame m, HasQueue Message m )
  => InPlay Quest -> m ()
accrueTokenWhileQuesting self =
  onTurnBegin self.controller $
    withQuest self.key \q ->
      when (isJust q.questingUnit) $
        addQuestToken self.key 1

-- | "Action: Spend N tokens to do X." Models the second half of a
-- token-payoff quest. The wrapper enforces "has ≥ N tokens" before
-- firing the body and debits the tokens on success, so the card body
-- only states the payoff.
--
-- > spendTokens "Draw 3 cards" 3 \u -> drawCards u.user 3
spendTokens
  :: Text
  -> Int
  -> ( forall m
      . (HasGame m, MonadIO m, HasQueue Message m, HasPromptIO m)
     => ActionUsage Quest -> m ()
     )
  -> CardBuilder Quest ()
spendTokens name n body = actionDef ActionDef
  { actionName = name
  , actionCost = 0
  , actionTarget = NoTargetSchema
  , availableInZone = Nothing
  , actionOpponentOnly = False
  , actionExtraCosts = []
  , actionEffect = ActionEffect \u ->
      withQuest u.self.key \q -> when (q.tokens >= n) do
        addQuestToken u.self.key (-n)
        body u
  }

-- | "Constant" static-effect block. Body fires every engine tick and
-- accumulates 'EffectM' contributions into the unit's per-tick
-- 'runtimeEffects'. The lambda receives just this unit; live game
-- state (the unit's current zone record, its owner, etc.) is
-- available via lookup helpers like 'zoneOf' so the read is always
-- fresh.
--
-- > battlefield $ constant \self -> do
-- >   z <- zoneOf self
-- >   when (z.developments >= 2) $ gainPower self 2
constant
  :: (UnitDetails -> EffectM ())
  -> CardBuilder Unit ()
constant body = modifyUnitExtras \e -> e
  { runtimeEffects = \g self ->
      let prev = e.runtimeEffects g self
       in prev <> execEffectM g (body self)
  }

-- ---------------------------------------------------------------------
-- Zone wrappers
--
-- 'battlefield', 'quest', 'kingdom' wrap a card-builder block and
-- zone-gate every registration added inside: triggers ('forced' /
-- 'triggered' / direct trigger combinators) get a 'self.zone == z'
-- check at fire time; actions get 'availableInZone = Just z'; for
-- Unit cards 'constant' callbacks get the same self-zone gate.
-- ---------------------------------------------------------------------

-- | Per-kind handling of the zone gate. Receive + actions are
-- handled uniformly across kinds; 'constant'-style 'runtimeEffects'
-- gating only applies to Unit cards (no other kind has that slot).
class CanZoneGate (k :: CardKind) where
  withZoneGate :: ZoneKind -> CardBuilder k () -> CardBuilder k ()

instance CanZoneGate Unit where
  withZoneGate z (CardBuilder inner) = CardBuilder $ do
    cd0 <- get
    let prevReceive = cd0.receive
        prevActions = cd0.actions
        prevRT = cd0.extras.runtimeEffects
    modify \cd -> cd
      { receive = noReceive
      , actions = []
      , extras = cd.extras {runtimeEffects = \_ _ -> mempty}
      }
    inner
    cd1 <- get
    let addedReceive = cd1.receive
        addedActions = cd1.actions
        addedRT = cd1.extras.runtimeEffects
    modify \cd -> cd
      { receive = composeReceive prevReceive (gateReceive z addedReceive)
      , actions =
          prevActions
            <> map (\a -> a {availableInZone = Just z}) addedActions
      , extras = cd.extras
          { runtimeEffects = \g self ->
              prevRT g self
                <> if self.zone == z then addedRT g self else mempty
          }
      }

instance CanZoneGate Support where
  withZoneGate z (CardBuilder inner) = CardBuilder $ do
    cd0 <- get
    let prevReceive = cd0.receive
        prevActions = cd0.actions
    modify \cd -> cd {receive = noReceive, actions = []}
    inner
    cd1 <- get
    let addedReceive = cd1.receive
        addedActions = cd1.actions
    modify \cd -> cd
      { receive = composeReceive prevReceive (gateReceive z addedReceive)
      , actions =
          prevActions
            <> map (\a -> a {availableInZone = Just z}) addedActions
      }

instance CanZoneGate Legend where
  withZoneGate z (CardBuilder inner) = CardBuilder $ do
    cd0 <- get
    let prevReceive = cd0.receive
        prevActions = cd0.actions
    modify \cd -> cd {receive = noReceive, actions = []}
    inner
    cd1 <- get
    let addedReceive = cd1.receive
        addedActions = cd1.actions
    modify \cd -> cd
      { receive = composeReceive prevReceive (gateReceive z addedReceive)
      , actions =
          prevActions
            <> map (\a -> a {availableInZone = Just z}) addedActions
      }

-- | "Battlefield. …" gate. Wraps a card-builder block; every
-- registration inside is gated to the battlefield zone.
--
-- > trollSlayers = unitCard "core-004" "Troll Slayers" do
-- >   ...
-- >   battlefield $ constant \zone self _owner ->
-- >     when (zone.developments >= 2) $ gainPower self 2
battlefield :: CanZoneGate k => CardBuilder k () -> CardBuilder k ()
battlefield = withZoneGate BattlefieldZone

-- | "Quest. …" gate. Mirrors 'battlefield' for the quest zone.
quest :: CanZoneGate k => CardBuilder k () -> CardBuilder k ()
quest = withZoneGate QuestZone

-- | "Kingdom. …" gate. Mirrors 'battlefield' for the kingdom zone.
kingdom :: CanZoneGate k => CardBuilder k () -> CardBuilder k ()
kingdom = withZoneGate KingdomZone
