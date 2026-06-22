{-# LANGUAGE GADTs #-}

module Invasion.Message (module Invasion.Message) where

import Invasion.CardDef (ActionTarget, CardDef)
import Invasion.Game (ActionWindowTrigger, Prompt, PromptResult)
import Invasion.Modifier (Modifier, ModifierScope)
import Invasion.Player (Drawing, EliminationReason)
import Invasion.Prelude
import Invasion.Types

-- | Engine events. Every state change goes through a constructor here;
-- card text plugs in by handling, intercepting, or emitting these. New
-- behavior generally means a new constructor, not inlining work in a
-- handler.
-- | Snapshot of a unit that has just left play. Bundles the
-- information a "when this unit leaves play" trigger needs without
-- requiring a fresh lookup against game state (the unit is already
-- gone by the time the trigger fires).
data DepartedUnit = DepartedUnit
  { key :: UnitKey
  , controller :: PlayerKey
  , zone :: ZoneKind
  , cardDef :: CardDef Unit
  }
  deriving stock Show

data Message where
  -- Setup / lifecycle
  Setup :: Message
  BeginGame :: Message
  -- Player upkeep
  ShuffleDeck :: PlayerKey -> Message
  Draw :: Drawing -> Message
  Eliminate :: PlayerKey -> EliminationReason -> Message
  -- Turn structure
  BeginTurn :: PlayerKey -> Message
  EndTurn :: PlayerKey -> Message
  BeginPhase :: Phase -> Message
  EndPhase :: Phase -> Message
  -- Phase steps
  ReturnResources :: PlayerKey -> Message
  CollectResources :: PlayerKey -> Message
  QuestDraw :: PlayerKey -> Message
  -- Action windows
  OpenActionWindow :: ActionWindowTrigger -> Message
  PassPriority :: PlayerKey -> Message
  CloseActionWindow :: Message
  -- Card play.
  --
  -- These reference a card-instance by its stable 'UnitKey' (the same
  -- key the card has carried since deck-init). The engine looks the
  -- card up in the named player's hand, verifies its kind matches the
  -- constructor, and reuses the key as the new in-play unit / support /
  -- quest / legend identity. No fresh key is minted on play.
  PlayUnit :: PlayerKey -> UnitKey -> ZoneKind -> Message
  PlayUnitOnQuest :: PlayerKey -> UnitKey -> UnitKey -> Message
    -- ^ Play a unit from hand into the quest zone, simultaneously
    -- committing it to quest on the named Quest card. The first
    -- 'UnitKey' is the in-hand card key; the second is the existing
    -- Quest's key. Pays cost, places the unit in 'QuestZone', and
    -- sets 'QuestDetails.questingUnit'.
  UnitEnteredPlay :: PlayerKey -> UnitKey -> Message
  UnitAmbushed :: PlayerKey -> UnitKey -> Message
    -- ^ Narration + trigger event: the named unit just entered play via
    -- the combat Ambush step (sent right after its 'UnitEnteredPlay').
    -- Cards' "Action: When this unit ambushes, …" abilities react to
    -- this via the 'onAmbush' trigger.
  AssignUnitToQuest :: PlayerKey -> UnitKey -> UnitKey -> Message
    -- ^ Attach an already-in-play unit (first UnitKey, must be in the
    -- quest zone of pk) to a quest (second UnitKey). Refused if the
    -- quest already has a questing unit.
  -- Damage / destroy
  DealDamageToUnit :: UnitKey -> Int -> Message
    -- ^ Apply N damage to a target unit, subject to Toughness
    -- cancellation. If accumulated damage equals or exceeds the
    -- unit's HP, the engine queues 'DestroyUnit'.
  DealDamageToUnitUncancellable :: UnitKey -> Int -> Message
    -- ^ Same as 'DealDamageToUnit' but bypasses Toughness and other
    -- damage-cancel effects. Used by cards with the
    -- 'DamageCannotBeCancelled' keyword or that explicitly call out
    -- uncancellable damage in their text.
  HealUnit :: UnitKey -> Int -> Message
    -- ^ Remove up to N damage from a target unit (clamped to 0).
  DestroyUnit :: UnitKey -> Message
    -- ^ Remove a unit from play and put its card into the controller's
    -- discard. Triggers 'UnitLeftPlay'.
  UnitLeftPlay :: DepartedUnit -> Message
    -- ^ Narration / hook point fired after a unit has been removed.
    -- Cards react by inspecting the previous controller and code.
  -- Corruption
  CorruptUnit :: UnitKey -> Message
    -- ^ Mark a unit corrupted. No-op if already corrupted.
  CleanseUnit :: UnitKey -> Message
    -- ^ Clear the corrupted flag (used by the kingdom-phase restoration
    -- step and by future cleanse effects).
  RestoreOneCorruptCard :: PlayerKey -> Message
    -- ^ Kingdom-phase restoration step: auto-cleanse one corrupt card
    -- the named player controls (first found). The rules let the
    -- player choose; until prompts exist, the engine just picks the
    -- first.
  -- Attachments
  PlayAttachment :: PlayerKey -> UnitKey -> UnitKey -> Message
    -- ^ Play a Support card from hand as an attachment to a target
    -- unit. First 'UnitKey' is the support card in hand; second is the
    -- target unit already in play. Pays cost, removes the card from
    -- hand, and emits 'SupportEnteredPlay'.
  SupportEnteredPlay :: PlayerKey -> UnitKey -> Message
    -- ^ A support card (attached or free-standing) has just entered
    -- play. The support's 'attachedTo' field distinguishes the two
    -- cases.
  -- Free-standing supports + quests
  PlaySupport :: PlayerKey -> UnitKey -> ZoneKind -> Message
    -- ^ Play a non-attachment Support card from hand into one of your
    -- zones. Pays cost, removes the card, emits 'SupportEnteredPlay'.
  PlaySupportFromDeck :: PlayerKey -> UnitKey -> ZoneKind -> Message
    -- ^ Bypass the cost / hand path: pull the named support card out
    -- of the player's deck and put it directly into the named zone.
    -- Used by deck-search effects (e.g. Dwarf Cannon Crew).
  PlayQuest :: PlayerKey -> UnitKey -> Message
    -- ^ Play a Quest from hand. Emits 'QuestEnteredPlay'.
  QuestEnteredPlay :: PlayerKey -> UnitKey -> Message
    -- ^ A Quest has just entered play. The fresh key references the
    -- entry in 'Game.quests'.
  -- Token bookkeeping
  AdjustSupportTokens :: UnitKey -> Int -> Message
    -- ^ Add (positive) or remove (negative) tokens from a support's
    -- counter. Clamped to >= 0.
  AdjustQuestTokens :: UnitKey -> Int -> Message
    -- ^ Same for a quest card.
  -- Support destruction
  DestroySupport :: UnitKey -> Message
    -- ^ Remove a free-standing support from play. Card goes to its
    -- controller's discard. Triggers 'SupportLeftPlay'.
  SupportLeftPlay :: PlayerKey -> UnitKey -> CardCode -> Message
  -- Quest destruction (used by Mission-quest sacrifice effects like
  -- Dominion of Chaos).
  DestroyQuest :: UnitKey -> Message
    -- ^ Remove a quest from play; its card goes to the controller's
    -- discard pile. Triggers 'QuestLeftPlay'.
  QuestLeftPlay :: PlayerKey -> UnitKey -> CardCode -> Message
  -- Experiences
  AttachExperience :: UnitKey -> CardCode -> Message
    -- ^ Pin a card (by code) as an "experience" on a host unit. Used by
    -- Skulltaker; the host card text reads 'experiences' to scale.
  -- Tactics
  PlayTactic :: PlayerKey -> UnitKey -> ActionTarget -> Message
    -- ^ Play a Tactic from hand: pay cost, send to discard, fire the
    -- tactic's 'receive' once with 'TacticResolved'. The
    -- 'ActionTarget' is the target the player chose at play time (or
    -- 'NoTarget' for non-targeting tactics).
  TacticResolved :: PlayerKey -> CardCode -> ActionTarget -> Int -> Message
    -- ^ Dispatch hook fired exactly once when a tactic resolves. The
    -- tactic's CardDef.receive is invoked with this message; cards
    -- like Berserk Fury and Blood for the Blood God react here. The
    -- 'ActionTarget' carries the choice from 'PlayTactic'; the 'Int'
    -- carries the X value paid for variable-cost tactics (0 for
    -- fixed-cost tactics).
  -- Prompts
  RequestPrompt :: Prompt -> Message
    -- ^ Suspend the engine with the given prompt. 'gameMain' will
    -- return as long as 'Game.pendingPrompt' is set, allowing the
    -- wire layer to push the state to the client.
  ResolvePrompt :: PromptResult -> Message
    -- ^ Carry the prompted player's answer back into the engine.
    -- Clears 'Game.pendingPrompt' and fires the callback specified
    -- by the prompt.
  -- Card action abilities
  TriggerCardAction :: PlayerKey -> UnitKey -> Int -> ActionTarget -> Message
    -- ^ Trigger the indexed action ability on an in-play card. The
    -- engine validates target (against the card's declared
    -- 'TargetSchema'), debits the resource cost, and fires the
    -- action's effect.
  -- Deferred effects
  DeferDamageToUnitUntilEoT :: UnitKey -> Int -> Message
    -- ^ Schedule N damage to land on the target at end of turn.
  DeferSacrificeUntilEoT :: UnitKey -> Message
    -- ^ Schedule the target unit to be sacrificed (destroyed) at end
    -- of turn. Mirrors 'DeferDamageToUnitUntilEoT' but enqueues a
    -- 'PEDestroyUnit' instead.
  -- Zone damage
  DealDamageToZone :: PlayerKey -> ZoneKind -> Int -> Message
    -- ^ Add N damage tokens to a capital zone. May burn the zone (and
    -- a second burn eliminates the player).
  -- Free unit summons (Iron Throneroom payoff, Reckless Attack, …).
  PutUnitIntoPlay :: PlayerKey -> UnitKey -> ZoneKind -> Message
    -- ^ Like 'PlayUnit' but skips the cost check / payment and pulls
    -- from hand. Used by effects that explicitly bypass the resource
    -- system. The 'UnitKey' is the in-hand card's stable key.
  PutUnitIntoPlayFromDiscard :: PlayerKey -> UnitKey -> ZoneKind -> Message
    -- ^ Same as 'PutUnitIntoPlay' but pulls the card from the
    -- player's discard pile instead of their hand.
  -- Scoped modifiers
  InstallModifier :: Ref Target -> Modifier -> Message
    -- ^ Add a 'Modifier' to the named target. Modifiers stack; multiple
    -- @GainPower n@ entries sum.
  ClearScopedModifiers :: ModifierScope -> Message
    -- ^ Drop every modifier matching the given scope (e.g. clear all
    -- 'EndOfTurn' modifiers at end of turn).
  ScheduleAttackerSacrifice :: Message
    -- ^ Schedule a 'PESacrificeAttackersThisPhase' end-of-phase
    -- effect for the current battlefield phase. Used by Reckless
    -- Attack.
  -- Damage shuffling (Valkia)
  MoveAllDamage :: UnitKey -> UnitKey -> Message
    -- ^ Move all damage on 'fromKey' to 'toKey'. Source unit ends with
    -- 0 damage; destination accumulates.
  MoveDamage :: UnitKey -> UnitKey -> Int -> Message
    -- ^ Move up to N damage from 'fromKey' to 'toKey'. Source heals the
    -- moved amount; destination accumulates. Used by Douse the Flames.
  -- Unit relocation
  MoveUnit :: UnitKey -> ZoneKind -> Message
    -- ^ Relocate an in-play unit to a different zone controlled by
    -- the SAME player. No-op if the destination equals the unit's
    -- current zone. Used by Pistoliers, Forced March, Temple of
    -- Shallya, Johannes Broheim.
  ReturnUnitToHand :: UnitKey -> Message
    -- ^ Remove a unit from play and put its card into its owner's
    -- hand (not discard). Triggers 'UnitLeftPlay' the same way
    -- 'DestroyUnit' does so on-leaves-play hooks fire. Used by
    -- Sigmar's Blessed, Pilgrimage.
  -- Deck manipulation
  MillFromDeck :: PlayerKey -> Int -> Message
    -- ^ Send the top N cards of the named player's deck to their
    -- discard pile. Used by Infiltrate!.
  DiscardHand :: PlayerKey -> Message
    -- ^ Discard every card in the named player's hand. Used by
    -- Will of Tzeentch and Journey to the Gate's sacrifice ability.
  RecycleDiscard :: PlayerKey -> Int -> Message
    -- ^ Shuffle up to N cards from the named player's discard pile
    -- back into their deck (engine picks first N for now; a real
    -- prompt is a follow-up). Prepare for War!.
  -- Development shuffling
  MoveDevelopment :: PlayerKey -> ZoneKind -> ZoneKind -> Message
    -- ^ Move one development from @from@ to @to@ in the named
    -- player's capital. Will of the Electors moves up to two via
    -- two consecutive messages.
  -- Indirect damage
  IndirectDamage :: PlayerKey -> Int -> Message
    -- ^ Allocate N damage across the named player's capital, the
    -- player choosing where it lands. Today the engine auto-routes
    -- it to the player's least-damaged non-burned zone to maximise
    -- survival; a player-driven allocator prompt is a follow-up
    -- (the rules let the player choose).
  -- Combat redirection
  RedirectAttackZone :: ZoneKind -> Message
    -- ^ Sigmar's Intervention. Rewrite the current 'CombatState's
    -- target zone before defenders are declared. No-op outside
    -- combat or if the destination zone is already burning.
  -- Development destruction
  DestroyDevelopment :: PlayerKey -> ZoneKind -> Message
    -- ^ Remove one development from the named zone. The card lands
    -- in the controller's discard so destroy-development effects
    -- (Demolition!, Grimgor Ironhide, Smash-Go-Boom!) route the
    -- facedown card back to the controller's pile.
  FlipDevelopment :: PlayerKey -> ZoneKind -> Message
    -- ^ Reveal the top facedown development in the named zone. If
    -- the underlying card is a Unit it enters play in this zone
    -- and an end-of-turn sacrifice is scheduled; any other type
    -- is sacrificed immediately. Used by Rip Dere 'eads Off!.
  -- Slaanesh's Domination: random hand peek + opt-in free play
  SlaaneshDominate :: PlayerKey -> PlayerKey -> Int -> Message
    -- ^ Caster picks @count@ cards at random from @opp@'s hand and
    -- is asked, per revealed tactic, whether to play it for free.
    -- Cards stay in the opponent's hand. Args: caster, opponent,
    -- number to reveal.
  -- Action cancellation
  ArmActionCancel :: PlayerKey -> Message
    -- ^ Add one "next card action this player fires has its
    -- effect cancelled" token. Cost is still paid; only the body
    -- is suppressed. Bright Wizard Apprentice writes to this.
  -- One-shot next-unit modifiers (We'z Bigga!)
  ScheduleNextUnitDiscount :: PlayerKey -> Int -> Message
    -- ^ Add N to this player's "next unit costs N less" budget.
    -- Resets at end of turn or as soon as the player plays a unit.
  ScheduleNextUnitDamage :: PlayerKey -> Int -> Message
    -- ^ The next unit this player plays enters with N damage.
  -- Legends
  PlayLegend :: PlayerKey -> UnitKey -> Message
    -- ^ Play a legend from hand directly onto the controller's capital
    -- board. Pays cost, removes the card from hand, emits
    -- 'LegendEnteredPlay'. Refused if the controller already has a
    -- legend in play.
  LegendEnteredPlay :: PlayerKey -> UnitKey -> Message
    -- ^ A legend has just entered play. Hook point for legend-side
    -- 'receive' bodies; Game itself just narrates.
  DealDamageToLegend :: UnitKey -> Int -> Message
    -- ^ Apply N damage to a target legend. If accumulated damage
    -- meets or exceeds the legend's printed HP, the engine queues
    -- 'DestroyLegend'.
  DestroyLegend :: UnitKey -> Message
    -- ^ Remove a legend from play; its card goes to the controller's
    -- discard. Triggers 'LegendLeftPlay'.
  LegendLeftPlay :: PlayerKey -> UnitKey -> CardCode -> Message
  -- Unit tokens (Silver Helm Detachment, War Hydra)
  AdjustUnitTokens :: UnitKey -> Int -> Message
    -- ^ Add (positive) or remove (negative) resource tokens on an
    -- in-play unit. Clamped to >= 0.
  -- Draw restriction (Infiltrate the tactic)
  SetDrawCap :: PlayerKey -> Int -> Message
    -- ^ "Target opponent cannot draw more than N cards this turn."
    -- Stored in 'Game.drawCaps'; enforced against the ThisTurn
    -- 'drawnBy' count; cleared at end of turn.
  -- Capital shields (Flagellants, Gifts of Aenarion)
  ArmCapitalShield :: PlayerKey -> Maybe Int -> Int -> Message
    -- ^ Arm a "cancel (the next N | all) damage to your capital this
    -- turn" grant for the named player. Second arg: resources
    -- refunded per point cancelled.
  -- Combat-wide riders
  ArmDefenderCounterstrike :: PlayerKey -> Int -> Message
    -- ^ Ulric's Fury: this player's defending units gain
    -- Counterstrike +N until end of turn.
  SetCombatDamageUncancellable :: Message
    -- ^ Mob Up: combat damage cannot be cancelled until end of turn.
  -- Tactic riders
  ArmFreeTactic :: PlayerKey -> Message
    -- ^ Runefang of Solland: the next non-Epic tactic this player
    -- plays this turn costs 0 (printed part; loyalty still applies).
  ClearTacticDamageContext :: Message
    -- ^ Trailing sentinel queued after a tactic's effect body so the
    -- damage messages it pushed amplify under
    -- 'Game.tacticDamageContext' and later ones don't.
  -- Out-of-pile plays
  PlaySupportFromDiscard :: PlayerKey -> UnitKey -> ZoneKind -> Message
    -- ^ Put the named support card from the player's discard pile
    -- directly into the named zone (Repair the Waystones).
  PutUnitIntoPlayFromDeck :: PlayerKey -> UnitKey -> ZoneKind -> Message
    -- ^ Pull the named unit card out of the player's deck and put it
    -- into play (Empty the Hold). Cost is skipped.
  PutRandomUnitIntoPlayFromDeckTop :: PlayerKey -> Int -> Message
    -- ^ Blessings of Tzeentch: look at the top N cards of the deck;
    -- one unit found there (chosen at random) is put into play in a
    -- zone of the player's choice; the rest shuffle back.
  StealUnitFromDiscard :: PlayerKey -> PlayerKey -> UnitKey -> ZoneKind -> Bool -> Message
    -- ^ Slaver Raid: @StealUnitFromDiscard newController srcPlayer
    -- key zone corrupt@ pulls a unit card out of @srcPlayer@'s
    -- discard pile and puts it into play under @newController@,
    -- optionally corrupted.
  -- Development manipulation
  ReturnDevelopmentToHand :: PlayerKey -> ZoneKind -> Message
    -- ^ Abandoned Mine: pop one development and return the facedown
    -- card to its owner's hand.
  ConvertDepartedToDevelopment :: PlayerKey -> UnitKey -> ZoneKind -> Message
    -- ^ Reclaim the Hold: pull the named card (which just left play)
    -- out of the player's discard pile and place it facedown as a
    -- development in the named zone.
  AnimateDevelopment :: PlayerKey -> ZoneKind -> Int -> Int -> Message
    -- ^ Bolt of Change: a development in the named zone becomes a
    -- unit with the given power and HP until end of turn. The
    -- development count stays put ("it also counts as a
    -- development"); if the animated unit dies, one development from
    -- that zone is destroyed in its stead.
  -- Hand / deck card flow
  TakeCardsFromDeckToHand :: PlayerKey -> [UnitKey] -> Message
    -- ^ Move the named cards from the player's deck into their hand
    -- (Chittering Horde).
  DiscardCardsFromHand :: PlayerKey -> [UnitKey] -> Message
    -- ^ Discard the named cards from the player's hand (Caught the
    -- Scent).
  DiscardCardsFromDeck :: PlayerKey -> [UnitKey] -> Message
    -- ^ Discard the named cards from anywhere in the player's deck,
    -- preserving the order of the rest (Plague Monk).
  ReturnCardsFromDiscardToHand :: PlayerKey -> [UnitKey] -> Message
    -- ^ Move the named cards from the player's discard pile into their
    -- hand (Gift of Life). Mirror of 'TakeCardsFromDeckToHand'.
  -- Attachment / support shuffling
  TransformUnitToAttachment :: UnitKey -> UnitKey -> Message
    -- ^ Vigilant Elector: the first unit leaves the unit pool and
    -- re-enters play as an Attachment on the second. Its synthetic
    -- support def destroys the host at the host controller's end of
    -- turn; the discard pile receives the original unit card when
    -- the attachment leaves play.
  MoveAttachment :: UnitKey -> UnitKey -> Message
    -- ^ Helblaster Crew: detach the named attachment from its host
    -- and attach it to the second unit.
  MoveSupport :: UnitKey -> ZoneKind -> Message
    -- ^ Relocate a free-standing support to another of its
    -- controller's zones (Sigmar's Brilliance).
  -- Control changes
  TakeControlOfUnit :: PlayerKey -> UnitKey -> Message
    -- ^ The named player takes control of the unit; it moves to that
    -- player's corresponding zone (same 'ZoneKind'). Refused when the
    -- hero-per-zone limit blocks the arrival. Used by Veteran
    -- Sellswords, Grasping Darkness, Your Will Is Mine.
  ScheduleControlReturn :: UnitKey -> PlayerKey -> Message
    -- ^ At end of turn, hand control of the named unit to the named
    -- player (Grasping Darkness returns its stolen unit).
  CheckUnitVitals :: UnitKey -> Message
    -- ^ Re-verify that the named unit's damage still meets or exceeds
    -- its effective max HP, and only then queue 'DestroyUnit'. Queued
    -- by the post-message stat sweep instead of a direct destroy so a
    -- unit saved in the meantime (Hydra Blade ransom, Vigilant
    -- Pistoliers relocation) isn't killed by a stale duplicate.
  -- Combat assignment surgery
  RedirectAssignedUnitDamage :: UnitKey -> UnitKey -> Int -> Message
    -- ^ Thick-Skinned: move up to N points of pending cancellable
    -- combat damage from the first unit's assignment onto the
    -- second.
  -- Zone bookkeeping
  HealCapital :: PlayerKey -> Int -> Message
    -- ^ Remove up to N total damage tokens from the named player's
    -- capital, distributed across zones (most-damaged first). Burned
    -- zones are not healed.
  HealZone :: PlayerKey -> ZoneKind -> Int -> Message
    -- ^ Remove up to N damage tokens from one specific zone.
  AddDevelopment :: PlayerKey -> ZoneKind -> Message
    -- ^ Place a facedown development in the named zone. Bypasses the
    -- once-per-turn limit (used by Dwarf Masons, Wake the Mountain).
  PlayDevelopment :: PlayerKey -> UnitKey -> ZoneKind -> Message
    -- ^ The active player's once-per-turn development play. Takes the
    -- named card from hand, places it facedown in the chosen zone,
    -- and trips the per-turn development gate so a second
    -- 'PlayDevelopment' this turn no-ops. Restricted to the player's
    -- own CapitalActionWindow.
  -- Bulk / AoE damage
  DealDamageToEachEnemyUnitInZone :: PlayerKey -> ZoneKind -> Int -> Message
    -- ^ N damage to every enemy unit currently sitting in the named
    -- zone. 'PlayerKey' is the caster's side; the engine deals to
    -- units whose controller is the opponent.
  DealDamageToEachUnitInCombat :: Int -> Message
    -- ^ N damage to every unit currently engaged in combat (attackers
    -- and defenders). No-op outside combat.
  -- Cancellation
  CancelAssignedDamageOnUnit :: UnitKey -> Int -> Message
    -- ^ Defenders of the Faith. Reduce cancellable damage staged
    -- against the named unit by up to N (floor 0). No-op outside
    -- combat or if the unit has no pending assignment.
  CancelAllAssignedDamage :: Message
    -- ^ Master Rune of Valaya. Clear every pending damage assignment
    -- on the in-flight combat (units AND spillover-to-zone). No-op
    -- outside combat.
  -- Hand interaction
  DiscardRandomFromHand :: PlayerKey -> Message
    -- ^ Discard one card chosen at random from the player's hand.
  -- Resources
  GainResources :: PlayerKey -> Int -> Message
    -- ^ Credit N resources to the named player's pool. Used by tactic
    -- effects (Burying the Grudge, …) that bypass the kingdom-phase
    -- collection step.
  SpendResources :: PlayerKey -> Int -> Message
    -- ^ Debit N resources from the named player's pool (clamped to
    -- 0). For action-ability costs that fall outside the normal
    -- play-card pipeline (e.g. Skulltaker's experience attach).
  -- Combat sequence — implemented as a 5-step ladder with an action
  -- window after each step, matching the rulebook. Either client may
  -- act in each window; CloseActionWindow advances to the next step.
  BeginCombat :: PlayerKey -> ZoneKind -> [UnitKey] -> Message
    -- ^ Step 1: Declare target of attack. 'PlayerKey' is the
    -- attacker, 'ZoneKind' is the defending zone, and the list is the
    -- attacking unit keys (committed already so they're known when
    -- the post-step window opens).
  AdvanceCombatToAttackers :: Message
    -- ^ Internal: fired by CloseActionWindow after the
    -- AfterDeclareCombatTarget window closes. Opens the
    -- AfterDeclareAttackers window.
  CancelAttack :: Message
    -- ^ Cancel the current attack: end the combat immediately without
    -- assigning damage and without firing any post-combat ("after
    -- combat damage", Scout, Raider) effects. The action-window stack
    -- has already unwound to the battlefield window, so the engine
    -- simply returns priority there. Used by Test of Will and the
    -- "cancel the attack" half of Fulminating Cage.
  BlockAttacksThisTurn :: PlayerKey -> Message
    -- ^ Bar the named player from declaring another attack for the rest
    -- of this turn (Fulminating Cage). Recorded in
    -- 'Game.attackBlockedThisTurn' and checked by 'BeginCombat'.
  ResolveAmbushStep :: Message
    -- ^ Step 2.5 (Ambush): after Declare Attackers, before Declare
    -- Defenders, offer the defender each affordable facedown
    -- development in the defending zone that carries 'Ambush' X. One
    -- ambush per firing; flipping re-sends this message to offer the
    -- next, then 'AdvanceCombatToDefenders' once the defender declines
    -- or nothing is affordable.
  AmbushDevelopment :: PlayerKey -> ZoneKind -> UnitKey -> Message
    -- ^ Flip a specific facedown development faceup as an ambush: pay
    -- its 'Ambush' X, pop it from the zone, put the card into play as
    -- its printed type (no end-of-turn sacrifice), and — for units —
    -- mark it 'MustDefend' so the upcoming Declare Defenders step
    -- force-includes it. Fires 'UnitEnteredPlay' for its text.
  AdvanceCombatToDefenders :: Message
    -- ^ Internal: opens the AfterDeclareAttackers → defenders
    -- transition, auto-picks defenders (until the defender has a
    -- prompt), fires Counterstrike, opens AfterDeclareDefenders.
  DeclareDefenders :: [UnitKey] -> Message
    -- ^ Defender locks in which of their units block. Auto-picks all
    -- eligible defenders if no list is supplied.
  AdvanceCombatToAssign :: Message
    -- ^ Internal: after the AfterDeclareDefenders window closes,
    -- compute and queue damage assignments. Opens
    -- AfterAssignCombatDamage.
  AdvanceCombatToApply :: Message
    -- ^ Internal: after the AfterAssignCombatDamage window closes,
    -- actually apply queued damage. Opens AfterApplyCombatDamage.
  ResolveCombat :: Message
    -- ^ Legacy entry-point preserved for tests / debug. Equivalent
    -- to firing AdvanceCombatToAssign + AdvanceCombatToApply
    -- back-to-back, skipping the intermediate action windows.
  FireScoutDiscards :: PlayerKey -> PlayerKey -> [UnitKey] -> [UnitKey] -> Message
    -- ^ Post-damage Scout sweep, deferred so that 'surviving Scout'
    -- is evaluated against the post-apply game state. Args:
    -- @attacker@, @defender@, original attacker keys, original
    -- defender keys. For each side, every key still in play whose
    -- 'CardDef' carries 'Scout' forces one random discard from the
    -- opposing player's hand.
  FireRaiderResources :: PlayerKey -> [UnitKey] -> Message
    -- ^ Post-damage Raider payoff, deferred like 'FireScoutDiscards'
    -- so "survived combat" is evaluated against post-apply state.
    -- Args: @attacker@ and the original attacker keys. The attacking
    -- player gains resources equal to the summed 'Raider' X of every
    -- attacker key still in play.
  EndCombat :: Message
    -- ^ Combat ends; clear 'Game.combat'.

deriving stock instance Show Message
