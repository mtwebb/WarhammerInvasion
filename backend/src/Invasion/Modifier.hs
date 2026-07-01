{-# LANGUAGE TemplateHaskell #-}

module Invasion.Modifier (module Invasion.Modifier) where

import Data.Aeson.TH
import Invasion.Prelude
import Invasion.Types (Keyword, UnitKey)

-- | Atomic effect a 'Modifier' carries on its target. Each constructor
-- is interpreted in one specific place in the engine — see
-- 'recomputeUnitStats' for power/HP math and the combat declarators
-- for the 'CannotAttack' / 'CannotDefend' gates.
data ModifierDetails
  = GainPower Int
    -- ^ @+N@ printed-power. Negative values debuff. Sums across
    -- stacked modifiers.
  | LoseHitPoints Int
    -- ^ @-N@ printed hit points (Vile Sorceress, Horrific Mutation).
    -- Negative HP cannot reduce a unit below 1.
  | CannotAttack
    -- ^ Excludes the unit from the legal attacker pool (Franz's Decree).
  | CannotDefend
    -- ^ Excludes the unit from the legal defender pool.
  | CannotBeCorrupted
    -- ^ The unit ignores 'CorruptUnit' messages (Blessing of Isha).
  | Blanked
    -- ^ The unit's printed text box is treated as blank (except Traits)
    -- while this modifier holds (Feared X). Folds into the same
    -- 'blanked' flag the support-side 'blanksHost' drives.
  | RedirectedThisTurn
    -- ^ A marker that some once-per-turn redirect has fired for
    -- this unit (Warrior Priests). Cleared at end of turn via
    -- 'EndOfTurn'-scoped install.
  | ActionUsedThisTurn
    -- ^ Marker that this in-play card's once-per-turn action has
    -- fired. Cards like Archmage of Saphery / Rock Lobber set this
    -- inside their action body and bail early if it's present.
  | GainHitPoints Int
    -- ^ @+N@ printed hit points until the scope expires (We Need Your
    -- Blood's beneficiary). Negative values are expressed with
    -- 'LoseHitPoints' instead.
  | DamageShield Int
    -- ^ "Cancel the next N damage that would be dealt to this unit."
    -- (Steel's Bane.) Consumed point-by-point on the cancellable
    -- damage path; the engine rewrites the remaining count in place.
  | RedirectShield Int UnitKey
    -- ^ "The next N damage dealt to this unit are redirected to
    -- [other unit]." (Blessing of Valaya.) Consumed like
    -- 'DamageShield'; the claimed points are re-dealt to the carried
    -- target. Expires silently if the target has left play.
  | LoseAllToughness
    -- ^ "This unit loses all Toughness." (Morathi's Pegasus's
    -- opponent-triggered ability.) 'totalToughness' returns 0 while
    -- present.
  | LoseAllPower
    -- ^ "This unit loses all power until the end of the turn."
    -- (Morathi.) 'computePower' returns 0 while present.
  | TargetTaxBonus Int
    -- ^ "Cancel any other action that targets this unit unless the
    -- action's controller pays an additional N resources." (Iron
    -- Discipline.) Summed into 'extraTargetTax' for any caster.
  | GainCombatDamage Int
    -- ^ "This unit deals +N damage in combat." (Naggaroth Spearmen.)
    -- Added by 'combatDamageOf' on top of effective power.
  | GainToughness Int
    -- ^ "This unit gains Toughness N" for the modifier's scope
    -- (Fearless in Battle). Summed into 'totalToughness' alongside the
    -- printed keyword and auras.
  | GainCounterstrike Int
    -- ^ "This unit gains Counterstrike N" for the modifier's scope
    -- (Celestial Wizard Acolyte). Summed into 'totalCounterstrike'
    -- alongside the printed keyword and 'selfCounterstrikeBonus'.
  | GainSavage Int
    -- ^ "This unit gains Savage N" for the modifier's scope (Savage
    -- Rush, Track the Prey). Summed into 'totalSavage' alongside the
    -- printed keyword and attachment grants.
  | CanDefendAnyZone
    -- ^ "This unit can defend any of its controller's zones." (Shield of
    -- the Gods.) Expands the defender candidate pool past the attacked
    -- zone, like the quester-defends-any-zone quest extra.
  | SavageDefenseBonus
    -- ^ "While defending, this unit deals +X combat damage where X is
    -- its Savage value." (Shield of the Gods.) Read by 'combatDamageOf'
    -- for a defending unit; the bonus is 'totalSavage'.
  | CannotBeTargeted Bool
    -- ^ "This card cannot be targeted by card effects." The 'Bool' is
    -- @opponentOnly@: 'True' blocks only the controller's opponent
    -- (Shield of Saphery, Tor Elyr); 'False' blocks every player
    -- including the controller (the self-protecting attachments).
    -- Applies to units AND supports — both share the 'UnitKey' space —
    -- and is consulted when enumerating target options.
  | MustDefend
    -- ^ "Target unit must defend this turn, if able." (Animosity,
    -- Alluring Daemonettes.) The defender-declaration step force-
    -- includes eligible units carrying this marker.
  | GainKeyword Keyword
    -- ^ "This unit gains [keyword] until the scope expires."
    -- (Swift-moving Storm grants Scout.) Folded into 'unitKeywords'
    -- via the unit's cached 'grantedKeywords' during recompute.
  | ActingAsDevelopment
    -- ^ "This unit becomes a development (no longer counts as a unit)"
    -- until the scope expires (Tree Kin, Thornflesh Dryad, Treeman
    -- Ancient). Pairs with disable-attack/defend, lose-all-power, and
    -- untargetable modifiers; this marker adds it to its zone's burn
    -- threshold as a development.
  deriving stock (Show, Eq)

-- TODO: add an 'EndOfPhase' scope. Many cards read "until the end of
-- the phase" (e.g. Wolf Chariot, Vaedra Bloodsworn, Maid of Sigmar's
-- buff family, and the skipped Get Outta My Way! / Cavalry Raid).
-- These currently use 'EndOfTurn' as an approximation, which is correct
-- only because each phase has at most one combat today — it breaks down
-- for any effect that should expire before a later same-turn phase, or
-- for multi-combat phases. Implement a real phase-scoped expiry
-- (cleared on 'EndPhase') and migrate the approximating cards to it.
data ModifierScope = EndOfTurn | Permanent
  deriving stock (Show, Eq)

data Modifier = Modifier
  { details :: ModifierDetails
  , scope :: ModifierScope
  }
  deriving stock (Show, Eq)

mconcat
  [ deriveToJSON defaultOptions ''Modifier
  , deriveToJSON defaultOptions ''ModifierDetails
  , deriveToJSON defaultOptions ''ModifierScope
  ]
