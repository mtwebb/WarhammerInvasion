<script setup lang="ts">
// Mounted while the seated player holds priority in the
// BattlefieldActionWindow during their own battlefield phase and no
// combat is already in flight. Lets the attacker declare a target
// zone + attacker subset; sends GameDeclareAttack, the engine takes
// over from there (it'll prompt the defender for blockers via the
// existing PromptPanel).

import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import type { EngineGame, EngineUnit, PlayerKey, ZoneKind } from '../api/protocol'
import { priorityHolder } from '../api/protocol'
import { game } from '../stores/game'

const props = defineProps<{
  engine: EngineGame
  seat: PlayerKey | null
}>()

const { t } = useI18n({ useScope: 'global' })

const aw = computed(() => props.engine.actionWindow)
const priorityIsMe = computed(
  () =>
    !!aw.value &&
    !!props.seat &&
    priorityHolder(aw.value.awaiting) === props.seat,
)

const isMyBattlefieldWindow = computed(
  () =>
    aw.value?.trigger === 'BattlefieldActionWindow'
    && props.engine.phase === 'BattlefieldPhase'
    && priorityIsMe.value
    && props.engine.currentPlayer === props.seat,
)

const noBlockingPrompt = computed(() => props.engine.pendingPrompt === null)

// My battlefield units that are valid attack candidates (not corrupt).
// We don't try to mirror every per-card 'canAttackZone' rule here — the
// engine re-checks and silently drops ineligible attackers — but the
// common cases (corruption, no units in battlefield) we surface so the
// UI doesn't lie to the player.
const eligibleAttackers = computed<EngineUnit[]>(() => {
  const s = props.seat
  if (!s) return []
  return props.engine.units.filter(
    (u) => u.controller === s && u.zone === 'BattlefieldZone' && !u.corrupted,
  )
})

// Attack candidates the player can pick: battlefield units plus their
// legend (which fights from the battlefield using its battlefield-zone
// power) when it isn't corrupt. Legend keys share the unit key space, so
// the engine accepts them in the same attackerKeys list.
interface AttackerOption {
  key: number
  title: string
}
const attackerOptions = computed<AttackerOption[]>(() => {
  const s = props.seat
  const opts: AttackerOption[] = eligibleAttackers.value.map((u) => ({
    key: u.key,
    title: u.cardDef.title,
  }))
  const leg = s ? props.engine.legends.find((l) => l.controller === s) : undefined
  if (leg && !leg.corrupted) {
    opts.push({ key: leg.key, title: leg.cardDef.title })
  }
  return opts
})

const canDeclare = computed(
  () =>
    isMyBattlefieldWindow.value
    && noBlockingPrompt.value
    && attackerOptions.value.length > 0,
)

// Component visibility: render only when the seated player could
// plausibly declare an attack right now. We still show the "no
// eligible attackers" hint when the window is open but they have
// nothing in their battlefield — clearer than silently hiding.
const visible = computed(
  () =>
    isMyBattlefieldWindow.value
    && noBlockingPrompt.value,
)

// ---- local UI state ----

type Stage = 'idle' | 'pick-zone' | 'pick-attackers'
const stage = ref<Stage>('idle')
const pickedZone = ref<ZoneKind | null>(null)
const pickedAttackers = ref<number[]>([])

// Reset whenever it stops being our window (we pass, opponent acts,
// phase changes, …). This guards against leftover state after the
// engine moves on without us submitting.
watch(visible, (v) => {
  if (!v) {
    stage.value = 'idle'
    pickedZone.value = null
    pickedAttackers.value = []
  }
})

function startDeclare() {
  pickedZone.value = null
  pickedAttackers.value = []
  stage.value = 'pick-zone'
}

function cancel() {
  stage.value = 'idle'
  pickedZone.value = null
  pickedAttackers.value = []
}

function pickZone(z: ZoneKind) {
  pickedZone.value = z
  stage.value = 'pick-attackers'
}

function toggleAttacker(key: number) {
  const i = pickedAttackers.value.indexOf(key)
  if (i >= 0) pickedAttackers.value.splice(i, 1)
  else pickedAttackers.value.push(key)
}

function confirm() {
  if (!pickedZone.value || pickedAttackers.value.length === 0) return
  game.declareAttack(pickedZone.value, pickedAttackers.value.slice())
  cancel()
}

const zones: ZoneKind[] = ['KingdomZone', 'QuestZone', 'BattlefieldZone']

function zoneLabel(z: ZoneKind): string {
  switch (z) {
    case 'KingdomZone':
      return t('game.play.capital.kingdom')
    case 'QuestZone':
      return t('game.play.capital.quest')
    case 'BattlefieldZone':
      return t('game.play.capital.battlefield')
  }
}
</script>

<template>
  <aside v-if="visible" class="combat-declare">
    <header>
      <strong>{{ t('game.play.combat.declare_heading') }}</strong>
    </header>

    <!-- Idle: either offer the button or explain why we can't. -->
    <template v-if="stage === 'idle'">
      <p v-if="!canDeclare" class="hint">
        {{ t('game.play.combat.no_attackers') }}
      </p>
      <div v-else class="actions">
        <button type="button" class="primary" @click="startDeclare">
          {{ t('game.play.combat.declare_button') }}
        </button>
      </div>
    </template>

    <!-- Step 1: pick target zone. -->
    <template v-else-if="stage === 'pick-zone'">
      <p class="hint">{{ t('game.play.combat.pick_zone') }}</p>
      <div class="picks">
        <button
          v-for="z in zones"
          :key="z"
          type="button"
          class="pick"
          @click="pickZone(z)"
        >
          {{ zoneLabel(z) }}
        </button>
      </div>
      <div class="actions">
        <button type="button" class="ghost" @click="cancel">
          {{ t('game.play.action.cancel') }}
        </button>
      </div>
    </template>

    <!-- Step 2: pick attackers. -->
    <template v-else>
      <p class="hint">
        {{ t('game.play.combat.pick_attackers', { zone: zoneLabel(pickedZone!) }) }}
      </p>
      <div class="picks">
        <button
          v-for="o in attackerOptions"
          :key="o.key"
          type="button"
          class="pick"
          :class="{ selected: pickedAttackers.includes(o.key) }"
          @click="toggleAttacker(o.key)"
        >
          {{ o.title }}
        </button>
        <p v-if="attackerOptions.length === 0" class="empty">
          {{ t('game.play.combat.no_attackers') }}
        </p>
      </div>
      <div class="actions">
        <button
          type="button"
          class="primary"
          :disabled="pickedAttackers.length === 0"
          @click="confirm"
        >
          {{ t('game.play.combat.confirm', { n: pickedAttackers.length }) }}
        </button>
        <button type="button" class="ghost" @click="cancel">
          {{ t('game.play.action.cancel') }}
        </button>
      </div>
    </template>
  </aside>
</template>

<style scoped>
.combat-declare {
  background: var(--bg-elev, #2a2a32);
  border: 2px solid var(--accent, #c4634a);
  border-radius: var(--radius-md, 6px);
  padding: 0.6rem 0.85rem;
  color: var(--fg, #f5f5f5);
  display: flex;
  flex-direction: column;
  gap: 0.4rem;
}
.combat-declare header {
  font-size: 0.78rem;
  letter-spacing: 0.1em;
  text-transform: uppercase;
  color: var(--fg-faint, #aaa);
}
.hint {
  margin: 0;
  font-size: 0.85rem;
  line-height: 1.3;
}
.picks {
  display: flex;
  flex-wrap: wrap;
  gap: 0.35rem;
}
.pick {
  background: var(--bg, #3a3a44);
  color: var(--fg, #f5f5f5);
  border: 1px solid var(--border, #555);
  padding: 0.3rem 0.6rem;
  border-radius: var(--radius-sm, 4px);
  cursor: pointer;
  min-height: 44px;
  font-size: 0.82rem;
}
.pick.selected {
  background: var(--accent, #f6b04c);
  color: var(--on-accent, #1c1c20);
  border-color: var(--accent, #f6b04c);
}
.empty {
  margin: 0;
  color: var(--fg-faint, #888);
  font-style: italic;
  font-size: 0.85rem;
}
.actions {
  display: flex;
  gap: 0.5rem;
  flex-wrap: wrap;
}
.actions button {
  min-height: 36px;
  padding: 0 0.9rem;
  font-size: 0.82rem;
  border-radius: var(--radius-sm, 4px);
  cursor: pointer;
}
.actions button.primary {
  background: var(--accent, #f6b04c);
  color: var(--on-accent, #1c1c20);
  border: 1px solid var(--accent, #f6b04c);
}
.actions button.primary:disabled {
  background: var(--border, #555);
  color: var(--fg-faint, #999);
  border-color: var(--border, #555);
  cursor: not-allowed;
}
.actions button.ghost {
  background: transparent;
  color: var(--fg-dim, #ccc);
  border: 1px solid var(--border, #555);
}
.actions button.ghost:hover {
  color: var(--fg, #f5f5f5);
  border-color: var(--accent-strong, #c4634a);
}
</style>
