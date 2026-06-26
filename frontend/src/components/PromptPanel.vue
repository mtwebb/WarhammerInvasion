<script setup lang="ts">
// Renders the engine's currently-pending prompt for the seated
// player. The engine pauses while a prompt is set; this panel is the
// only way to resume it. For the non-prompted seat the panel shows a
// "waiting for opponent" notice so spectators / the other player
// know what's happening.

import { computed, ref, watch } from 'vue'
import { useI18n } from 'vue-i18n'
import type {
  EngineCard,
  EngineGame,
  EnginePrompt,
  EngineUnit,
  PlayerKey,
  Race,
  TargetOption,
  ZoneKind,
} from '../api/protocol'
import { isVisibleCard } from '../api/protocol'
import { game } from '../stores/game'

const props = defineProps<{
  engine: EngineGame
  seat: PlayerKey | null
}>()

const { t } = useI18n({ useScope: 'global' })

const prompt = computed<EnginePrompt | null>(() => props.engine.pendingPrompt)

const itsMine = computed(
  () => !!prompt.value && !!props.seat && prompt.value.player === props.seat,
)

// Selected unit keys for ChooseUnits-style prompts.
const picks = ref<number[]>([])
watch(prompt, () => {
  picks.value = []
})

const me = computed(() => {
  if (!props.seat) return null
  return props.seat === 'Player1' ? props.engine.player1 : props.engine.player2
})

interface PickOption {
  key: number
  label: string
}

const options = computed<PickOption[]>(() => {
  if (!prompt.value) return []
  if (prompt.value.kind.tag !== 'ChooseUnits') return []
  const filter = prompt.value.kind.filterSpec
  const m = me.value
  if (!m) return []

  const fromCardList = (cards: EngineCard[], race?: Race): PickOption[] =>
    cards
      .filter((c) => {
        if (c.kind !== 'Unit') return false
        if (race == null) return true
        return c.races.includes(race)
      })
      .map((c) => ({ key: c.key, label: c.title }))

  // The viewer's own hand is always fully visible; the type allows
  // redacted stubs only because the OPPONENT's hand uses the same
  // shape.
  const myHand = (): EngineCard[] => m.hand.filter(isVisibleCard)

  const fromUnits = (units: EngineUnit[]): PickOption[] =>
    units
      .filter((u) => u.controller === props.seat)
      .map((u) => ({ key: u.key, label: u.cardDef.title }))

  switch (filter.tag) {
    case 'AnyOwnUnit':
      return fromUnits(props.engine.units)
    case 'AnyUnitInPlay':
      return props.engine.units.map((u) => ({ key: u.key, label: u.cardDef.title }))
    case 'UnitsFromList': {
      const allowed = new Set(filter.contents)
      // Candidate keys may name a legend (e.g. a legend declared as a
      // zone defender via Descendant of Gods), so check legends too.
      const unitOpts = props.engine.units
        .filter((u) => allowed.has(u.key))
        .map((u) => ({ key: u.key, label: u.cardDef.title }))
      const legendOpts = props.engine.legends
        .filter((l) => allowed.has(l.key))
        .map((l) => ({ key: l.key, label: l.cardDef.title }))
      return [...unitOpts, ...legendOpts]
    }
    case 'OwnUnitsFromHandByRace':
      return fromCardList(myHand(), filter.contents)
    case 'OwnUnitsFromDiscardByRace':
      return fromCardList(m.discard, filter.contents)
    case 'OwnUnitsFromHandOrDiscardByRace':
      return [
        ...fromCardList(myHand(), filter.contents),
        ...fromCardList(m.discard, filter.contents),
      ]
  }
})

// ChooseFromCards: the engine embeds the candidate cards (search
// results, reveals) directly on the prompt. On the wire each entry is
// a full Card (def fields + `key`).
const cardChoices = computed<PickOption[]>(() => {
  if (!prompt.value || prompt.value.kind.tag !== 'ChooseFromCards') return []
  return prompt.value.kind.cards.map((c, i) => ({
    key: (c as EngineCard & { key?: number }).key ?? i,
    label: c.title,
  }))
})

const pickBounds = computed<{ min: number; max: number } | null>(() => {
  const k = prompt.value?.kind
  if (!k) return null
  if (k.tag === 'ChooseUnits' || k.tag === 'ChooseFromCards') {
    return { min: k.minPick, max: k.maxPick }
  }
  return null
})

function togglePick(key: number) {
  const i = picks.value.indexOf(key)
  if (i >= 0) {
    picks.value.splice(i, 1)
  } else if (pickBounds.value && picks.value.length >= pickBounds.value.max) {
    // Already at max — ignore further clicks; deselect first.
  } else {
    picks.value.push(key)
  }
}

function submitUnits() {
  game.resolvePromptUnits(picks.value)
}

function submitNone() {
  game.resolvePromptNone()
}

function submitYes() {
  game.resolvePromptBool(true)
}

function submitNo() {
  game.resolvePromptBool(false)
}

const sacrificeOptions = computed<PickOption[]>(() => {
  if (!prompt.value || prompt.value.kind.tag !== 'ChooseSacrifice') return []
  const zone = prompt.value.kind.zone
  return props.engine.units
    .filter((u) => u.controller === props.seat && u.zone === zone)
    .map((u) => ({ key: u.key, label: u.cardDef.title }))
})

const minOk = computed(() => {
  const b = pickBounds.value
  if (!b) return true
  return picks.value.length >= b.min
})

const amountPick = ref<number>(0)
watch(prompt, () => {
  if (prompt.value?.kind.tag === 'ChooseAmount') {
    amountPick.value = prompt.value.kind.minAmount
  }
})

function submitAmount() {
  game.resolvePromptAmount(amountPick.value)
}

const unitTitleByKey = computed<Map<number, string>>(() => {
  const m = new Map<number, string>()
  for (const u of props.engine.units) m.set(u.key, u.cardDef.title)
  return m
})

// Support titles: free-standing supports plus every attachment.
const supportTitleByKey = computed<Map<number, string>>(() => {
  const m = new Map<number, string>()
  for (const s of props.engine.supports) m.set(s.key, s.cardDef.title)
  for (const u of props.engine.units)
    for (const a of u.attachments) m.set(a.key, a.cardDef.title)
  return m
})

const legendTitleByKey = computed<Map<number, string>>(() => {
  const m = new Map<number, string>()
  for (const l of props.engine.legends) m.set(l.key, l.cardDef.title)
  return m
})

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

function playerLabel(p: PlayerKey): string {
  if (props.seat && p === props.seat) return t('game.prompt.yours')
  return t('game.prompt.opponents')
}

function targetOptionLabel(o: TargetOption): string {
  switch (o.tag) {
    case 'TargetUnitOption':
      return unitTitleByKey.value.get(o.contents) ?? `Unit #${o.contents}`
    case 'TargetSupportOption':
      return supportTitleByKey.value.get(o.contents) ?? `Support #${o.contents}`
    case 'TargetZoneOption': {
      const [owner, zone] = o.contents
      return `${playerLabel(owner)} ${zoneLabel(zone).toLowerCase()}`
    }
    case 'TargetPlayerOption':
      return playerLabel(o.contents)
    case 'TargetLegendOption':
      return legendTitleByKey.value.get(o.contents) ?? `Legend #${o.contents}`
  }
}

function targetOptionKey(o: TargetOption): string {
  switch (o.tag) {
    case 'TargetUnitOption':
      return `u:${o.contents}`
    case 'TargetSupportOption':
      return `s:${o.contents}`
    case 'TargetZoneOption': {
      const [owner, zone] = o.contents
      return `z:${owner}:${zone}`
    }
    case 'TargetPlayerOption':
      return `p:${o.contents}`
    case 'TargetLegendOption':
      return `l:${o.contents}`
  }
}

function submitTargetOption(o: TargetOption) {
  game.resolvePromptTargetOption(o)
}
</script>

<template>
  <aside v-if="prompt" class="prompt-panel" :class="{ 'is-mine': itsMine }">
    <header>
      <strong>{{ itsMine ? t('game.prompt.your_choice') : t('game.prompt.waiting_on_opponent') }}</strong>
    </header>

    <p class="prompt-desc">{{ prompt.kind.description }}</p>

    <!-- ChooseYesNo -->
    <div v-if="prompt.kind.tag === 'ChooseYesNo'" class="actions">
      <template v-if="itsMine">
        <button type="button" @click="submitYes">{{ t('game.prompt.yes') }}</button>
        <button type="button" @click="submitNo">{{ t('game.prompt.no') }}</button>
      </template>
      <template v-else>
        <em>{{ t('game.prompt.waiting') }}</em>
      </template>
    </div>

    <!-- ChooseUnits -->
    <div v-else-if="prompt.kind.tag === 'ChooseUnits'" class="actions">
      <template v-if="itsMine">
        <div class="picks">
          <button
            v-for="o in options"
            :key="o.key"
            type="button"
            class="pick"
            :class="{ selected: picks.includes(o.key) }"
            @click="togglePick(o.key)"
          >
            {{ o.label }}
          </button>
          <p v-if="options.length === 0" class="empty">{{ t('game.prompt.no_eligible') }}</p>
        </div>
        <div class="confirm">
          <button type="button" :disabled="!minOk" @click="submitUnits">
            {{ t('game.prompt.confirm_n', { n: picks.length, max: prompt.kind.maxPick }) }}
          </button>
          <button v-if="prompt.kind.minPick === 0" type="button" @click="submitNone">
            {{ t('game.prompt.skip') }}
          </button>
        </div>
      </template>
      <template v-else>
        <em>{{ t('game.prompt.waiting') }}</em>
      </template>
    </div>

    <!-- ChooseFromCards: search results / reveals. -->
    <div v-else-if="prompt.kind.tag === 'ChooseFromCards'" class="actions">
      <template v-if="itsMine">
        <div class="picks">
          <button
            v-for="o in cardChoices"
            :key="o.key"
            type="button"
            class="pick"
            :class="{ selected: picks.includes(o.key) }"
            @click="togglePick(o.key)"
          >
            {{ o.label }}
          </button>
          <p v-if="cardChoices.length === 0" class="empty">{{ t('game.prompt.no_eligible') }}</p>
        </div>
        <div class="confirm">
          <button type="button" :disabled="!minOk" @click="submitUnits">
            {{ t('game.prompt.confirm_n', { n: picks.length, max: prompt.kind.maxPick }) }}
          </button>
          <button v-if="prompt.kind.minPick === 0" type="button" @click="submitNone">
            {{ t('game.prompt.skip') }}
          </button>
        </div>
      </template>
      <template v-else>
        <em>{{ t('game.prompt.waiting') }}</em>
      </template>
    </div>

    <!-- ChooseAmount -->
    <div v-else-if="prompt.kind.tag === 'ChooseAmount'" class="actions">
      <template v-if="itsMine">
        <div class="amount-row">
          <input
            type="number"
            :min="prompt.kind.minAmount"
            :max="prompt.kind.maxAmount"
            v-model.number="amountPick"
            class="amount-input"
          />
          <span class="amount-bounds">
            ({{ prompt.kind.minAmount }}–{{ prompt.kind.maxAmount }})
          </span>
        </div>
        <div class="confirm">
          <button
            type="button"
            :disabled="
              amountPick < prompt.kind.minAmount ||
              amountPick > prompt.kind.maxAmount
            "
            @click="submitAmount"
          >
            {{ t('game.prompt.confirm') }}
          </button>
        </div>
      </template>
      <template v-else>
        <em>{{ t('game.prompt.waiting') }}</em>
      </template>
    </div>

    <!-- ChooseTargetOption -->
    <div v-else-if="prompt.kind.tag === 'ChooseTargetOption'" class="actions">
      <template v-if="itsMine">
        <div class="picks">
          <button
            v-for="o in prompt.kind.options"
            :key="targetOptionKey(o)"
            type="button"
            class="pick"
            @click="submitTargetOption(o)"
          >
            {{ targetOptionLabel(o) }}
          </button>
          <p v-if="prompt.kind.options.length === 0" class="empty">
            {{ t('game.prompt.no_targets') }}
          </p>
        </div>
      </template>
      <template v-else>
        <em>{{ t('game.prompt.waiting') }}</em>
      </template>
    </div>

    <!-- ChooseSacrifice -->
    <div v-else-if="prompt.kind.tag === 'ChooseSacrifice'" class="actions">
      <template v-if="itsMine">
        <div class="picks">
          <button
            v-for="o in sacrificeOptions"
            :key="o.key"
            type="button"
            class="pick"
            :class="{ selected: picks.includes(o.key) }"
            @click="picks = [o.key]"
          >
            {{ o.label }}
          </button>
          <p v-if="sacrificeOptions.length === 0" class="empty">{{ t('game.prompt.no_eligible') }}</p>
        </div>
        <div class="confirm">
          <button
            type="button"
            :disabled="picks.length === 0 && !prompt.kind.optional && sacrificeOptions.length > 0"
            @click="picks.length > 0 ? submitUnits() : submitNone()"
          >
            {{ picks.length > 0 ? t('game.prompt.sacrifice') : t('game.prompt.skip') }}
          </button>
        </div>
      </template>
      <template v-else>
        <em>{{ t('game.prompt.waiting') }}</em>
      </template>
    </div>
  </aside>
</template>

<style scoped>
.prompt-panel {
  background: var(--bg-elev-2, #2a2a32);
  border: 1px solid var(--border, #555);
  border-radius: var(--radius-md, 6px);
  padding: 0.75rem 1rem;
  color: var(--fg, #f5f5f5);
}
.prompt-panel.is-mine {
  border-color: var(--accent-strong, #f6b04c);
  box-shadow: 0 0 8px rgba(224, 119, 94, 0.4);
}
.prompt-desc {
  margin: 0.25rem 0 0.5rem;
}
.actions .picks {
  display: flex;
  flex-wrap: wrap;
  gap: 0.35rem;
  margin-bottom: 0.5rem;
  max-height: 40dvh;
  overflow-y: auto;
}
.pick {
  background: var(--bg, #3a3a44);
  color: var(--fg, #f5f5f5);
  border: 1px solid var(--border, #555);
  padding: 0.25rem 0.55rem;
  border-radius: var(--radius-sm, 4px);
  cursor: pointer;
  min-height: 44px;
}
.pick.selected {
  background: var(--accent, #f6b04c);
  color: var(--on-accent, #1c1c20);
  border-color: var(--accent, #f6b04c);
}
.confirm {
  display: flex;
  gap: 0.5rem;
}
.amount-row {
  display: flex;
  gap: 0.5rem;
  align-items: center;
  margin-bottom: 0.5rem;
}
.amount-input {
  width: 5rem;
  min-height: 44px;
  background: var(--bg, #3a3a44);
  color: var(--fg, #f5f5f5);
  border: 1px solid var(--border, #555);
  border-radius: var(--radius-sm, 4px);
  padding: 0 0.6rem;
  font-size: 1rem;
}
.amount-bounds {
  color: var(--fg-faint, #aaa);
  font-size: 0.85rem;
}
.confirm button {
  min-height: 44px;
  padding: 0 1rem;
  background: var(--accent, #f6b04c);
  color: var(--on-accent, #1c1c20);
  border: 0;
  border-radius: var(--radius-sm, 4px);
  cursor: pointer;
}
.confirm button:disabled {
  background: var(--bg, #555);
  color: var(--fg-faint, #999);
  cursor: not-allowed;
}
.empty {
  color: var(--fg-faint, #888);
  font-style: italic;
  margin: 0;
}
</style>
