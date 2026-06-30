// Per-game store: holds the latest GameView pushed by the server.
//
// The store is *assigned to* by server frames. The view layer never
// mutates GameView directly — every change is a server round-trip.

import { computed, ref } from 'vue'
import { openSocket, type SocketStatus, type TypedSocket } from '../api/socket'
import type {
  ChatLine,
  GameIn,
  GameOut,
  GameView,
  MaintenanceState,
  Race,
  TargetOption,
  UserInfo,
  ZoneKind,
} from '../api/protocol'
import { auth } from './auth'
import { diffSnapshots } from './boardFx'

const _status = ref<SocketStatus>('idle')
const _gameId = ref<string | null>(null)
const _you = ref<UserInfo | null>(null)
const _view = ref<GameView | null>(null)
const _maintenance = ref<MaintenanceState | null>(null)
const _lastError = ref<{ code: string; at: number } | null>(null)
const _closed = ref<{ reason: string; at: number } | null>(null)

let socket: TypedSocket<GameIn> | null = null

function reset() {
  _status.value = 'idle'
  _gameId.value = null
  _you.value = null
  _view.value = null
  _maintenance.value = null
  _lastError.value = null
  _closed.value = null
}

function handle(msg: GameOut) {
  switch (msg.tag) {
    case 'GameWelcome':
      _you.value = msg.you
      _view.value = msg.game
      _maintenance.value = msg.maintenance
      break
    case 'GameUpdate':
      // Publish semantic diffs BEFORE the snapshot swap so board-FX
      // subscribers can still measure the old DOM (e.g. the final
      // position of a unit that's about to leave play). Card movement
      // itself is animated by PlaySide's GSAP FLIP pass during the
      // re-render that follows this assignment.
      diffSnapshots(_view.value?.engine ?? null, msg.game.engine ?? null)
      _view.value = msg.game
      break
    case 'GameChatNew':
      if (_view.value) {
        const next: ChatLine[] = [..._view.value.chat, msg.line].slice(-200)
        _view.value = { ..._view.value, chat: next }
      }
      break
    case 'GameError':
      _lastError.value = { code: msg.code, at: Date.now() }
      break
    case 'GameClosed':
      _closed.value = { reason: msg.reason, at: Date.now() }
      break
    case 'GameMaintenance':
      _maintenance.value = msg.state
      break
  }
}

interface ConnectOpts {
  gameId: string
  inviteToken?: string | null
  password?: string | null
}

function connect(opts: ConnectOpts) {
  if (socket && _gameId.value === opts.gameId) return
  if (socket) {
    socket.close()
    socket = null
  }
  reset()
  // Wait for auth bootstrap so the first connect carries the right
  // identity (signed-in user with their JWT, or guest spectator). A
  // null token is valid here — the server accepts guests.
  if (!auth.ready.value) return
  _gameId.value = opts.gameId

  const params = new URLSearchParams()
  if (opts.inviteToken) params.set('t', opts.inviteToken)
  if (opts.password) params.set('password', opts.password)
  const qs = params.toString()
  const url = `/ws/games/${opts.gameId}${qs ? `?${qs}` : ''}`

  socket = openSocket<GameOut, GameIn>(
    { url, getToken: () => auth.accessToken.value },
    {
      onMessage: handle,
      onStatusChange: (s) => {
        _status.value = s
      },
      // Access tokens have a short TTL (~15min); without this, a
      // dropped socket after expiry would reconnect forever with the
      // stale token baked into the URL and be rejected as `forbidden`.
      beforeReconnect: async () => {
        await auth.refresh()
        return true
      },
    },
  )
}

function disconnect() {
  if (socket) {
    socket.close()
    socket = null
  }
  reset()
}

function sendChat(text: string) {
  socket?.send({ tag: 'GameChatSend', text })
}

function selectDeck(deckId: string) {
  socket?.send({ tag: 'GameSelectDeck', deckId })
}

function selectStarter(race: Race) {
  socket?.send({ tag: 'GameSelectStarter', race })
}

function clearDeck() {
  socket?.send({ tag: 'GameClearDeck' })
}

function startGame() {
  socket?.send({ tag: 'GameStart' })
}

function leaveGame() {
  socket?.send({ tag: 'GameLeave' })
}

function passPriority() {
  socket?.send({ tag: 'GamePassPriority' })
}

function playCard(cardKey: number, zone: ZoneKind | null = null, target: number | null = null) {
  socket?.send({ tag: 'GamePlayCard', cardKey, zone, target })
}

function resolvePromptUnits(unitKeys: number[]) {
  socket?.send({
    tag: 'GameResolvePrompt',
    result: { tag: 'PromptUnitsWire', unitKeys },
  })
}

function resolvePromptBool(yes: boolean) {
  socket?.send({
    tag: 'GameResolvePrompt',
    result: { tag: 'PromptBoolWire', yes },
  })
}

function resolvePromptNone() {
  socket?.send({
    tag: 'GameResolvePrompt',
    result: { tag: 'PromptNoneWire' },
  })
}

function resolvePromptAmount(amount: number) {
  socket?.send({
    tag: 'GameResolvePrompt',
    result: { tag: 'PromptAmountWire', amount },
  })
}

function resolvePromptTargetOption(option: TargetOption) {
  socket?.send({
    tag: 'GameResolvePrompt',
    result: { tag: 'PromptTargetOptionWire', option },
  })
}

function resolvePromptTrait(trait: string) {
  socket?.send({
    tag: 'GameResolvePrompt',
    result: { tag: 'PromptTraitWire', trait },
  })
}

function declareAttack(attackZone: ZoneKind, attackerKeys: number[]) {
  socket?.send({ tag: 'GameDeclareAttack', attackZone, attackerKeys })
}

function playDevelopment(cardKey: number, developmentZone: ZoneKind) {
  socket?.send({ tag: 'GamePlayDevelopment', cardKey, developmentZone })
}

export const game = {
  status: computed(() => _status.value),
  gameId: computed(() => _gameId.value),
  you: computed(() => _you.value),
  view: computed(() => _view.value),
  // Convenience: pulls the engine snapshot off the latest view. Null
  // until the game is started.
  engine: computed(() => _view.value?.engine ?? null),
  maintenance: computed(() => _maintenance.value),
  lastError: computed(() => _lastError.value),
  closed: computed(() => _closed.value),
  connect,
  disconnect,
  sendChat,
  selectDeck,
  selectStarter,
  clearDeck,
  startGame,
  passPriority,
  playCard,
  resolvePromptUnits,
  resolvePromptBool,
  resolvePromptNone,
  resolvePromptAmount,
  resolvePromptTargetOption,
  resolvePromptTrait,
  declareAttack,
  playDevelopment,
  leaveGame,
}
