// Wire types for the lobby and per-game WebSockets.
// Mirror of backend/src/Invasion/Server/Protocol.hs. When that file
// changes, this one changes in the same PR.

import type { Capital } from '../lib/race'

export interface UserInfo {
  userId: string
  displayName: string
}

export interface ChatLine {
  from: UserInfo
  text: string
  at: string // ISO timestamp from the server
}

export type Visibility = 'Public' | 'Private'

export type GameStatus = 'StatusWaiting' | 'StatusPlaying' | 'StatusEnded'

// Scheduled-deploy window. While the server holds a non-null value,
// clients render a banner with a countdown to `until` and the server
// refuses new game creation. Cleared by the admin endpoint or by a
// server restart. `until` is an ISO timestamp.
export interface MaintenanceState {
  until: string
  message: string | null
}

// Summary of the deck loaded into a seat. Two shapes share the record:
// a user-built deck carries `deckId` (and no `starterRace`); a pre-built
// starter carries `starterRace` (and a `null` `deckId`). At most one of
// the two is ever non-null.
export interface DeckView {
  deckId: string | null
  starterRace: Race | null
  name: string
  capital: Capital | null
  size: number
}

export interface SeatView {
  seat: PlayerKey
  user: UserInfo
  isHost: boolean
  deck: DeckView | null
}

export interface GameSummary {
  gameId: string
  name: string
  host: UserInfo
  visibility: Visibility
  hasPassword: boolean
  filledSeats: number
  status: GameStatus
  allowSpectators: boolean
  spectatorCount: number
}

export interface GameView {
  gameId: string
  name: string
  host: UserInfo
  visibility: Visibility
  hasPassword: boolean
  allowSpectators: boolean
  spectatorCount: number
  // When true, the waiting room offers a race picker instead of the
  // saved-deck picker; the server loads the pre-built 40-card starter
  // for the chosen race when the game starts.
  useStarterDecks: boolean
  inviteToken: string | null
  seats: SeatView[]
  status: GameStatus
  chat: ChatLine[]
  // The engine snapshot. `null` while the game is waiting; populated
  // once Setup + BeginGame have run. Kept here as a single payload so
  // the renderer can derive everything from one source of truth.
  engine: EngineGame | null
}

// ---------------------------------------------------------------------------
// Engine snapshot
// Mirror of the JSON produced by Invasion.Game's ToJSON instance.

export type PlayerKey = 'Player1' | 'Player2'

export type Phase = 'KingdomPhase' | 'QuestPhase' | 'CapitalPhase' | 'BattlefieldPhase'

export type ZoneKind = 'KingdomZone' | 'QuestZone' | 'BattlefieldZone'

export type CardKind = 'Unit' | 'Support' | 'Quest' | 'Tactic' | 'Legend' | 'DraftFormat'

// Mirror of Invasion.Types.Number: either Fixed n or Variable.
export type EngineNumber =
  | { tag: 'Fixed'; contents: number }
  | { tag: 'Variable' }

// Mirror of Invasion.CardDef.Trait. Enum constructors serialize as their
// bare names (allNullaryToStringTag = True by default).
export type Trait =
  | 'Warrior' | 'Spell' | 'Engineer' | 'Elite' | 'Slayer' | 'Priest' | 'Hero'
  | 'Ranger' | 'Rune' | 'Building' | 'Attachment' | 'Weapon' | 'Siege'
  | 'Daemon' | 'Creature' | 'Sorceror' | 'Knight' | 'Cavalry' | 'Mission'
  | 'QuestTrait' | 'Wasteland' | 'CapitalCenter' | 'Rift' | 'Relic'
  | 'Banner' | 'Goblin' | 'Mage' | 'Mutation' | 'Noble' | 'Shaman'
  | 'Skill' | 'Warpstone' | 'Zealot'
  | 'Skaven' | 'WitchHunter' | 'Hex' | 'Vault' | 'Berserker' | 'Dragon'
  | 'WarMachine' | 'Musician' | 'StandardBearer' | 'Location' | 'Fortification' | 'Epic'
  | 'Troll' | 'WitchElf' | 'Disease'
  | 'Cultist' | 'Bretonnian' | 'Thief' | 'Slave' | 'Environment'

// Card definition as serialized by Invasion.CardDef.ToJSON. The 'receive'
// function field is dropped on the wire — see CardDef.hs.
export interface EngineCardDef {
  code: string
  title: string
  kind: CardKind
  races: Race[]
  cost: EngineNumber
  loyalty: number
  power: number
  hitPoints: EngineNumber | null
  traits: Trait[]
  text: string | null
  flavor: string | null
  keywords: unknown[]
  unique: boolean
}

// A specific card instance (Invasion.Card.Card). Every card in deck,
// hand, discard, or play carries the same stable `key` from setup
// through the end of the game. The frontend uses this key as its CSS
// view-transition name so a card visually morphs as it moves between
// surfaces (hand → zone → discard).
//
// On the wire, the backend flattens the card definition's fields onto
// the same object as `key`, so EngineCard extends EngineCardDef.
export interface EngineCard extends EngineCardDef {
  key: number
}

// In-play unit (Invasion.Entity.UnitDetails). 'key' is the engine's
// 'UnitKey', written as a bare integer on the wire.
export interface EngineUnit {
  key: number
  controller: PlayerKey
  zone: ZoneKind
  cardDef: EngineCardDef
  damage: number
  corrupted: boolean
  attachments: EngineSupport[]
  experiences: string[]
  // Engine-cached effective stats: printed values plus attachments,
  // modifiers, and auras. Recomputed server-side after every message.
  effectivePower: number
  effectiveMaxHP: number
  // True while the unit is a declared attacker / defender in the
  // in-flight combat. Cleared when combat ends.
  attacking: boolean
  defending: boolean
  // Resource tokens sitting on the unit (Silver Helm Detachment,
  // War Hydra).
  tokens: number
  // True while an attached support blanks the unit's printed text box
  // (Witch Hag's Curse). Traits are unaffected.
  blanked: boolean
}

export interface EngineSupport {
  key: number
  controller: PlayerKey
  zone: ZoneKind
  cardDef: EngineCardDef
  attachedTo: number | null
  tokens: number
}

export interface EngineQuest {
  key: number
  controller: PlayerKey
  // Whose play area visually houses this quest. Equal to `controller`
  // for most quests; Dominion of Chaos lives on the opponent's side
  // while remaining under its controller's control.
  zoneOwner: PlayerKey
  cardDef: EngineCardDef
  tokens: number
}

// A legend in play. Legends live on their controller's capital board
// (not inside a zone) but the engine still carries a `zone` tag for
// rendering / attack-routing parity with units. Each player may have at
// most one legend in play at a time.
export interface EngineLegend {
  key: number
  controller: PlayerKey
  zone: ZoneKind
  cardDef: EngineCardDef
  damage: number
}

export interface EngineZone {
  kind: ZoneKind
  developments: number
  damage: number
  burned: boolean
  // hitPoints is derived (8 + developments); not on the wire.
}

export interface EngineCapital {
  kingdom: EngineZone
  quest: EngineZone
  battlefield: EngineZone
}

export type ActionWindowTrigger =
  | 'BeginningOfTurnActionWindow'
  | 'KingdomActionWindow'
  | 'QuestActionWindow'
  | 'CapitalActionWindow'
  | 'BattlefieldActionWindow'
  | 'AfterDeclareCombatTarget'
  | 'AfterDeclareAttackers'
  | 'AfterDeclareDefenders'
  | 'AfterAssignCombatDamage'
  | 'AfterApplyCombatDamage'
  | 'EndOfTurnActionWindow'

export type PassState =
  | { tag: 'NoPasses'; contents: PlayerKey }
  | { tag: 'OnePass'; contents: PlayerKey }

export interface EngineActionWindow {
  trigger: ActionWindowTrigger
  awaiting: PassState
}

export type EliminationReason = 'DeckedOut' | 'CapitalBurned'

export type PlayerLifecycle =
  | { tag: 'IdlePlayer' }
  | { tag: 'Eliminated'; contents: EliminationReason }
  | { tag: 'PlayerDraw'; contents: unknown }

// Currently only Dwarf exists engine-side, but the asset set covers the
// full Warhammer: Invasion race list. Add to this union as we add races
// to backend/src/Invasion/Types.hs.
export type Race =
  | 'Dwarf'
  | 'Empire'
  | 'HighElf'
  | 'Chaos'
  | 'Orc'
  | 'DarkElf'

// A redacted card: the server strips everything but the stable `key`
// from cards the viewer isn't allowed to see (the opponent's hand and
// facedown developments). The key alone is enough to count cards and
// keep animations continuous when a hidden card is later revealed.
export interface HiddenCard {
  key: number
}

export type HandCard = EngineCard | HiddenCard

export function isVisibleCard(c: HandCard): c is EngineCard {
  return (c as EngineCard).code !== undefined
}

export interface EnginePlayer {
  key: PlayerKey
  state: PlayerLifecycle
  capital: EngineCapital
  resources: number
  // Your own hand arrives with full card-def payloads; the opponent's
  // hand is redacted to key-only stubs server-side. Each card carries
  // the stable `key` the engine uses to identify the specific copy —
  // pass it back in `GamePlayCard` to disambiguate duplicates in hand.
  hand: HandCard[]
  // Deck contents and order are hidden from everyone (including the
  // owner): the server sends a list of empty objects so `.length`
  // keeps working but nothing else survives.
  deck: unknown[]
  discard: EngineCard[]
  race: Race
  // Map from hand card key (as a stringified integer — Aeson encodes
  // newtype-around-Int map keys as strings) to the reason the card is
  // currently unplayable. Absent key = card is playable. The server
  // recomputes this on every snapshot publish; treat it as derived
  // state, never write to it locally.
  handPlayability: Record<string, PlayabilityIssue>
}

// Mirrors Invasion.Player.PlayabilityIssue. Every case is encoded as a
// tagged object (allNullaryToStringTag = false on the Haskell side) so
// the discriminator is always `tag`.
export type PlayabilityIssue =
  | { tag: 'InsufficientResources'; contents: [number, number] }
  | { tag: 'UniqueAlreadyInPlay' }
  | { tag: 'LimitedAlreadyPlayed' }
  | { tag: 'LegendAlreadyInPlay' }
  | { tag: 'NotYourTurn' }
  | { tag: 'NotInActionWindow' }
  | { tag: 'WrongActionWindow' }
  | { tag: 'NoValidTarget' }

export type GameLifecycle =
  | { tag: 'GameSetup' }
  | { tag: 'GamePlaying' }
  | {
      tag: 'GameFinished'
      contents: {
        winner: PlayerKey
        reason: 'OpponentDeckedOut' | 'OpponentCapitalBurned'
      }
    }

export type LogCategory =
  | 'LogSystem'
  | 'LogPhase'
  | 'LogTurn'
  | 'LogPlayerAction'
  | 'LogResult'

// Engine-emitted transcript entry. `key` is an i18n key (resolved in
// frontend/src/locales/). `params` carries interpolation values; enum-
// shaped values (player keys, phases, triggers, reasons) are written
// raw and resolved through nested i18n lookups before substitution —
// see `formatLogEntry` in the game view.
export interface LogEntry {
  at: string
  category: LogCategory
  key: string
  params: Record<string, string>
}

export interface EngineGame {
  player1: EnginePlayer
  player2: EnginePlayer
  firstPlayer: PlayerKey
  currentPlayer: PlayerKey
  turn: number
  phase: Phase | null
  // Top of the action-window stack (denormalized; see actionWindowStack).
  actionWindow: EngineActionWindow | null
  // Full stack — combat sub-step windows sit on top of the
  // BattlefieldActionWindow they opened inside of.
  actionWindowStack: EngineActionWindow[]
  modifiers: unknown
  lifecycle: GameLifecycle
  log: LogEntry[]
  units: EngineUnit[]
  supports: EngineSupport[]
  quests: EngineQuest[]
  legends: EngineLegend[]
  nextUnitKey: number
  // The engine pauses while this is set. The seated player named in
  // `prompt.player` is expected to reply with GameResolvePrompt.
  pendingPrompt: EnginePrompt | null
  // In-flight combat (null outside the Battlefield phase or between
  // attacks). The frontend reads this so the player can see who's
  // attacking what during the prompt-driven combat ladder.
  combat: EngineCombatState | null
  // True after the active player has played their once-per-turn
  // development. Resets at BeginTurn. UI uses this to disable the
  // "Play as development" affordance after the player has spent it.
  developmentPlayedThisTurn: boolean
}

// Mirror of Invasion.Game.CombatState. Fields not surfaced by the UI
// (pendingAssignments, attackerPowerPenalty) are still on the wire so
// debug tooling can read them.
export interface EngineCombatState {
  attackingPlayer: PlayerKey
  defendingPlayer: PlayerKey
  targetZone: ZoneKind
  // When set, the attacker is targeting the opposing legend through
  // the named zone (rather than the capital section). Excess damage
  // burns out instead of touching the zone.
  targetLegend: number | null
  attackers: number[]
  defenders: number[]
  attackerPowerPenalty: number
  pendingAssignments: EnginePendingDamage[]
}

export interface EnginePendingDamage {
  target: EnginePendingTarget
  cancellable: number
  uncancellable: number
}

export type EnginePendingTarget =
  | { tag: 'PDUnit'; contents: number }
  | { tag: 'PDZone'; contents: [PlayerKey, ZoneKind] }
  | { tag: 'PDLegend'; contents: number }

// Wire-side mirror of Invasion.Game.Prompt.
export interface EnginePrompt {
  player: PlayerKey
  kind: PromptKind
  callback: unknown // engine-internal tag; the client only renders kind
}

export type PromptKind =
  | {
      tag: 'ChooseUnits'
      filterSpec: PromptFilter
      minPick: number
      maxPick: number
      description: string
    }
  | {
      tag: 'ChooseSacrifice'
      zone: ZoneKind
      optional: boolean
      description: string
    }
  | { tag: 'ChooseYesNo'; description: string }
  | {
      tag: 'ChooseFromCards'
      // Full Card payloads (def fields + stable `key`) for the
      // prompted player; redacted to an empty list for other viewers.
      cards: EngineCard[]
      minPick: number
      maxPick: number
      description: string
    }
  | {
      tag: 'ChooseTargetOption'
      options: TargetOption[]
      description: string
    }
  | {
      tag: 'ChooseAmount'
      minAmount: number
      maxAmount: number
      description: string
    }

export type TargetOption =
  | { tag: 'TargetUnitOption'; contents: number }
  | { tag: 'TargetZoneOption'; contents: [PlayerKey, ZoneKind] }
  | { tag: 'TargetSupportOption'; contents: number }

export type PromptFilter =
  | { tag: 'AnyOwnUnit' }
  | { tag: 'AnyUnitInPlay' }
  | { tag: 'UnitsFromList'; contents: number[] }
  | { tag: 'OwnUnitsFromHandByRace'; contents: Race }
  | { tag: 'OwnUnitsFromDiscardByRace'; contents: Race }
  | { tag: 'OwnUnitsFromHandOrDiscardByRace'; contents: Race }

export type PromptResultWire =
  | { tag: 'PromptUnitsWire'; unitKeys: number[] }
  | { tag: 'PromptBoolWire'; yes: boolean }
  | { tag: 'PromptTargetOptionWire'; option: TargetOption }
  | { tag: 'PromptAmountWire'; amount: number }
  | { tag: 'PromptNoneWire' }

// Derived helpers — keep alongside the wire types so they stay in sync.
export function zoneHitPoints(z: EngineZone): number {
  return 8 + z.developments
}

export function zoneBurning(z: EngineZone): boolean {
  return z.damage >= zoneHitPoints(z)
}

export function priorityHolder(s: PassState): PlayerKey {
  return s.contents
}

// ---------------------------------------------------------------------------
// Lobby socket

export type LobbyIn =
  | { tag: 'LobbyChatSend'; text: string }
  | {
      tag: 'LobbyCreateGame'
      name: string
      visibility: Visibility
      password: string | null
      // Optional: when null, server defaults to true for public games
      // and false for private ones.
      allowSpectators: boolean | null
      // Optional: when true, the engine auto-passes priority for any
      // player whose only option in an action window would be to pass
      // (no Tactic in hand, no in-play card carrying an action).
      // Defaults to false on the server when null.
      autoSkipActionWindows: boolean | null
      // Optional: when true, the waiting room offers a race picker and
      // the server seats the pre-built 40-card starter for the chosen
      // race instead of one of the seated player's saved decks.
      // Defaults to false on the server when null.
      useStarterDecks: boolean | null
    }
  | { tag: 'LobbyJoinPublic'; gameId: string }
  | { tag: 'LobbyJoinWithPassword'; gameId: string; password: string | null }

export type LobbyOut =
  | {
      tag: 'LobbyWelcome'
      // Null for guest connections — they see chat + games but can't
      // post or host.
      you: UserInfo | null
      users: UserInfo[]
      games: GameSummary[]
      chat: ChatLine[]
      maintenance: MaintenanceState | null
    }
  | { tag: 'LobbyChatNew'; line: ChatLine }
  | { tag: 'LobbyUsersUpdate'; users: UserInfo[] }
  | { tag: 'LobbyGamesUpdate'; games: GameSummary[] }
  | { tag: 'LobbyGameCreated'; gameId: string; inviteToken: string | null }
  | { tag: 'LobbyGameJoinOk'; gameId: string; inviteToken: string | null }
  | { tag: 'LobbyMaintenance'; state: MaintenanceState | null }
  | { tag: 'LobbyError'; code: string }

// ---------------------------------------------------------------------------
// Game socket

export type GameIn =
  | { tag: 'GameChatSend'; text: string }
  | { tag: 'GameSelectDeck'; deckId: string }
  // Only valid when the slot was created with `useStarterDecks`.
  | { tag: 'GameSelectStarter'; race: Race }
  | { tag: 'GameClearDeck' }
  | { tag: 'GameStart' }
  | { tag: 'GamePassPriority' }
  | {
      // Play a specific card instance from this seat's hand. `cardKey`
      // is the engine's stable identity for that card (the same `key`
      // value carried on each EngineCard).
      tag: 'GamePlayCard'
      cardKey: number
      zone: ZoneKind | null
      target: number | null
    }
  | { tag: 'GameResolvePrompt'; result: PromptResultWire }
  | {
      // Declare an attack against the opponent. Server accepts this
      // only when the sender holds priority in the
      // BattlefieldActionWindow during their own battlefield phase
      // and no combat is already in flight. The engine then runs the
      // 5-step combat ladder, prompting both players for the
      // sub-step decisions (defenders, Counterstrike target,
      // damage-assignment order) over the existing prompt channel.
      tag: 'GameDeclareAttack'
      attackZone: ZoneKind
      attackerKeys: number[]
    }
  | {
      // Active player's once-per-turn face-down development. Engine
      // refuses outside CapitalActionWindow or after the player has
      // already played one this turn.
      tag: 'GamePlayDevelopment'
      cardKey: number
      developmentZone: ZoneKind
    }
  | { tag: 'GameLeave' }

export type GameOut =
  // Null `you` indicates a guest spectator (no signed-in account).
  | {
      tag: 'GameWelcome'
      you: UserInfo | null
      game: GameView
      maintenance: MaintenanceState | null
    }
  | { tag: 'GameUpdate'; game: GameView }
  | { tag: 'GameChatNew'; line: ChatLine }
  | { tag: 'GameError'; code: string }
  | { tag: 'GameClosed'; reason: string }
  | { tag: 'GameMaintenance'; state: MaintenanceState | null }
