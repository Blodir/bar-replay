module Replays.Model exposing (..)

import Debounce exposing (Debounce)
import Array exposing (Array)
import Http
import Time exposing (Posix)

type alias Model =
  { replays: ReplaysWrapper
  , playerFilter: Maybe String
  , debounce: Debounce String
  }

type Msg =
    GotReplays (Result Http.Error ReplaysResponse)
  | GotReplay (Result Http.Error ReplayResponse)
  | RefreshReplays
  | PlayerFilterChange String
  | PlayerFilterInput String
  | DownloadReplay String
  | DebounceMsg Debounce.Msg
  | NoOp
  | LoadMore
  | Tick

type ReplaysWrapper = Failure | Loading | Success Replays

type alias Replays =
  { lastResponse: ReplaysResponse
  , loadingMore: Bool
  , accumulated: Array ReplaysResponseReplay
  }

type alias ReplaysResponse =
  { data : Array ReplaysResponseReplay
  , page: Int
  }

type alias ReplaysResponseReplay =
  { id : String
  , startTime: String
  , durationMs: Int
  , map: { fileName: String }
  , allyTeams: Array
    { winningTeam: Bool
    , players: Array { name: String }
    }
  }

type alias ReplaysResponseMap =
  { fileName: String
  }

type alias AllyTeams = Array AllyTeam

type alias AllyTeam =
  { winningTeam: Bool
  , players: Array Player
  }

type alias Player = { name: String }

type alias ReplayResponse =
  { fileName: String
  }
