module Model exposing (..)

import Http
import Array exposing (Array)

type Model = Failure | Loading | Success Replays
type Msg = GotReplays (Result Http.Error Replays) | RefreshReplays

type alias Replays =
  { data : Array Replay
  }

type alias Replay =
  { id : String
  , startTime: String
  , durationMs: Int
  , map: { fileName: String }
  , allyTeams: Array
    { winningTeam: Bool
    , players: Array { name: String }
    }
  }

type alias Map =
  { fileName: String
  }

type alias AllyTeams = Array AllyTeam

type alias AllyTeam =
  { winningTeam: Bool
  , players: Array Player
  }

type alias Player = { name: String }

