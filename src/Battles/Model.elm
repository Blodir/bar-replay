module Battles.Model exposing (..)

import Battles.Decoders exposing (BattlesResponse)
import Http
import Time

type alias Model =
  { battles: BattlesResponseWrapper
  }

type BattlesResponseWrapper = Failure Http.Error | Loading | Success BattlesResponse

type Msg =
  GotBattle (Result Http.Error BattlesResponse)
  | Tick
