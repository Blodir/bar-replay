module ReplayDecoder exposing (replayDecoder)

import Array exposing (Array)
import Json.Decode exposing (Decoder, succeed, bool, map, array, field, int, string, bool, map2)
import Json.Decode.Pipeline exposing (required)
import Model exposing (..)

replayDecoder : Decoder ReplayResponse
replayDecoder = map ReplayResponse (field "fileName" string)
