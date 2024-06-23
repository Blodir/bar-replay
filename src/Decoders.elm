module Decoders exposing (replaysDecoder)

import Array exposing (Array)
import Json.Decode exposing (Decoder, succeed, bool, map, array, field, int, string, bool, map2)
import Json.Decode.Pipeline exposing (required)
import Model exposing (..)

replaysDecoder : Decoder Replays
replaysDecoder = map Replays (field "data" replaysDataDecoder)

replaysDataDecoder : Decoder (Array Replay)
replaysDataDecoder = array replayDecoder

replayDecoder : Decoder Replay
replayDecoder =
  succeed Replay
    |> required "id" string
    |> required "startTime" string
    |> required "durationMs" int
    |> required "Map" mapDecoder
    |> required "AllyTeams" allyTeamsDecoder

mapDecoder : Decoder Map
mapDecoder = map Map (field "fileName" string)

allyTeamsDecoder : Decoder AllyTeams
allyTeamsDecoder = array
  ( map2 AllyTeam
    (field "winningTeam" bool)
    (field "Players" (array (map Player (field "name" string))))
  )

