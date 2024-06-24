module ReplaysDecoder exposing (replaysDecoder)

import Array exposing (Array)
import Json.Decode exposing (Decoder, succeed, bool, map, array, field, int, string, bool, map2)
import Json.Decode.Pipeline exposing (required)
import Model exposing (..)

replaysDecoder : Decoder ReplaysResponse
replaysDecoder = map2 ReplaysResponse (field "data" replaysDataDecoder) (field "page" int)

replaysDataDecoder : Decoder (Array ReplaysResponseReplay)
replaysDataDecoder = array replayDecoder

replayDecoder : Decoder ReplaysResponseReplay
replayDecoder =
  succeed ReplaysResponseReplay
    |> required "id" string
    |> required "startTime" string
    |> required "durationMs" int
    |> required "Map" mapDecoder
    |> required "AllyTeams" allyTeamsDecoder

mapDecoder : Decoder ReplaysResponseMap
mapDecoder = map ReplaysResponseMap (field "fileName" string)

allyTeamsDecoder : Decoder AllyTeams
allyTeamsDecoder = array
  ( map2 AllyTeam
    (field "winningTeam" bool)
    (field "Players" (array (map Player (field "name" string))))
  )

