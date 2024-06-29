module Replays.Replays exposing (init, update, subscriptions)

import Html
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Debounce
import Debounce exposing (Debounce)
import Array exposing (Array)
import Task
import File.Download
import Json.Decode
import Json.Decode.Pipeline exposing (required)
import Replays.Model exposing (..)

replaysDecoder : Json.Decode.Decoder ReplaysResponse
replaysDecoder = Json.Decode.map2 ReplaysResponse (Json.Decode.field "data" replaysDataDecoder) (Json.Decode.field "page" Json.Decode.int)

replaysDataDecoder : Json.Decode.Decoder (Array ReplaysResponseReplay)
replaysDataDecoder = Json.Decode.array replaysReplayDecoder

replaysReplayDecoder : Json.Decode.Decoder ReplaysResponseReplay
replaysReplayDecoder =
  Json.Decode.succeed ReplaysResponseReplay
    |> Json.Decode.Pipeline.required "id" Json.Decode.string
    |> Json.Decode.Pipeline.required "startTime" Json.Decode.string
    |> Json.Decode.Pipeline.required "durationMs" Json.Decode.int
    |> Json.Decode.Pipeline.required "Map" mapDecoder
    |> Json.Decode.Pipeline.required "AllyTeams" allyTeamsDecoder

mapDecoder : Json.Decode.Decoder ReplaysResponseMap
mapDecoder = Json.Decode.map ReplaysResponseMap (Json.Decode.field "fileName" Json.Decode.string)

allyTeamsDecoder : Json.Decode.Decoder AllyTeams
allyTeamsDecoder = Json.Decode.array
  ( Json.Decode.map2 AllyTeam
    (Json.Decode.field "winningTeam" Json.Decode.bool)
    (Json.Decode.field "Players" (Json.Decode.array (Json.Decode.map Player (Json.Decode.field "name" Json.Decode.string))))
  )

replayDecoder : Json.Decode.Decoder ReplayResponse
replayDecoder = Json.Decode.map ReplayResponse (Json.Decode.field "fileName" Json.Decode.string)

init : () -> (Model, Cmd Msg)
init _ =
  ( { replays = Loading,
      playerFilter = Nothing,
      debounce = Debounce.init
    }
  , getReplays Nothing 1
  )

getReplay : String -> Cmd Msg
getReplay id =
  Http.get
    { url = "https://api.bar-rts.com/replays/" ++ id
    , expect = Http.expectJson GotReplay replayDecoder
    }

getReplays : Maybe String -> Int -> Cmd Msg
getReplays playerName page =
  let
    baseUrl = "https://api.bar-rts.com/replays?limit=5&preset=duel&hasBots=false&endedNormally=true"
  in
    Http.get
      { url = baseUrl ++ "&page=" ++ (String.fromInt page) ++ case playerName of
          Just p -> if String.isEmpty p then "" else "&players=" ++ p
          Nothing -> ""
      , expect = Http.expectJson GotReplays replaysDecoder
      }

debounceConfig : Debounce.Config Msg
debounceConfig =
  { strategy = Debounce.later 300
  , transform = DebounceMsg
  }

replayDownloadUrl : ReplayResponse -> String
replayDownloadUrl replay =
  "https://storage.uk.cloud.ovh.net/v1/AUTH_10286efc0d334efd917d476d7183232e/BAR/demos/" ++ replay.fileName

debouncerSave : String -> Cmd Msg
debouncerSave s = Task.perform PlayerFilterChange (Task.succeed s)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    NoOp ->
      ( model, Cmd.none )
    DebounceMsg str ->
      let
        ( debounce, cmd ) =
          Debounce.update
            debounceConfig
            (Debounce.takeLast debouncerSave)
            str
            model.debounce
      in
        ( { model | debounce = debounce }
        , cmd
        ) 
    PlayerFilterInput s ->
      let
        (debounce, cmd) =
          Debounce.push debounceConfig s model.debounce
      in
        ({ model | debounce = debounce }, cmd)
    GotReplays result ->
      case result of
        Ok replaysResponse ->
          case model.replays of
            Success oldReplays ->
              if oldReplays.loadingMore then
                ({ model | replays = Success ({ lastResponse = replaysResponse, loadingMore = False, accumulated = Array.append oldReplays.accumulated replaysResponse.data })}, Cmd.none)
              else
                ({ model | replays = Success ({ lastResponse = replaysResponse, loadingMore = False, accumulated = replaysResponse.data })}, Cmd.none)
            _ -> 
              ({ model | replays = Success ({ lastResponse = replaysResponse, loadingMore = False, accumulated = replaysResponse.data })}, Cmd.none)
        Err _ ->
          ({ model | replays = Failure }, Cmd.none)
    GotReplay result ->
      case result of
        Ok replay ->
          (model, File.Download.url (replayDownloadUrl replay))
        Err _ ->
          ({ model | replays = Failure }, Cmd.none)
    RefreshReplays ->
      ( { model | replays = Loading }
      , getReplays (model.playerFilter) 1
      )
    PlayerFilterChange str ->
      let
        filterStr : Maybe String
        filterStr = if String.isEmpty str then Nothing else Just str
      in
        ( { model | replays = Loading, playerFilter = filterStr }
        , getReplays (filterStr) 1
        )
    DownloadReplay id ->
      ( model, getReplay id)
    LoadMore ->
      case model.replays of
        Success replays ->
          ( { model | replays = Success { replays | loadingMore = True } }
          , getReplays model.playerFilter (replays.lastResponse.page + 1)
          )
        _ -> 
          (model, Cmd.none)
    Tick ->
      ( { model | replays = case model.replays of
          Success r -> Success { r | loadingMore = False }
          _ -> model.replays
        }
      , getReplays (model.playerFilter) 1
      )
