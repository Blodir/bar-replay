module Main exposing (..)

import Browser
import Http
import ReplaysDecoder exposing (replaysDecoder)
import ReplayDecoder exposing (replayDecoder)
import Model exposing (..)
import Views exposing (..)
import String exposing (isEmpty)
import Debounce
import Task
import File exposing (File)
import File.Download
import Array

main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

init : () -> (Model, Cmd Msg)
init _ =
  ( { replays = Loading,
      playerFilter = Nothing,
      debounce = Debounce.init
    }
  , getReplays Nothing 1
  )

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

getReplay : String -> Cmd Msg
getReplay id =
  Http.get
    { url = "https://api.bar-rts.com/replays/" ++ id
    , expect = Http.expectJson GotReplay replayDecoder
    }

debounceConfig : Debounce.Config Msg
debounceConfig =
  { strategy = Debounce.later 300
  , transform = DebounceMsg
  }

replayDownloadUrl : ReplayResponse -> String
replayDownloadUrl replay =
  "https://storage.uk.cloud.ovh.net/v1/AUTH_10286efc0d334efd917d476d7183232e/BAR/demos/" ++ replay.fileName

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
              ({ model | replays = Success ({ lastResponse = replaysResponse, loadingMore = False, accumulated = Array.append oldReplays.accumulated replaysResponse.data })}, Cmd.none)
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
        filterStr = if isEmpty str then Nothing else Just str
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

debouncerSave : String -> Cmd Msg
debouncerSave s = Task.perform PlayerFilterChange (Task.succeed s)

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

