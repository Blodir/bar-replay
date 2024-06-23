module Main exposing (..)

import Browser
import Array exposing (Array, get)
import Http
import Decoders exposing (..)
import Model exposing (..)
import Views exposing (..)

main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

init : () -> (Model, Cmd Msg)
init _ = (Loading, getReplays)

getReplays : Cmd Msg
getReplays =
  Http.get
    { url = "https://api.bar-rts.com/replays?page=1&limit=5&preset=duel&hasBots=false&endedNormally=true"
    , expect = Http.expectJson GotReplays replaysDecoder
    }

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotReplays result ->
      case result of
        Ok replays ->
          (Success replays, Cmd.none)
        Err _ ->
          (Failure, Cmd.none)
    RefreshReplays ->
      ( Loading
      , getReplays
      )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

