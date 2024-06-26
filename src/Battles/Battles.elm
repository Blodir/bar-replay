module Battles.Battles exposing (init, update)

import Html exposing (..)
import Battles.Decoders exposing (battlesDecoder)
import Http
import Html exposing (..)
import Battles.Decoders exposing (PlayerGameStatus(..))
import Battles.Model exposing (..)

init : () -> ( Model, Cmd Msg )
init _ =
  ( { battles = Loading }
  , getBattles
  )

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    GotBattle result ->
      case result of
        Ok battlesResponse ->
          ({ model | battles = Success battlesResponse }, Cmd.none)
        Err e ->
          ({ model | battles = Failure e }, Cmd.none)

getBattles : Cmd Msg
getBattles =
  Http.get
    { url = "https://api.bar-rts.com/battles"
    , expect = Http.expectJson GotBattle battlesDecoder
    }
