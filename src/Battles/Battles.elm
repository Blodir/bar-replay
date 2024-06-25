module Battles.Battles exposing (Model, Msg, init, view, update)

import Html exposing (..)

type alias Model = {}

init : () -> ( Model, Cmd msg )
init _ =
  ( {}
  , Cmd.none
  )

view : Model -> Html Msg
view model = text "Hello battles"

type Msg = NoOp

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  (model, Cmd.none)
