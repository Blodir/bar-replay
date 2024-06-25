module Main exposing (..)

import Browser
import String exposing (isEmpty)
import File exposing (File)
import Battles.Battles as Battles
import Replays.Replays as Replays
import Html exposing (Html)
import Array exposing (Array)
import Html.Attributes

type alias Model =
  { replays: Replays.Model,
    battles: Battles.Model
  }

type Msg =
    Replays Replays.Msg
  | Battles Battles.Msg

main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    (replaysModel, replaysCmd) = Replays.init ()
    (battlesModel, battlesCmd) = Battles.init ()
  in
    ({ replays = replaysModel, battles = battlesModel }
    , Cmd.batch([Cmd.map (\msg -> Replays msg) replaysCmd, Cmd.map (\msg -> Battles msg) battlesCmd])
    )

view : Model -> Html Msg
view model =
  Html.div [ Html.Attributes.style "display" "flex" ]
  [ Html.map (\msg -> Replays msg) (Replays.view model.replays)
  , Html.map (\msg -> Battles msg) (Battles.view model.battles) ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Replays msg2 ->
      let
        (newModel, newCmd) = Replays.update msg2 model.replays
      in
        ( { model | replays = newModel }, Cmd.map (\msg3 -> Replays msg3) newCmd )
    Battles msg2 ->
      let
        (newModel, newCmd) = Battles.update msg2 model.battles
      in
        ( { model | battles = newModel }, Cmd.map (\msg3 -> Battles msg3) newCmd )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

