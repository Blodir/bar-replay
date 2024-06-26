module Main exposing (..)

import Browser
import Replays.Replays
import Replays.Model
import Replays.View
import Battles.Battles
import Battles.View
import Battles.Model
import Html.Styled exposing (..)
import Html.Styled.Attributes

import Css
import Css.Global
import Html.Styled exposing (Html)
import Html.Styled.Attributes exposing (href)
import Styles exposing (..)
import Html.Styled.Attributes exposing (target)

type alias Model =
  { replays: Replays.Model.Model,
    battles: Battles.Model.Model
  }

type Msg =
    Replays Replays.Model.Msg
  | Battles Battles.Model.Msg

main = Browser.element
  { init = init
  , update = update
  , subscriptions = subscriptions
  , view = view
  }

init : () -> (Model, Cmd Msg)
init _ =
  let
    (replaysModel, replaysCmd) = Replays.Replays.init ()
    (battlesModel, battlesCmd) = Battles.Battles.init ()
  in
    ({ replays = replaysModel, battles = battlesModel }
    , Cmd.batch([Cmd.map (\msg -> Replays msg) replaysCmd, Cmd.map (\msg -> Battles msg) battlesCmd])
    )

globals : Html Msg
globals = Css.Global.global
  [ Css.Global.selector "*, *:before, *:after" [ Css.boxSizing Css.borderBox ]
  , Css.Global.body
    [ Css.color neutral2
    , Css.backgroundColor neutral9
    , Css.fontFamily Css.sansSerif
    , Css.fontSize (Css.px 16)
    ]
  , Css.Global.a [Css.color primary3]
  ]

view model =
  div [ Html.Styled.Attributes.css [  ] ]
  [ globals
  , topbarView
  , div [ Html.Styled.Attributes.css [Css.displayFlex, Css.justifyContent Css.center]]
    [ div
      [ Html.Styled.Attributes.css
        [ Css.displayFlex
        , Css.property "gap" "24px"
        , Css.padding2 (Css.px 8) (Css.px 16)
        , Css.backgroundColor neutral8
        , Css.maxWidth (Css.px 1000)
        , Css.width (Css.pct 100)
        , Css.justifyContent Css.spaceAround
        , Css.boxShadow4 (Css.px 0) (Css.px 0) (Css.px 32) (Css.hsla 252 0.9 0.1 0.3)
        ]
      ]
      [ map (\msg -> Battles msg) (Battles.View.view model.battles)
      , map (\msg -> Replays msg) (Replays.View.view model.replays) ]
    ]
  ] |> Html.Styled.toUnstyled

topbarView =
  div 
    [ Html.Styled.Attributes.css
      [ Css.justifyContent Css.center
      , Css.alignItems Css.center
      , Css.backgroundColor neutral7
      , Css.color neutral2
      , Css.displayFlex
      , Css.padding2 (Css.px 8) (Css.px 16)
      , Css.boxShadow5 (Css.px 0) (Css.px -16) (Css.px 16) (Css.px 16) (Css.hsla 252 0.8 0.3 0.2)
      , Css.position Css.relative
      ]
    ]
    [ div [ Html.Styled.Attributes.css [Css.width <| Css.pct 100, Css.maxWidth <| Css.px 1200 ] ]
      
      [ h1
        [ Html.Styled.Attributes.css
          [ Css.margin (Css.px 0)
          , Css.color primary4
          , Css.fontWeight (Css.int 400)
          , Css.fontSize (Css.px 16)
          ]
        ]
        [text "BAR Duel Dashboard"]
  {-     , a
        [ href "https://github.com/Blodir/bar-replay"
        , target "_blank"
        , Html.Styled.Attributes.css
          [ Css.display Css.inlineBlock
          ]
        ]
        [text "Github"] -}
      ]
    ]

update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
  case msg of
    Replays msg2 ->
      let
        (newModel, newCmd) = Replays.Replays.update msg2 model.replays
      in
        ( { model | replays = newModel }, Cmd.map (\msg3 -> Replays msg3) newCmd )
    Battles msg2 ->
      let
        (newModel, newCmd) = Battles.Battles.update msg2 model.battles
      in
        ( { model | battles = newModel }, Cmd.map (\msg3 -> Battles msg3) newCmd )

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

