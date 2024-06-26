module Battles.Battles exposing (Model, Msg, init, view, update)

import Html exposing (..)
import Battles.Decoders exposing (BattlesResponse, battlesDecoder)
import Http
import Html exposing (..)
import Html.Attributes
import Array
import Battles.Decoders exposing (Battle)
import Battles.Decoders exposing (GameType(..))
import Utils exposing (errorToString)
import Battles.Decoders exposing (Player)
import Battles.Decoders exposing (PlayerGameStatus(..))

type alias Model =
  { battles: BattlesResponseWrapper
  }

type BattlesResponseWrapper = Failure Http.Error | Loading | Success BattlesResponse

type Msg =
  GotBattle (Result Http.Error BattlesResponse)

init : () -> ( Model, Cmd Msg )
init _ =
  ( { battles = Loading }
  , getBattles
  )

view : Model -> Html Msg
view model =
  case model.battles of
    Failure e -> text ("Failure: " ++ (errorToString e))
    Loading -> text "Loading..."
    Success battles ->
      div [] <|
      [ h2 [] [text "Live battles"]
      , p [] [text <| "Displaying duels with at least one player with >= " ++ (String.fromFloat minOs) ++ " OS"]
      ] ++ (
        Array.toList (
          Array.map battleView <| Array.filter battlesFilter battles
        )
      )

minOs : Float
minOs = 30

battlesFilter : Battle -> Bool
battlesFilter battle =
  ( case battle.gameType of
    Just gt -> gt == Duel 
    Nothing -> False
  ) && not
    ( Array.isEmpty
      <| Array.filter
        (\player -> case player.skill of
          Just os -> os >= minOs
          Nothing -> False
        )
        battle.players
    )

battleView : Battle -> Html Msg
battleView battle =
  div []
    ( (case battle.mapFileName of
        Just fileName ->
          [Html.img [ Html.Attributes.src ("https://api.bar-rts.com/maps/" ++ fileName ++ "/texture-thumb.jpg") ] []]
        Nothing ->
          []     
      ) ++
      [ text battle.title ] ++ (Array.toList (Array.map playerView battle.players))
    )

playerView : Player -> Html Msg
playerView player =
  div []
  [ text player.username
  , text
    ( case player.skill of
      Just skill -> String.fromFloat skill
      Nothing -> "No data"
    )
  , text
    ( case player.gameStatus of
      Playing -> "Playing"
      Spectating -> "Spectating"
      Waiting -> "Waiting"
    )
  ]

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
