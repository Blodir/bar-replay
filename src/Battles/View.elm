module Battles.View exposing (view)

import Battles.Model exposing (..)
import Battles.Decoders exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Array
import Utils exposing (errorToString)
import Css exposing (..)
import Styles exposing (..)

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
        List.map battleView  <| battlesSort <| Array.toList <| Array.filter battlesFilter battles
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

battlesSort : List Battle -> List Battle
battlesSort battles =
  let
    highestRatingIn : Battle -> Float
    highestRatingIn battle =
      let
        h =
          List.head <| List.sortBy
            (\player -> case player.skill of
              Just s -> -s
              Nothing -> 1000
            )
            (Array.toList battle.players)
      in
        case h of
          Just h2 ->
            case h2.skill of
              Just s -> s
              Nothing -> -1000
          Nothing -> -1000
  in
    List.sortWith
      (\battle1 battle2
        -> if highestRatingIn battle1 > highestRatingIn battle2 then LT else GT
      )
      battles

battleView : Battle -> Html Msg
battleView battle =
  div [ css [ displayFlex, marginBottom (px 16), Css.property "gap" "12px" ]]
    [ div []
      ( case battle.mapFileName of
        Just fileName ->
          [ img
            [ src ("https://api.bar-rts.com/maps/" ++ fileName ++ "/texture-thumb.jpg")
            , Html.Styled.Attributes.width 100
            , Html.Styled.Attributes.height 100
            ] []]
        Nothing ->
          []
      )
    , div []
      [ div
        [ css [{- fontSize <| px 18, color neutral1,  -}marginBottom <| px 2] ]
        [text battle.title]
      , div [] [text "on ", span [css [color neutral1]] [text battle.map]]
      , div
        [ css
          [ marginTop <| px 8
          , Css.property "display" "grid"
          , Css.property "grid-template-columns" "min-content 1fr 1fr"
          , Css.property "grid-column-gap" "10px"
          , Css.property "grid-row-gap" "4px"
          ]
        ]
        (List.foldr (\a b -> List.append a b) [] (List.map playerView (sortPlayers <| Array.toList battle.players)))
      ]
    ]

sortPlayers : List Player -> List Player
sortPlayers players =
  let
    unwrap skill = case skill of
      Just s -> s
      Nothing -> -1000
  in
    List.sortWith
      (\p1 p2 -> case p1.gameStatus of
        Playing -> case p2.gameStatus of
          Playing -> compare (-1 * unwrap p1.skill) (-1 * unwrap p2.skill)
          Waiting -> LT
          Spectating -> LT
        Waiting -> case p2.gameStatus of
          Playing -> GT
          Waiting -> compare (-1 * unwrap p1.skill) (-1 * unwrap p2.skill)
          Spectating -> LT
        Spectating -> case p2.gameStatus of
          Playing -> GT
          Waiting -> GT
          Spectating -> compare (-1 * unwrap p1.skill) (-1 * unwrap p2.skill)
      )
      players

playerView : Player -> List (Html Msg)
playerView player =
  let
    c = playerStatusColor player
  in
    [ div [css [c]]
      [ text
        ( case player.skill of
          Just skill -> String.fromFloat skill
          Nothing -> "-"
        )
      ]
    , div [css [c]] [text player.username]
    , div [css [c]]
      [ text
        ( case player.gameStatus of
          Playing -> "Playing"
          Spectating -> "Spectating"
          Waiting -> "Waiting"
        )
      ]
    ]

playerStatusColor : Player -> Style
playerStatusColor player =
  color (case player.gameStatus of
    Playing -> neutral1
    Waiting -> neutral4
    Spectating -> neutral5
  )

