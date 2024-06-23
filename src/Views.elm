module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array
import Model exposing (..)

view : Model -> Html Msg
view model =
  case model of
    Failure ->
      text "Failure"
    Loading ->
      text "Loading..."
    Success replays ->
      Html.div []
      (
        ([button [ onClick RefreshReplays ] [ text "Refresh" ]])
        ++ (Array.toList (Array.map replayView replays.data))
      )

replayView : Replay -> Html Msg
replayView replay =
  Html.div []
    [ Html.img [ Html.Attributes.src ("https://api.bar-rts.com/maps/" ++ replay.map.fileName ++ "/texture-thumb.jpg") ] []
    , text replay.map.fileName
    , Html.ul [] (Array.toList (Array.map (\team ->
        Html.li []
          [ text (if team.winningTeam then "winners" else "losers")
          , Html.ul [] (Array.toList (Array.map (\player -> Html.li [] [text player.name]) team.players))
          ]
      ) replay.allyTeams))
    ]

