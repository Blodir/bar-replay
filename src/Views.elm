module Views exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Array
import Model exposing (..)
import File.Download

view : Model -> Html Msg
view model =
  Html.div []
  [ Html.div []
    [ label [ for "playerFilter" ] [ text "Player name:" ]
    , input [ list "playerFilterOptions", id "playerFilter", name "playerFilter", onInput PlayerFilterInput ] []
    -- TODO autocomplete (there's no api for this smh)
{-     , datalist [ id "playerFilterOptions" ]
      [ option [ value "Blodir" ] []
      , option [ value "Chisato" ] []
      ] -}
    ]
  , case model.replays of
      Failure ->
        text "Failure"
      Loading ->
        text "Loading..."
      Success replays ->
        Html.div []
        (
          ([button [ onClick RefreshReplays ] [ text "Refresh" ]])
          ++ (Array.toList (Array.map replayView replays.accumulated))
          ++ (if replays.loadingMore then [text "Loading more..."] else [])
          ++ [button [ onClick LoadMore ] [text "Load more"]]
        )
  ]

replayView : ReplaysResponseReplay -> Html Msg
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
    , replayDownloadButton replay
    ]

replayDownloadButton : ReplaysResponseReplay -> Html Msg
replayDownloadButton replay =
  button [ onClick (DownloadReplay replay.id) ] [ text "Download" ]
