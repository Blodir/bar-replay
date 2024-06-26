module Replays.View exposing (..)

import Replays.Model exposing (..)
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (..)
import Html.Styled.Events exposing (..)
import Array
import Styles exposing (neutral1)
import Css exposing (..)
import Styles exposing (btnDefault)
import Styles exposing (inputDefault)

view : Model -> Html Msg
view model = replaysView model

replaysView : Model -> Html Msg
replaysView model =
  div []
    [ h2 [] [text "Recent replays"]
    , div []
      [ label [ for "playerFilter" ] [ text "Player name:" ]
      , input [ list "playerFilterOptions", id "playerFilter", name "playerFilter", onInput PlayerFilterInput, css inputDefault ] []
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
          div []
          (
            ([button [ onClick RefreshReplays, css <| [margin2 (px 8) (px 0)] ++ btnDefault ] [ text "Refresh" ]])
            ++ (Array.toList (Array.map replayView replays.accumulated))
            ++ (if replays.loadingMore then [text "Loading more..."] else [])
            ++ [button [ onClick LoadMore, css btnDefault ] [text "Load more"]]
          )
    ]

replayView : ReplaysResponseReplay -> Html Msg
replayView replay =
  div [css [displayFlex, Css.property "gap" "12px", marginBottom <| px 16]]
    [ img [ src <| mapImgSrc replay, Html.Styled.Attributes.width 100, Html.Styled.Attributes.height 100 ] []
    , div []
      [ div [] [text "on ", span [css [color neutral1]] [text replay.map.fileName]]
      , div [css [margin2 (px 8) (px 0)]]
        <| List.map (\player -> div [] [text player.name])
        <| List.foldr (\a b -> a ++ b) []
        <| Array.toList
        <| Array.map (\team -> Array.toList team.players) replay.allyTeams
      , replayDownloadButton replay
      ]
    ]

mapImgSrc : ReplaysResponseReplay -> String
mapImgSrc replay = "https://api.bar-rts.com/maps/" ++ replay.map.fileName ++ "/texture-thumb.jpg"

replayDownloadButton : ReplaysResponseReplay -> Html Msg
replayDownloadButton replay =
  button [ onClick (DownloadReplay replay.id), css btnDefault ] [ text "Download" ]
