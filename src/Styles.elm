module Styles exposing (..)

import Css exposing (..)
import Html.Styled.Attributes exposing (..)

primary1 = hsl 252 0.9 0.95
primary2 = hsl 252 0.9 0.88
primary3 = hsl 252 0.9 0.8
primary4 = hsl 252 0.9 0.7
primary5 = hsl 252 0.6 0.5
primary6 = hsl 252 0.6 0.3
primary7 = hsl 252 0.6 0.2
primary8 = hsl 252 0.6 0.15
primary9 = hsl 252 0.6 0.1

neutral1 = hsl 252 0.04 0.98
neutral2 = hsl 252 0.04 0.88
neutral3 = hsl 252 0.04 0.8
neutral4 = hsl 252 0.04 0.7
neutral5 = hsl 252 0.04 0.32
neutral6 = hsl 252 0.04 0.24
neutral7 = hsl 252 0.04 0.18
neutral8 = hsl 252 0.04 0.14
neutral9 = hsl 252 0.04 0.1

btnDefault : List Style
btnDefault =
  [ padding2 (px 6) (px 12)
  , backgroundColor neutral6
  , color primary1
  , border <| px 0
  , borderRadius <| px 4
  , boxShadow4 (px 1) (px 2) (px 4) <| hsla 252 0.8 0.2 0.4
  , fontSize <| px 14
  , hover
    [ backgroundColor neutral5
    , cursor pointer
    ]
  ]

inputDefault : List Style
inputDefault =
  [ padding2 (px 2) (px 4)
  , backgroundColor neutral2
  , borderRadius <| px 2
  , boxShadow5 inset (px 1) (px 2) (px 4) <| hsla 252 0.8 0.2 0.4
  , border <| px 0
  , fontSize <| px 16
  ]
