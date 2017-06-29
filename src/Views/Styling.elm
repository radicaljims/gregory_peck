module Styling exposing (..)

import Color exposing (..)
import Style exposing (..)

type alias Styles = List (String, String)

centeredLayout : List Style
centeredLayout =
  [ display flex_
  , justifyContent center
  , alignItems center
  ]

centeredLayout2 : List Style
centeredLayout2 =
  [ justifyContent center
  , alignItems center
  ]

columnLayout : Styles
columnLayout =
  [ justifyContent center
  , alignItems center
  , width (px 550)
  , height (px 550)
  ]

solidBorder : Styles
solidBorder = [border (px 10), border solid]

tdStyle : Styles
tdStyle = List.append solidBorder [width (px 50), height (px 50)]

tdBlue : Styles
tdBlue = List.append tdStyle [backgroundColor (color_ blue)]

tdYellow : Styles
tdYellow = List.append tdStyle [backgroundColor (color_ yellow)]

tdBlack : Styles
tdBlack = List.append tdStyle [backgroundColor (color_ black)]

tdWhite : Styles
tdWhite = List.append tdStyle [backgroundColor (color_ white)]

blueText : Styles
blueText = [textAlign center, color (color_ blue)]

yellowText : Styles
yellowText = [textAlign center, color (color_ yellow)]

whiteText : Styles
whiteText = [textAlign center, color (color_ white)]

centerText : Styles
centerText = [textAlign center]
