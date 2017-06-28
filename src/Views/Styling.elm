module Styling exposing (..)

import Color exposing (..)
import Style exposing (..)

type alias Styles = List (String, String)

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
