module Views exposing (..)

import Color exposing (..)

import Html exposing (table, tr, td, h3, div, text, Html)
import Html.Attributes exposing (style)

import Style exposing (..)

import Board

type alias Styles = List (String, String)

solidBorder : Styles
solidBorder = [border (px 10), border solid]

tdStyle : Styles
tdStyle = List.append solidBorder [width (px 50), height (px 50)]

tdBlue : Styles
tdBlue = List.append tdStyle [backgroundColor (color_ blue)]

tdBlack : Styles
tdBlack = List.append tdStyle [backgroundColor (color_ black)]

newCol : Html a
newCol = td [style tdStyle] []

newBlackCol : Html a
newBlackCol = td [style tdBlack] []

colorize : Int -> Html a -> Html a
colorize idx elem =
    if idx % 2 == 0 then newBlackCol else elem

colorizeOdd : Int -> Html a -> Html a
colorizeOdd idx elem =
    if (idx % 2) /= 0 then newBlackCol else elem

colorizeSquares : Int -> List (Html a) -> List (Html a)
colorizeSquares r tds =
    if (r % 2 == 0) then (List.indexedMap colorize tds) else (List.indexedMap colorizeOdd tds)

renderRow : Int -> Board.Model -> Html a
renderRow r board =
    let
      tds = List.repeat board.dimensions.x newCol
      colorized = colorizeSquares r tds
    in
        tr [style solidBorder] colorized

renderRows : Int -> Board.Model -> List (Html a)
renderRows r board =
    if r > 0 then (renderRow r board) :: (renderRows (r - 1) board) else []

renderBoard : Board.Model -> Html a
renderBoard board =
    let
        rows = renderRows board.dimensions.y board
    in
      Html.table [style solidBorder] rows

view : Board.Model -> Html a
view board =
    let
      header = h3 [] [text"Checkers!"]
      body = renderBoard board
    in
      div [] [header, body]
