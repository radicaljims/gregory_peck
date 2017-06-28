module Views exposing (..)

import Array exposing (Array, toList, indexedMap)

import Html exposing (table, tr, td, h3, div, text, Html)
import Html.Attributes exposing (style)

import Geometry exposing (Position)
import Board

import Styling exposing (..)

whichColor : Int -> Int -> Styles
whichColor r c =
    let
      even i = i % 2 == 0
      colorize b = if b then tdBlack else tdStyle
    in
      if (even r) then (colorize (even c)) else (colorize (not (even c)))

renderCell : Int -> Int -> Position -> Html a
renderCell r c position =
      td [style (whichColor r c)] []

renderRow : Int -> Array Position -> Html a
renderRow idx cells =
    tr [style solidBorder] (List.indexedMap (renderCell idx) (toList cells))

renderBoard : Board.Model -> Html a
renderBoard board =
    let
      rows = List.indexedMap renderRow (toList board.cells)
    in
      Html.table [style solidBorder] rows

view : Board.Model -> Html a
view board =
    let
      header = h3 [] [text"Checkers!"]
      body = renderBoard board
    in
      div [] [header, body]
