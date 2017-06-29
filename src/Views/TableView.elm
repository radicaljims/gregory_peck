module TableView exposing (..)

import Array exposing (Array, toList, indexedMap)
import Dict exposing (Dict)

import Html exposing (table, tr, td, h3, div, text, Html)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Geometry exposing (Position, even, odd)
import Board exposing (Pieces, Msg)
import Piece

import Styling exposing (..)

whichColor : Pieces -> Int -> Int -> Styles
whichColor pieces r c =
    let
      colorize b = if b then tdBlack else tdStyle
      havePiece = Dict.get (c, r) pieces
    in
      case havePiece of
          Nothing ->
            if (even r) then (colorize (even c)) else (colorize (odd c))

          Just piece ->
              if piece.color == Piece.Blue then tdBlue else tdYellow

renderCell : Pieces -> Int -> Int -> Position -> Html Msg
renderCell pieces r c position =
    td [style (whichColor pieces r c), onClick (Board.Clicked (c, r))] []

renderRow : Pieces -> Int -> Array Position -> Html Msg
renderRow pieces idx cells =
    tr [style solidBorder] (List.indexedMap (renderCell pieces idx) (toList cells))

renderBoard : Board.Model -> Html Msg
renderBoard board =
    let
      rows = List.indexedMap (renderRow board.pieces) (toList board.cells)
    in
      Html.table [style solidBorder] rows
