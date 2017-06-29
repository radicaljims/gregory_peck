module BootstrapView exposing (..)

import Array exposing (Array, toList)
import List exposing (append, indexedMap)
import Dict exposing (Dict, get)

import Html exposing (table, tr, td, h1, div, text, Html, node)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.CDN as CDN

import Styling exposing (..)

import Board exposing (Pieces, Msg)
import Geometry exposing (..)
import Piece exposing (..)

whichColor : Pieces -> Int -> Int -> Styles
whichColor pieces r c =
    let
      colorize b = if b then tdBlack else tdWhite
      havePiece = Dict.get (c, r) pieces
    in
      case havePiece of
          Nothing ->
            if (even r) then (colorize (even c)) else (colorize (odd c))

          Just piece ->
              if piece.color == Piece.Blue then tdBlue else tdYellow

renderCell : Pieces -> Int -> Int -> Position -> Grid.Column Msg
renderCell pieces r c position =
    let
        css = [style (whichColor pieces r c), onClick (Board.Clicked (c, r))]
        node = div css [text "."]
    in
        Grid.col [Col.sm2] [node]

renderRow : Pieces -> Int -> Array Position -> Html Msg
renderRow pieces idx cells =
    Grid.row [] (indexedMap (renderCell pieces idx) (toList cells))

renderBoard : Board.Model -> List (Html Msg)
renderBoard board =
      indexedMap (renderRow board.pieces) (toList board.cells)


view : Board.Model -> Html Msg
view board =
    let
        header =
          [ CDN.stylesheet -- creates an inline style node with the Bootstrap CSS
          , h1 [] [text "Checkers!"]
          ]
        grid = renderBoard board

    in
        Grid.container [] (append header grid)
