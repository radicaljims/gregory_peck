module TableView exposing (..)

import Array exposing (Array, toList, indexedMap)
import Dict exposing (Dict)

import Html exposing (table, tr, td, h3, div, text, Html, span)
import Html.Attributes exposing (style, width)
import Html.Events exposing (onClick)

import Bootstrap.Table as Table

import Geometry exposing (Position, even, odd)
import Board exposing (Pieces, Msg)
import Piece

import Styling exposing (..)

whichColor : Pieces -> Int -> Int -> (Styles, String)
whichColor pieces r c =
    let
      colorize b = if b then tdBlack else tdStyle
      havePiece = Dict.get (c, r) pieces
    in
      case havePiece of
          Nothing ->
            if (even r) then (colorize (even c), "") else (colorize (odd c), "")

          Just piece ->
              let
                  crownedText = if piece.crowned then  "C" else ""
              in
                  if piece.color == Piece.Blue then (tdBlue, crownedText) else (tdYellow, crownedText)

renderCell : Pieces -> Int -> Int -> Position -> Html Msg
renderCell pieces r c position =
    let
        (s, t) = whichColor pieces r c
    in
        td [style s, style centerText, onClick (Board.Clicked (c, r))] [span [style whiteText] [text t]]

renderRow : Pieces -> Int -> Array Position -> Html Msg
renderRow pieces idx cells =
    tr [style solidBorder] (List.indexedMap (renderCell pieces idx) (toList cells))

renderBoard : Board.Model -> Html Msg
renderBoard board =
    let
      rows = List.indexedMap (renderRow board.pieces) (toList board.cells)
    in
      Html.table [] rows

renderScore : Board.Model -> Html Msg
renderScore board =
    let
        blueScore = toString board.score.blue
        yellowScore = toString board.score.yellow
    in
        Table.simpleTable
            ( Table.simpleThead [ Table.th [Table.cellAttr (style blueText)] [text "Blue"]
                                , Table.th [Table.cellAttr (style yellowText)] [text "Yellow"]]
            , Table.tbody []
                [ Table.tr []
                      [ Table.td [Table.cellAttr (style centerText)] [text blueScore]
                      , Table.td [Table.cellAttr (style centerText)] [text yellowScore]]
                ]
            )
