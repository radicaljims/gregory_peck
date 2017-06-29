module Views exposing (..)

import Html exposing (table, tr, td, h3, div, text, Html)

import Board exposing (Pieces, Msg)
import TableView exposing (renderBoard)

view : Board.Model -> Html Msg
view board =
    let
      header = h3 [] [text "Checkers!"]
      body = renderBoard board
    in
      div [] [header, body]
