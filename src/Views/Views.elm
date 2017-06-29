module Views exposing (..)

import Html exposing (table, tr, td, h1, div, text, Html, node)
import Html.Attributes exposing (style, rel, href)

import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Bootstrap.CDN as CDN

import Styling exposing (..)

import Board exposing (Pieces, Msg)

import TableView exposing (renderBoard, renderScore)

view : Board.Model -> Html Msg
view board =
    let
      -- css = node "link" [ rel "stylesheet", href "https://maxcdn.bootstrapcdn.com/bootstrap/4.0.0-alpha.6/css/bootstrap.min.css"] []
      header = h1 [style centeredLayout] [text "Checkers!"]
      score = renderScore board
      body = renderBoard board
    in
        Grid.containerFluid [] <|
            [ CDN.stylesheet
            , Grid.row [Row.middleSm, Row.centerSm] [Grid.col [Col.sm12] [header]]
            , Grid.row [] [Grid.col [Col.sm12] [score]]
            , Grid.row [Row.middleSm, Row.centerSm] [Grid.col [Col.sm5, Col.middleSm] [body]]
            ]
