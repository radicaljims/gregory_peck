module Main exposing (main)

import Html exposing (beginnerProgram)

import Board
import Views

main : Program Never Board.Model Board.Msg
main = beginnerProgram { model = Board.defaultBoard, view = Views.view, update = Board.update}
