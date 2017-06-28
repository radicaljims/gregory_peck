module Board exposing (defaultBoard, update, Model, Msg)

import Array exposing (..)
import Dict exposing (Dict)

import Geometry exposing (..)
import Piece exposing (..)

type alias Dimensions = { x : Int, y : Int }

type alias Cells = Array (Array Position)

type alias Pieces = Dict Position Piece

type alias Model = { dimensions : Dimensions, cells : Cells, pieces : Pieces }

defaultDimensions : Dimensions
defaultDimensions = { x = 6, y = 6 }

createCells : Dimensions -> Cells
createCells {x, y} =
    let
      init row col = (col, row)
    in
      initialize y (\r -> initialize x (init r))

piecesForRow : Int -> Color -> Dimensions -> List (Position, Piece)
piecesForRow r color {x, y} =
    let
        seed = List.range 0 (x - 1)
        converter i = ((i, r), createPiece color (i, r))
    in
        List.map converter seed

createPieces : Dimensions -> Dict Position Piece
createPieces dimensions =
    let
        {x, y} = dimensions
        blues = List.append (piecesForRow 0 Blue dimensions) (piecesForRow 1 Blue dimensions)
        yellows = List.append (piecesForRow (y - 1) Yellow dimensions) (piecesForRow (y - 2) Yellow dimensions)

    in
        Dict.fromList (List.append blues yellows)

defaultBoard : Model
defaultBoard =
    let
      cells = createCells defaultDimensions
      pieces = createPieces defaultDimensions
    in
      { dimensions = defaultDimensions, cells = cells, pieces = pieces }

type Msg = NoOp

update : Msg -> Model -> Model
update msg model =
    case msg of

        NoOp -> model
