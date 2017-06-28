module Board exposing (defaultBoard, update, Model, Msg)

import Array exposing (..)
import Piece exposing (..)

type alias Dimensions = { x : Int, y : Int }

-- TODO: I think we'll want to revisit side and represent it as a map of position -> Piece
type alias Side = Array (Array Piece)
type alias Model = { dimensions : Dimensions, blues : Side, whites : Side }

defaultDimensions : Dimensions
defaultDimensions = { x = 5, y = 5}

createSide : Color -> Dimensions -> Side
createSide color {x, y} =
    let
        init color row col =
            createPiece color {x = col, y = row}
    in
      -- Initialize 2 rows of X pieces, which I think is the default configuration
      initialize 2 (\r -> initialize x (init color r))

defaultBoard : Model
defaultBoard =
    let
        blues = createSide Blue defaultDimensions
        whites = createSide White defaultDimensions

    in
        { dimensions = defaultDimensions, blues = blues, whites = whites }

type Msg = NoOp

update : Msg -> Model -> Model
update msg model =
    case msg of

        NoOp -> model
