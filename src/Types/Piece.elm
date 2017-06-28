module Piece exposing (..)

type Color = Blue | White

type alias Position = { x : Int, y : Int }
type alias Piece = { position : Position, color : Color, crowned : Bool, alive : Bool }

createPiece : Color -> Position -> Piece
createPiece color position = { position = position, color = color, crowned = False, alive = True }
