module Piece exposing (..)

import Geometry exposing (Position)

type Color = Blue | Yellow

type alias Piece = { position : Position, color : Color, crowned : Bool, alive : Bool }

createPiece : Color -> Position -> Piece
createPiece color position = { position = position, color = color, crowned = False, alive = True }
