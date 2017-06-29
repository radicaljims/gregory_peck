module Board exposing (..)

import Debug

import Array exposing (..)
import Dict exposing (Dict)
import Maybe exposing (Maybe, andThen)

import Geometry exposing (..)
import Piece exposing (..)

type alias Dimensions = { x : Int, y : Int }

type alias Cells = Array (Array Position)
type alias Pieces = Dict Position Piece

type alias MoveState = { position : Position }

type alias Model = { dimensions : Dimensions, cells : Cells, pieces : Pieces, movement : Maybe MoveState }

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
        offset r = if (even r) then even else odd
        converter f i =
            case (f i) of
                False -> Nothing

                True -> Just ((i, r), createPiece color (i, r))
    in
        List.filterMap (converter (offset r)) seed

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
      { dimensions = defaultDimensions, cells = cells, pieces = pieces, movement = Nothing }

type Msg = NoOp | Clicked Position


-- type MoveType = Illegal | Move | Attack

-- TODO: after each move we will have to test if another attack is possible, and not auto switch sides
-- since I think I read a player HAS to take all jumps
legal : Piece -> Position -> Position -> Pieces -> (Bool, Pieces)
legal piece (old_x, old_y) (new_x, new_y) pieces =

    -- We can't jump on people!
    if (Dict.member (new_x, new_y) pieces) then (False, pieces)
        else

          case piece.color of

              -- TODO: bounds checking!
              -- TODO: will eventually need to consider crowned pieces
              -- TODO: need to handle jump moves, which entails modifying pieces so we'd
              -- have to return that out of here so model could be updated
              -- Or just return a move type out of here: Illegal, Legal, Attack?
              Blue ->

                  -- TODO: I should sit down and sort out how the + and -'s switch as you move up and down the grid
                  if abs (old_x - new_x) == 2 && old_y - new_y == -2 then
                      let
                          neighbor = (old_x - (1 * sign (old_x - new_x)), old_y + 1)
                      in
                          case Dict.get neighbor pieces of
                              Nothing ->
                                  (False, pieces)

                              Just a_piece ->
                                  if a_piece.color /= Blue then
                                      (True, Dict.remove neighbor pieces) else
                                      (False, pieces)

                      else
                          (abs (old_x - new_x) == 1 && old_y - new_y == -1, pieces)

              Yellow ->
                  if abs (old_x - new_x) == 2 && old_y - new_y == 2 then
                      let
                          neighbor = (old_x + (1 * sign (new_x - old_x)), old_y - 1)
                      in
                          case Dict.get neighbor pieces of
                              Nothing ->
                                  (False, pieces)

                              Just a_piece ->
                                  if a_piece.color /= Yellow then
                                      (True, Dict.remove neighbor pieces) else
                                      (False, pieces)

                      else
                          (abs (old_x - new_x) == 1 && old_y - new_y == 1, pieces)


update : Msg -> Model -> Model
update msg model =
    case msg of

        NoOp -> model

        -- This is pretty right-leaning!
        Clicked position ->
            case model.movement of
                Nothing ->
                    { model | movement = Just { position = position }}

                Just movement ->

                    case Dict.get movement.position model.pieces of
                        Nothing -> { model | movement = Nothing }

                        Just a_piece ->

                            let
                                (was_legal, new_pieces) = legal a_piece movement.position position model.pieces
                            in
                              if was_legal
                                  then
                                      let
                                          updated = Dict.insert position a_piece new_pieces
                                          cleaned = Dict.remove movement.position updated
                                      in
                                          { model | movement = Nothing, pieces = cleaned }
                                  else
                                      { model | movement = Nothing }
