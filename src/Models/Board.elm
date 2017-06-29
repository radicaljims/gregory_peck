module Board exposing (..)

-- import Debug

import Array exposing (..)
import Dict exposing (Dict)
import Maybe exposing (Maybe, andThen)

import Geometry exposing (..)
import Piece exposing (..)

type alias Dimensions = { x : Int, y : Int }

type alias Cells = Array (Array Position)
type alias Pieces = Dict Position Piece

type alias MoveState = { position : Position }

type alias Score = { blue : Int, yellow : Int }

type alias Model = { dimensions : Dimensions, cells : Cells, pieces : Pieces, movement : Maybe MoveState
                   , score : Score }

defaultDimensions : Dimensions
defaultDimensions = { x = 10, y = 10 }
-- defaultDimensions = { x = 4, y = 4 }

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
        blues = List.append (piecesForRow 0 Blue dimensions) (List.append (piecesForRow 1 Blue dimensions) (piecesForRow 2 Blue dimensions))
        yellows = List.append (piecesForRow (y - 1) Yellow dimensions) (List.append (piecesForRow (y - 2) Yellow dimensions) (piecesForRow (y - 3) Yellow dimensions))

        -- blues = piecesForRow 0 Blue dimensions
        -- yellows = piecesForRow (y - 1) Yellow dimensions
    in
        Dict.fromList (List.append blues yellows)

defaultBoard : Model
defaultBoard =
    let
      cells = createCells defaultDimensions
      pieces = createPieces defaultDimensions
    in
      { dimensions = defaultDimensions, cells = cells, pieces = pieces, movement = Nothing
      , score = { blue = 0, yellow = 0} }

type Msg = NoOp | Clicked Position | ClickClear


-- TODO: after each move we will have to test if another attack is possible, and not auto switch sides
-- since I think I read a player HAS to take all jumps

check_neighbor : Piece.Color -> Position -> Pieces -> (Bool, Bool, Pieces)
check_neighbor color neighbor pieces =

  case Dict.get neighbor pieces of
      Nothing ->
          (False, False, pieces)

      Just a_piece ->
          if a_piece.color /= color then
              (True, True, Dict.remove neighbor pieces) else
              (False, False, pieces)


legal : Piece -> Position -> Position -> Pieces -> (Bool, Bool, Pieces)
legal piece (old_x, old_y) (new_x, new_y) pieces =

  if (Dict.member (new_x, new_y) pieces)
    then
      (False, False, pieces)
    else

      let
          directionCompensation = if piece.color == Blue then -1 else 1
      in
          if piece.crowned then
            if abs (old_x - new_x) == 2 && (abs (old_y - new_y)) == 2 then
                let
                    nx = old_x + (sign (new_x - old_x))
                    ny = old_y + (sign (new_y - old_y))
                in
                    check_neighbor piece.color (nx, ny) pieces
                else
                    (abs (old_x - new_x) == 1 && (abs (old_y - new_y)) == 1, False, pieces)
          else
            if abs (old_x - new_x) == 2 && (directionCompensation * (old_y - new_y)) == 2 then
                let
                    nx = if piece.color == Blue then old_x - (sign (old_x - new_x)) else old_x + (sign (new_x - old_x))
                    ny = old_y - directionCompensation
                in
                    check_neighbor piece.color (nx, ny) pieces
                else
                    (abs (old_x - new_x) == 1 && (directionCompensation * (old_y - new_y)) == 1, False, pieces)


updateScore : Bool -> Color -> Score -> Score
updateScore increment color score =
    if increment then
        if color == Blue then
            { score | blue = score.blue + 1 } else
            { score | yellow = score.yellow + 1 }
    else
        score


update : Msg -> Model -> Model
update msg model =
    case msg of

        NoOp -> model

        Clicked position ->
            case model.movement of
                Nothing ->
                    { model | movement = Just { position = position }}

                Just movement ->
                    if position == movement.position
                        then
                          model

                        else
                          case Dict.get movement.position model.pieces of
                              Nothing -> { model | movement = Nothing }

                              Just a_piece ->
                                let
                                  (was_legal, killed, new_pieces) = legal a_piece movement.position position model.pieces
                                in
                                  if was_legal
                                    then
                                      let
                                        (x, y) = position
                                        should_crown = if not (a_piece.crowned) then
                                                           if a_piece.color == Blue then (y == model.dimensions.y - 1) else (y == 0)
                                                       else
                                                           True
                                        crowned = { a_piece | crowned = should_crown }
                                        updated = Dict.insert position crowned new_pieces
                                        cleaned = Dict.remove movement.position updated
                                        new_score = updateScore killed a_piece.color model.score
                                        actives = Dict.size cleaned
                                      in
                                          if new_score.blue == actives || new_score.yellow == actives then
                                              defaultBoard else
                                              { model | movement = Nothing, pieces = cleaned, score = new_score }
                                    else
                                        { model | movement = Nothing }

        ClickClear ->
            { model | movement = Nothing }
