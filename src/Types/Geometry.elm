module Geometry exposing (..)

type alias Position = (Int, Int)

even : Int -> Bool
even i = i % 2 == 0

odd : Int -> Bool
odd i = not (even i)

sign : Int -> Int
sign x = if x >= 0 then 1 else -1
