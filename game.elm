import Debug
import Graphics.Element exposing (show)
import Array

leftIndex: Int -> Int -> Int
leftIndex i w =
  if (i % w) == 0 then i - 1 + w else i - 1

rightIndex: Int -> Int -> Int
rightIndex i w =
  if (i + 1) % w == 0 then i + 1 - w else i + 1

buildIndices: Int -> Int -> Int -> List Int
buildIndices i w h =
  let
    t = w * h
    up = (i - w) % t
    down = (i + w) % t
  in
    [ (leftIndex up w), up, (rightIndex up w)
    , (leftIndex i w), i, (rightIndex i w)
    , (leftIndex down w), down, (rightIndex down w)]

getIndices indices cells =
  --Array.filter (\n -> List.member n indices) cells
  List.map (\n -> (Array.get n cells)) indices

justToInt: Maybe Int -> Int
justToInt x =
  case x of
    Just y -> y
    Nothing -> 0

neighborCount board width height =
  (\n -> board
    |> getIndices (buildIndices n width height)
    |> List.map justToInt
    |> List.sum)

width = 4
height = 4
board = Array.fromList [0,0,0,0,
                        0,1,1,0,
                        0,1,1,0,
                        0,0,0,0]

neighborsToNewGen list =
  Array.indexedMap (\i n ->
    if n == 4
    then justToInt (Array.get i board)
    else if n == 3
    then 1
    else 0) list

main =
  Array.indexedMap (\n _ -> (neighborCount board width height) n) board
    |> neighborsToNewGen
    |> show