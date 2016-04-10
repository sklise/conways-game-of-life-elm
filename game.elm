import Array
import Debug
import Graphics.Element exposing (show)
import Html exposing (..)
import Time exposing (..)

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

getNeighbors indices cells =
  List.map (\n -> Array.get n cells |> Maybe.withDefault 0) indices

countNeighbors board width height =
  let
    boardArr = Array.fromList board
  in
    (\n -> boardArr
      |> getNeighbors (buildIndices n width height)
      |> List.sum)

neighborsToNewGen: Int -> Int -> Int
neighborsToNewGen boardVal neighborCount =
  if neighborCount == 4 then
    boardVal
  else if neighborCount == 3 then
    1
  else
    0

type alias World =
  { w: Int
  , h: Int
  , b: List Int
  }

worldCensus: World -> World
worldCensus world =
  let
    listBoard = Array.fromList world.b
    nc = Array.toList (Array.indexedMap (\n _ -> (countNeighbors world.b world.w world.h) n) listBoard)
  in
    { world |
      b = (List.map2 neighborsToNewGen world.b nc)
    }

-- World Update

updateWorld : Input -> World -> World
updateWorld {delta} model =
  worldCensus model

-- View
view : World -> Html
view world =
  pre []
    (List.indexedMap (\i n -> text ((toString n) ++ (if (i+1) % world.w == 0 then "\n" else "")) ) world.b)


defaultWorld : World
defaultWorld =
  { w = 7
  , h = 7
  , b = [1,0,1,0,0,0,1,
         0,1,1,1,1,1,1,
         0,1,1,0,0,0,0,
         0,1,1,0,0,0,0,
         0,0,0,0,1,1,0,
         1,0,0,0,1,0,1,
         1,0,0,1,0,0,1]
  }

delta =
  Signal.map inSeconds (fps 0.5)

type alias Input =
  { delta : Time
  }

input : Signal Input
input =
  Signal.sampleOn delta <|
    Signal.map Input
      delta

worldState : Signal World
worldState =
  Signal.foldp updateWorld defaultWorld input

main =
  Signal.map view worldState


