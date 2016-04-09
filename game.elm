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

neighborsToNewGen: Array.Array Int -> Array.Array Int
neighborsToNewGen arr =
  Array.indexedMap (\i n ->
    if n == 4 then
      justToInt (Array.get i arr)
    else if n == 3 then
      1
    else 0) arr

type alias World =
  { w: Int
  , h: Int
  , b: Array.Array Int
  }

worldCensus: World -> World
worldCensus world =
  { w = world.w
  , h = world.h
  , b = Array.indexedMap (\n _ -> (neighborCount world.b world.w world.h) n) world.b
      |> neighborsToNewGen
  }

-- World Update

updateWorld : Input -> World -> World
updateWorld {delta} model =
  worldCensus model

-- View

view : World -> Html
view model =
  div []
    [
    div [] (List.map (\n -> text (toString n)) (Array.toList model.b))
    ]


defaultWorld : World
defaultWorld =
  { w = 5
  , h = 5
  , b = Array.fromList [1,0,0,0,0,
                        0,1,1,0,1,
                        0,1,1,0,0,
                        0,0,0,0,1,
                        1,0,0,1,0]
  }

delta =
  Signal.map inSeconds (fps 1)

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


