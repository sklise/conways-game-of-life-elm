module Main exposing (..)

import Array
import Html exposing (..)
import Html.Attributes exposing (..)
import Random
import Svg exposing (Svg)
import Svg.Attributes
import Time exposing (Time, millisecond)


leftIndex : Int -> Int -> Int
leftIndex i w =
    if (i % w) == 0 then
        i - 1 + w
    else
        i - 1


rightIndex : Int -> Int -> Int
rightIndex i w =
    if (i + 1) % w == 0 then
        i + 1 - w
    else
        i + 1


buildIndices : Int -> Int -> Int -> List Int
buildIndices i w h =
    let
        t =
            w * h

        up =
            (i - w) % t

        down =
            (i + w) % t
    in
        [ (leftIndex up w)
        , up
        , (rightIndex up w)
        , (leftIndex i w)
        , i
        , (rightIndex i w)
        , (leftIndex down w)
        , down
        , (rightIndex down w)
        ]


getNeighbors indices cells =
    List.map (\n -> Array.get n cells |> Maybe.withDefault 0) indices


countNeighbors board width height =
    let
        boardArr =
            Array.fromList board
    in
        (\n ->
            boardArr
                |> getNeighbors (buildIndices n width height)
                |> List.sum
        )


neighborsToNewGen : Int -> Int -> Int
neighborsToNewGen boardVal neighborCount =
    if neighborCount == 4 then
        boardVal
    else if neighborCount == 3 then
        1
    else
        0


worldCensus : World -> World
worldCensus world =
    let
        listBoard =
            Array.fromList world.b

        nc =
            Array.toList (Array.indexedMap (\n _ -> (countNeighbors world.b world.w world.h) n) listBoard)
    in
        { world
            | b = (List.map2 neighborsToNewGen world.b nc)
        }


cellToDiv : Int -> Int -> Int -> Int -> Int -> Svg a
cellToDiv width height cellSize index cell =
    let
        x = (index % width)
            |> (*) cellSize
            |> toString
        y = (index // width)
            |> (*) cellSize
            |> toString
        fillColor =
            if cell == 1 then
                "black"
            else
                "white"
    in
        Svg.rect
            [ Svg.Attributes.width (toString cellSize)
            , Svg.Attributes.height (toString cellSize)
            , Svg.Attributes.x x
            , Svg.Attributes.y y
            , Svg.Attributes.fill fillColor
            ]
            []


renderWorld : World -> List (Html a)
renderWorld world =
    (List.indexedMap (cellToDiv world.w world.h world.cellSize) world.b)



-- MODEL
-- World is an array of cell states and dimensions of the world


type alias World =
    { w : Int
    , h : Int
    , cellSize : Int
    , b : List Int
    }


init : Int -> Int -> Int -> ( World, Cmd Msg )
init w h cellSize =
    (
        World w h cellSize (List.range 0 (w * h)),
        Random.generate Chill (Random.list (w * h) (Random.int 0 1))
    )



-- UPDATE


type Msg
    = Advance Time
    | Chill (List Int)


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        Advance newTime ->
            ( worldCensus world, Cmd.none )

        Chill l ->
            ( { world | b = l }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : World -> Sub Msg
subscriptions world =
    Time.every (millisecond * 1000) Advance



-- VIEW


view : World -> Html Msg
view world =
    Svg.svg [ Svg.Attributes.viewBox "0 0 200 200", width 1000, height 1000 ] (renderWorld world)


main =
    Html.program
        { init = init 100 100 5
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
