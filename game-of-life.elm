module Main exposing (..)

import Dict exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Svg exposing (Svg)
import Svg.Attributes
import Time exposing (Time, millisecond)


type alias Cell =
    { x : Int
    , y : Int
    }


type alias CellWithNeighbors =
    { x : Int
    , y : Int
    , n : Int
    , s : Bool
    }


type alias World =
    Dict String Cell


type alias WorldNeighbors =
    Dict String CellWithNeighbors


type Msg
    = Advance Time
    | Chill World


coordToKey : Int -> Int -> String
coordToKey x y =
    (toString x) ++ "/" ++ (toString y)


incNeighborCount : Int -> Int -> Maybe CellWithNeighbors -> Maybe CellWithNeighbors
incNeighborCount x y i =
    case i of
        Nothing ->
            Just { y = y, x = x, s = False, n = 1 }

        Just i ->
            Just { i | n = (i.n + 1) }


updateSelf : Int -> Int -> Maybe CellWithNeighbors -> Maybe CellWithNeighbors
updateSelf x y cell =
    case cell of
        Nothing ->
            Just { y = y, x = x, s = True, n = 0 }

        Just cell ->
            Just { cell | s = True }


addNeighborTo : Int -> Int -> WorldNeighbors -> WorldNeighbors
addNeighborTo x y =
    Dict.update (coordToKey x y) (incNeighborCount x y)


worldToNeighborCount : a -> Cell -> WorldNeighbors -> WorldNeighbors
worldToNeighborCount key cell neighbors =
    let
        x =
            cell.x

        y =
            cell.y
    in
        neighbors
            |> Dict.update (coordToKey x y) (updateSelf x y)
            |> addNeighborTo (x - 1) y
            |> addNeighborTo (x - 1) (y + 1)
            |> addNeighborTo (x - 1) (y - 1)
            |> addNeighborTo x (y - 1)
            |> addNeighborTo x (y + 1)
            |> addNeighborTo (x + 1) y
            |> addNeighborTo (x + 1) (y - 1)
            |> addNeighborTo (x + 1) (y + 1)


killSpawnLive :
    String
    -> CellWithNeighbors
    -> World
    -> World
killSpawnLive key cell newWorld =
    -- If the cell has 3 neighbors, it either is spawned or stays alive.
    -- In either case, add this cell to the new state of the world
    -- If the cell is alive and has 2 neighbors it perisists to the new
    -- state of the world.
    -- In all other cases the cell either stays dead or is killed. So just
    -- return the world as is.
    if cell.n == 3 then
        Dict.insert key { x = cell.x, y = cell.y } newWorld
    else if cell.s && cell.n == 2 then
        Dict.insert key { x = cell.x, y = cell.y } newWorld
    else
        newWorld


worldCensus : World -> World
worldCensus dict =
    dict
        |> Dict.foldl worldToNeighborCount Dict.empty
        |> Dict.foldl killSpawnLive Dict.empty


renderCell : Int -> Int -> Cell -> Svg msg
renderCell xOrigin yOrigin cell =
    Svg.rect
        [ Svg.Attributes.width "10"
        , Svg.Attributes.height "10"
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.x (toString (cell.x * 10 + xOrigin))
        , Svg.Attributes.y (toString (cell.y * 10 + yOrigin))
        , Svg.Attributes.fill "black"
        ]
        []



-- UPDATE


update : Msg -> World -> ( World, Cmd Msg )
update msg world =
    case msg of
        Advance newTime ->
            ( worldCensus world, Cmd.none )

        Chill w ->
            ( w, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Float -> World -> Sub Msg
subscriptions millisPerFrame world =
    Time.every (millisecond * millisPerFrame) Advance



-- VIEW


view : Int -> Int -> World -> Html Msg
view w h world =
    Html.div
        []
        [ Html.h1 [] [ Html.text "Conway's Game of Life" ]
        , Svg.svg
            [ width w
            , height h
            ]
            (world
                |> Dict.values
                |> List.map (renderCell (w // 2) (h // 2))
            )
        ]


main : Program Never World Msg
main =
    Html.program
        { init =
            ( Dict.fromList
                [ ( "0/0", { x = 0, y = 0 } )
                , ( "1/0", { x = 1, y = 0 } )
                , ( "-1/1", { x = -1, y = 1 } )
                , ( "0/1", { x = 0, y = 1 } )
                , ( "0/2", { x = 0, y = 2 } )
                ]
            , Cmd.none
            )
        , view = view 1000 700
        , update = update
        , subscriptions = subscriptions 100
        }
