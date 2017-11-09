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


init : World -> ( World, Cmd Msg )
init world =
    ( world, Cmd.none )


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


worldToNeighborCount : a -> Cell -> Dict String CellWithNeighbors -> Dict String CellWithNeighbors
worldToNeighborCount key cell neighbors =
    let
        x =
            cell.x

        y =
            cell.y

        n0 =
            Dict.update (coordToKey x y) (updateSelf x y) neighbors

        n1 =
            Dict.update (coordToKey (x - 1) y) (incNeighborCount (x - 1) y) n0

        n2 =
            Dict.update (coordToKey (x - 1) (y + 1)) (incNeighborCount (x - 1) (y + 1)) n1

        n3 =
            Dict.update (coordToKey (x - 1) (y - 1)) (incNeighborCount (x - 1) (y - 1)) n2

        n4 =
            Dict.update (coordToKey x (y - 1)) (incNeighborCount x (y - 1)) n3

        n5 =
            Dict.update (coordToKey x (y + 1)) (incNeighborCount x (y + 1)) n4

        n6 =
            Dict.update (coordToKey (x + 1) y) (incNeighborCount (x + 1) y) n5

        n7 =
            Dict.update (coordToKey (x + 1) (y - 1)) (incNeighborCount (x + 1) (y - 1)) n6

        n8 =
            Dict.update (coordToKey (x + 1) (y + 1)) (incNeighborCount (x + 1) (y + 1)) n7
    in
        n8


killSpawnLive :
    String
    -> CellWithNeighbors
    -> Dict String Cell
    -> Dict String Cell
killSpawnLive key entry newDict =
    if (not entry.s && entry.n == 3) then
        Dict.insert key { x = entry.x, y = entry.y } newDict
    else if entry.s && (entry.n == 2 || entry.n == 3) then
        Dict.insert key { x = entry.x, y = entry.y } newDict
    else
        newDict


worldCensus : World -> World
worldCensus dict =
    dict
        |> Dict.foldl worldToNeighborCount Dict.empty
        |> Dict.foldl killSpawnLive Dict.empty


renderCell : Cell -> Svg msg
renderCell cell =
    Svg.rect
        [ Svg.Attributes.width "10"
        , Svg.Attributes.height "10"
        , Svg.Attributes.stroke "white"
        , Svg.Attributes.x (toString (cell.x * 10 + 500))
        , Svg.Attributes.y (toString (cell.y * 10 + 500))
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


subscriptions : World -> Sub Msg
subscriptions world =
    Time.every (millisecond * 500) Advance



-- VIEW


view : World -> Html Msg
view world =
    Svg.svg
        [ width 1000
        , height 1000
        ]
        (List.map renderCell (Dict.values world))


newWorld : List ( String, Cell ) -> World
newWorld l =
    Dict.fromList l


main : Program Never World Msg
main =
    Html.program
        { init =
            ( newWorld
                [ ( "0/0", { x = 0, y = 0 } )
                , ( "1/0", { x = 1, y = 0 } )
                , ( "-1/1", { x = -1, y = 1 } )
                , ( "0/1", { x = 0, y = 1 } )
                , ( "0/2", { x = 0, y = 2 } )
                ]
            , Cmd.none
            )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
