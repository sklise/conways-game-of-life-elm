module GameOfLife exposing (..)

import Dict exposing (..)
import Html exposing (Html, a, button, div, h1, span, text)
import Html.Attributes exposing (class, href, style, target)
import Html.Events exposing (onClick)
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


type alias Model =
    { cellSize : Int
    , height : Int
    , paused : Bool
    , speed : Float
    , width : Int
    , world : World
    }


type alias World =
    Dict String Cell


type alias WorldNeighbors =
    Dict String CellWithNeighbors


type Msg
    = Advance Time
    | Chill Model
    | IncreaseSpeed
    | DecreaseSpeed
    | Pause
    | SetPattern World
    | StepForward


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


killSpawnLive : String -> CellWithNeighbors -> World -> World
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


renderCell : Int -> Int -> Int -> Cell -> Svg msg
renderCell xOrigin yOrigin cellSize cell =
    Svg.rect
        [ Svg.Attributes.width (toString cellSize)
        , Svg.Attributes.height (toString cellSize)
        , Svg.Attributes.x (toString (cell.x * cellSize + xOrigin))
        , Svg.Attributes.y (toString (cell.y * cellSize + yOrigin))
        , Svg.Attributes.fill "black"
        ]
        []


patternToButton : ( String, List Cell ) -> Html Msg
patternToButton pair =
    button [ onClick (SetPattern (cellListToWorld (Tuple.second pair))) ] [ text (Tuple.first pair) ]


cellToPair : Cell -> ( String, Cell )
cellToPair c =
    ( coordToKey c.x c.y, c )


cellListToWorld : List Cell -> World
cellListToWorld l =
    l
        |> List.map cellToPair
        |> Dict.fromList


methuselahs : List ( String, List Cell )
methuselahs =
    [ ( "r-pentomino"
      , [ { x = 0, y = 0 }
        , { x = 1, y = 0 }
        , { x = -1, y = 1 }
        , { x = 0, y = 1 }
        , { x = 0, y = 2 }
        ]
      )
    , ( "acorn"
      , [ { x = 0, y = 0 }
        , { x = -2, y = -1 }
        , { x = -2, y = 1 }
        , { x = -3, y = 1 }
        , { x = 1, y = 1 }
        , { x = 2, y = 1 }
        , { x = 3, y = 1 }
        ]
      )
    , ( "B-heptomino"
      , [ { x = -1, y = -1 }
        , { x = 1, y = -1 }
        , { x = 2, y = -1 }
        , { x = -1, y = 0 }
        , { x = 0, y = 0 }
        , { x = 1, y = 0 }
        , { x = 0, y = 1 }
        ]
      )
    , ( "Pi-heptomino"
      , [ { x = -1, y = -1 }
        , { x = -1, y = 0 }
        , { x = -1, y = 1 }
        , { x = 0, y = -1 }
        , { x = 1, y = -1 }
        , { x = 1, y = 0 }
        , { x = 1, y = 1 }
        ]
      )
    , ( "rabbits"
      , [ { x = -3, y = -1 }
        , { x = -3, y = 0 }
        , { x = -2, y = 0 }
        , { x = -2, y = 1 }
        , { x = -1, y = 0 }
        , { x = 1, y = -1 }
        , { x = 2, y = -1 }
        , { x = 2, y = 0 }
        , { x = 3, y = -1 }
        ]
      )
    , ( "bunnies"
      , [ { x = -3, y = -1 }
        , { x = -2, y = 2 }
        , { x = -1, y = 0 }
        , { x = -1, y = 1 }
        , { x = 0, y = 2 }
        , { x = 2, y = 1 }
        , { x = 3, y = -1 }
        , { x = 3, y = 0 }
        , { x = 4, y = 1 }
        ]
      )
    , ( "switch engine"
      , [ { x = -3, y = 0 }
        , { x = -2, y = -1 }
        , { x = -2, y = 1 }
        , { x = 0, y = -1 }
        , { x = 0, y = 2 }
        , { x = 1, y = 1 }
        , { x = 1, y = 2 }
        , { x = 2, y = 2 }
        ]
      )
    ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Advance newTime ->
            ( { model | world = worldCensus model.world }, Cmd.none )

        Chill m ->
            ( m, Cmd.none )

        IncreaseSpeed ->
            ( { model | speed = (max 10 (model.speed - 25)) }, Cmd.none )

        DecreaseSpeed ->
            ( { model | speed = model.speed + 25 }, Cmd.none )

        Pause ->
            ( { model | paused = not model.paused }, Cmd.none )

        SetPattern w ->
            ( { model | world = w }, Cmd.none )

        StepForward ->
            ( { model | world = worldCensus model.world }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    if model.paused then
        Sub.none
    else
        Time.every (millisecond * model.speed) Advance



-- VIEW


view : Model -> Html Msg
view model =
    let
        pauseButtonText =
            if model.paused then
                "Play"
            else
                "Pause"

        advanceControls =
            if model.paused then
                [ button [ onClick StepForward ] [ text "Step" ]
                ]
            else
                [ button [ onClick IncreaseSpeed ] [ text "Speed up" ]
                , button [ onClick DecreaseSpeed ] [ text "Slow Down" ]
                ]

        renderedWorld =
            model.world
                |> Dict.values
                |> List.map (renderCell (model.width // 2) (model.height // 2) model.cellSize)
    in
        div
            []
            [ h1 [] [ text "Conway's Game of Life" ]
            , button [ onClick Pause ] [ text pauseButtonText ]
            , span [] advanceControls
            , div
                [ class "gameoutline"
                , style
                    [ ( "width", (toString model.width) ++ "px" )
                    , ( "height", (toString model.height) ++ "px" )
                    ]
                ]
                [ Svg.svg
                    [ Svg.Attributes.width (toString model.width)
                    , Svg.Attributes.height (toString model.height)
                    ]
                    renderedWorld
                ]
            , div []
                (List.concat
                    [ [ a
                            [ href "http://conwaylife.com/w/index.php?title=Methuselah"
                            , target "_blank"
                            ]
                            [ text "Methuselah Patterns" ]
                      , text ": "
                      ]
                    , (List.map patternToButton methuselahs)
                    ]
                )
            ]



-- MAIN


main : Program Never Model Msg
main =
    Html.program
        { init =
            ( { width = 1000
              , height = 600
              , cellSize = 5
              , paused = False
              , speed = 100
              , world =
                    cellListToWorld
                        [ { x = 0, y = 0 }
                        , { x = 1, y = 0 }
                        , { x = -1, y = 1 }
                        , { x = 0, y = 1 }
                        , { x = 0, y = 2 }
                        ]
              }
            , Cmd.none
            )
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
