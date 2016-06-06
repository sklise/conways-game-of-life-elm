import Array
import Html exposing (..)
import Html.App as Html
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Random
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


cellToDiv : Int -> Int -> Int -> Html a
cellToDiv width index cell =
    div
        [ style
            [ ( "backgroundColor"
              , if cell == 1 then
                    "black"
                else
                    "white"
              )
            , ( "width", "5px" )
            , ( "height", "5px" )
            , ( "float", "left" )
            , ( "clear"
              , if index % width == 0 then
                    "left"
                else
                    "none"
              )
            ]
        ]
        [ text " " ]


renderWorld : World -> List (Html a)
renderWorld world =
    (List.indexedMap (cellToDiv world.w) world.b)



-- MODEL
-- World is an array of cell states and dimensions of the world


type alias World =
    { w : Int
    , h : Int
    , b : List Int
    }


init : Int -> Int -> ( World, Cmd Msg )
init w h =
    ( World w h [0..w * h], Random.generate Chill (Random.list (w * h) (Random.int 0 1)) )



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
    Time.every (millisecond * 200) Advance



-- VIEW


view : World -> Html Msg
view world =
    div []
        [ div [] (renderWorld world)
        ]


main =
    Html.program
        { init = init 50 50
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
