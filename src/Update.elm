module Update exposing (update)

import Keyboard exposing (rawValue)
import Random
import Set
import States exposing (initialModel)
import Types exposing (..)


coordGenerator : Int -> Random.Generator Int
coordGenerator boardSize =
    Random.int 1 boardSize


positionGenerator : Int -> Random.Generator Position
positionGenerator boardSize =
    Random.pair (coordGenerator boardSize) (coordGenerator boardSize)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Press key ->
            let
                rKey =
                    rawValue key
            in
            case rKey of
                " " ->
                    -- Space
                    ( { model
                        | state =
                            case model.state of
                               Playing -> Paused
                               Paused -> Playing
                               _ -> model.state
                      }
                    , Cmd.none
                    )

                "ArrowLeft" ->
                    ( changeDirection model Left, Cmd.none )

                "ArrowRight" ->
                    ( changeDirection model Right, Cmd.none )

                "ArrowUp" ->
                    ( changeDirection model Up, Cmd.none )

                "ArrowDown" ->
                    ( changeDirection model Down, Cmd.none )

                "r" ->
                    ( initialModel, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        Tick delta ->
            let
                newDelta =
                    model.tickDelta + delta
            in
            if newDelta < model.gameSpeed then
                ( { model | tickDelta = newDelta }, Cmd.none )

            else
                ( { model | tickDelta = 0 } |> gameLoop, Cmd.none )


changeDirection : Model -> Direction -> Model
changeDirection model direction =
    let
        attempt =
            [ direction, model.previousDirection ]

        disallowed =
            List.any (\a -> a == attempt) opposites
    in
    if disallowed then
        model

    else
        { model | direction = direction }


opposites : List (List Direction)
opposites =
    [ [ Up, Down ]
    , [ Down, Up ]
    , [ Left, Right ]
    , [ Right, Left ]
    ]


positionsMatch : Maybe Position -> Maybe Position -> Bool
positionsMatch mA mB =
    case mA of
        Nothing ->
            False

        Just a ->
            case mB of
                Nothing ->
                    False

                Just b ->
                    a == b


isOutOfBounds : Position -> Int -> Bool
isOutOfBounds head boardSize =
    let
        x =
            Tuple.first head

        y =
            Tuple.second head
    in
    if
        x
            > boardSize
            || x
            < 1
            || y
            > boardSize
            || y
            < 1
    then
        True

    else
        False


isEatingSelf : List Position -> Bool
isEatingSelf snake =
    case snake of
        [] ->
            False

        [ a ] ->
            False

        a :: rest ->
            List.any (\i -> i == a) rest


dropLast : List Position -> List Position
dropLast snake =
    List.take (List.length snake - 1) snake


gameLoop : Model -> Model
gameLoop model =
    if model.state == Playing then
        move model

    else
        model


updateForFood : Model -> Model
updateForFood model =
    let
        newPosition =
            Random.step (positionGenerator model.boardSize) model.seed

        isInSnake =
            List.any (\s -> s == Tuple.first newPosition) model.snake
    in
    if isInSnake then
        updateForFood { model | seed = Tuple.second newPosition }

    else
        { model | food = Tuple.first newPosition }


move : Model -> Model
move model =
    let
        currentHead =
            Maybe.withDefault ( 0, 0 ) (List.head model.snake)

        newHead =
            moveInDirection currentHead model.direction

        newSnake =
            newHead :: model.snake

        isEatingFood =
            positionsMatch (List.head newSnake) (Just model.food)
    in
    if isEatingSelf newSnake then
        { model | state = EatenTail }

    else if isOutOfBounds newHead model.boardSize then
        { model | state = HitWall }

    else if isEatingFood then
        { model | snake = newSnake } |> updateForFood

    else
        { model
            | snake = dropLast newSnake
            , previousDirection = model.direction
        }


moveInDirection : Position -> Direction -> Position
moveInDirection head direction =
    addVector head (directionToVector direction)


addVector : Position -> Position -> Position
addVector left right =
    let
        x =
            Tuple.first left + Tuple.first right

        y =
            Tuple.second left + Tuple.second right
    in
    ( x, y )


directionToVector : Direction -> ( Int, Int )
directionToVector direction =
    case direction of
        Up ->
            ( -1, 0 )

        Down ->
            ( 1, 0 )

        Left ->
            ( 0, -1 )

        Right ->
            ( 0, 1 )
