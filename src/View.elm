module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Html.Lazy exposing (lazy)
import Types exposing (..)
import Util


stateToString : State -> String
stateToString state =
    case state of
        Paused ->
            "Paused"

        Playing ->
            "Playing"

        EatenTail ->
            "Lost! You ate your tail. R key to play again."

        HitWall ->
            "Lost! You bumped into the wall. R key to play again."


score : List Position -> String
score snake =
    List.length snake |> String.fromInt


view : Model -> Html Msg
view model =
    div []
        [ lazy gameTitle model.state
        , board model
        , lazy gameScore model.snake
        ]


gameTitle : State -> Html Msg
gameTitle state =
    h1 [ class "title" ]
        [ text (stateToString state) ]


gameScore : List Position -> Html Msg
gameScore snake =
    h2 [] [ text ("Score: " ++ score snake) ]


board model =
    table [ class "table" ]
        [ tbody []
            (List.map (\row -> tableRow model row) model.board)
        ]


tableRow : Model -> List Position -> Html Msg
tableRow model positions =
    tr []
        (List.map (\row -> tableItem model row) positions)


tableItem : Model -> Position -> Html Msg
tableItem model position =
    let
        isSnake =
            List.any (\s -> s == position) model.snake

        isFood =
            position == model.food
    in
    td
        [ classList
            [ ( "tile", True )
            , ( "food", isFood )
            , ( snakeClasses model position, isSnake )
            ]
        ]
        []


directionClass : Direction -> String
directionClass direction =
    case direction of
        Up ->
            "up"

        Down ->
            "down"

        Left ->
            "left"

        Right ->
            "right"


snakeClasses : Model -> Position -> String
snakeClasses model position =
    let
        isHead =
            Util.valueEqual (Just position) (List.head model.snake)

        isTail =
            Util.valueEqual (Just position) (List.reverse model.snake |> List.head)
    in
    if isHead then
        "snake head-" ++ directionClass model.direction

    else if isTail then
        "snake tail"

    else
        "snake"
