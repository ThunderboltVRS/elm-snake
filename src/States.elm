module States exposing (init, initialModel, subscriptions)

import Browser.Events
import Keyboard exposing (Key(..), KeyChange(..))
import Random exposing (Seed)
import Types exposing (..)


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( initialModel, Cmd.none )


positions : Int -> List Int -> List Position
positions row columns =
    List.map (\c -> ( row, c )) columns


initialModel : Model
initialModel =
    let
        boardSize =
            18

        rows =
            List.range 1 boardSize

        columns =
            List.range 1 boardSize

        board =
            rows
                |> List.map (\c -> positions c columns)
    in
    { board = board
    , snake = [ ( 5, 6 ), ( 5, 5 ), ( 5, 4 ), ( 5, 3 ), ( 5, 2 ) ]
    , food = ( 2, 1 )
    , direction = Right
    , previousDirection = Right
    , boardSize = boardSize
    , state = Paused
    , tickDelta = 0
    , gameSpeed = 150
    , seed = Random.initialSeed 10
    }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.downs Press
        , Browser.Events.onAnimationFrameDelta Tick
        ]
