module Types exposing (Direction(..), Flags, Model, Msg(..), Position, Row, State(..))

import Keyboard exposing (Key(..), RawKey)
import Random exposing (Seed)


type Msg
    = Press RawKey
    | Tick Float


type alias Model =
    { board : List Row
    , snake : List Position
    , food : Position
    , direction : Direction
    , previousDirection : Direction
    , boardSize : Int
    , state : State
    , tickDelta : Float
    , gameSpeed : Float
    , seed : Seed
    }


type Direction
    = Up
    | Left
    | Right
    | Down


type alias Position =
    ( Int, Int )


type alias Row =
    List Position


type State
    = Paused
    | Playing
    | EatenTail
    | HitWall


type alias Flags =
    {}
