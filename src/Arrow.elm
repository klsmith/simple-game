module Arrow exposing (Arrow, create, update, view)

import CustomShapes exposing (arrowHead)
import Playground exposing (..)


type alias Arrow =
    { alive : Bool
    , data : ArrowData
    }


type alias ArrowData =
    { x : Number
    , y : Number
    , direction : Number
    , power : Number
    }


create : ( Number, Number ) -> Number -> Number -> Arrow
create ( x, y ) power direction =
    { alive = True
    , data =
        { x = x
        , y = y
        , power = power
        , direction = direction
        }
    }


speed : Number
speed =
    24


update : Screen -> Arrow -> Arrow
update screen { alive, data } =
    let
        newData =
            { data
                | x = data.x + xSpeed data.power data.direction
                , y = data.y + ySpeed data.power data.direction
            }
    in
    { alive = isAlive screen newData, data = newData }


isAlive : Screen -> ArrowData -> Bool
isAlive { top, bottom, left, right } arrow =
    arrow.y
        < (top + 32)
        && arrow.y
        > bottom
        - 32
        && arrow.x
        < right
        + 32
        && arrow.x
        > left
        - 32


xSpeed : Number -> Number -> Number
xSpeed power direction =
    (speed * power) * cos (degrees direction)


ySpeed : Number -> Number -> Number
ySpeed power direction =
    (speed * power) * sin (degrees direction)


view : Arrow -> Shape
view { alive, data } =
    arrowHead (powerColor data.power) 32
        |> move data.x data.y
        |> rotate data.direction


powerColor : Number -> Color
powerColor power =
    let
        red =
            255

        green =
            255 - (power * 255)

        blue =
            green
    in
    rgb red green blue
