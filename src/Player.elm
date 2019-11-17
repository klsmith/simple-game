module Player exposing (Player, create, update, view)

import Arrow exposing (Arrow, create)
import CustomShapes exposing (arrowHead, outlinedCircle)
import Playground exposing (..)
import Round
import Set
import Ternary exposing (tern)


type Player
    = Player PlayerData


type alias PlayerData =
    { x : Number
    , y : Number
    , direction : Number
    , power : Number
    }


create : ( Number, Number ) -> Player
create ( x, y ) =
    Player
        { x = x
        , y = y
        , direction = 0
        , power = 0
        }


speed =
    16


radius =
    32


update : Computer -> Player -> ( Player, List Arrow )
update { keyboard, mouse, screen } (Player player) =
    ( Player
        { player
            | x = updateX player.x keyboard screen
            , y = updateY player.y keyboard screen
            , direction =
                directionFromPoints
                    (toPoint player)
                    (toPoint mouse)
            , power = updatePower player.power mouse
        }
    , createArrows player mouse
    )


updateX : Number -> Keyboard -> Screen -> Number
updateX currentX keyboard screen =
    let
        newX =
            currentX + (speed * toWasdX keyboard)
    in
    if (newX - radius) < screen.left then
        screen.left + radius

    else if (newX + radius) > screen.right then
        screen.right - radius

    else
        newX


toWasdX : Keyboard -> Number
toWasdX { keys } =
    (tern (Set.member "d" keys) 1 <| 0)
        - (tern (Set.member "a" keys) 1 <| 0)


updateY : Number -> Keyboard -> Screen -> Number
updateY currentY keyboard screen =
    let
        newY =
            currentY + (speed * toWasdY keyboard)
    in
    if (newY - radius) < screen.bottom then
        screen.bottom + radius

    else if (newY + radius) > screen.top then
        screen.top - radius

    else
        newY


toWasdY : Keyboard -> Number
toWasdY { keys } =
    (tern (Set.member "w" keys) 1 <| 0)
        - (tern (Set.member "s" keys) 1 <| 0)


type alias Pointable record =
    { record
        | x : Number
        , y : Number
    }


type alias Point =
    ( Number, Number )


toPoint : Pointable record -> Point
toPoint record =
    ( record.x, record.y )


directionFromPoints : Point -> Point -> Number
directionFromPoints ( x1, y1 ) ( x2, y2 ) =
    degreesFromRadians (atan2 (y2 - y1) (x2 - x1))


degreesFromRadians : Number -> Number
degreesFromRadians rads =
    rads * (180 / pi)


updatePower : Number -> Mouse -> Number
updatePower power mouse =
    if mouse.down then
        if power < 1 then
            power + 0.01

        else
            1

    else
        0


createArrows : PlayerData -> Mouse -> List Arrow
createArrows { x, y, power, direction } mouse =
    if power > 0.25 && not mouse.down then
        [ Arrow.create ( x, y ) power direction ]

    else
        []


view : Player -> Shape
view (Player player) =
    group
        [ outlinedCircle white black radius 1
        , aimingArrow player.power
        ]
        |> move player.x player.y
        |> rotate player.direction


aimingArrow : Number -> Shape
aimingArrow power =
    arrowHead (powerColor power) radius
        |> moveX (power * -(radius / 2))


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
