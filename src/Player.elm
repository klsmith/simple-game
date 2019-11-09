module Player exposing (Player, create, update, view)

import Playground exposing (..)
import Round
import Set
import Ternary exposing (tern)


type Player
    = Player
        { x : Number
        , y : Number
        , direction : Number
        }


create : ( Number, Number ) -> Player
create ( x, y ) =
    Player { x = x, y = y, direction = 0 }


speed =
    16


update : Computer -> Player -> Player
update { keyboard, mouse } (Player player) =
    Player
        { x = player.x + (speed * toWasdX keyboard)
        , y = player.y + (speed * toWasdY keyboard)
        , direction =
            directionFromPoints
                (toPoint player)
                (toPoint mouse)
        }


toWasdX : Keyboard -> Number
toWasdX { keys } =
    (tern (Set.member "d" keys) 1 <| 0)
        - (tern (Set.member "a" keys) 1 <| 0)


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


view : Player -> Shape
view (Player player) =
    group
        [ outlinedCircle white black 32 1
        , arrowHead red 32
        ]
        |> move player.x player.y
        |> rotate player.direction


outlinedCircle : Color -> Color -> Number -> Number -> Shape
outlinedCircle outlineColor fillColor size outlineSize =
    group
        [ circle outlineColor size
        , circle fillColor (size - outlineSize)
        ]


arrowHead : Color -> Number -> Shape
arrowHead color radius =
    polygon
        red
        [ ( -(radius / 2), radius / 4 )
        , ( -(radius / 4), 0 )
        , ( -(radius / 2), -(radius / 4) )
        , ( radius, 0 )
        ]
