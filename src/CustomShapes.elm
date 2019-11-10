module CustomShapes exposing (arrowHead, outlinedCircle)

import Playground exposing (..)


outlinedCircle : Color -> Color -> Number -> Number -> Shape
outlinedCircle outlineColor fillColor size outlineSize =
    group
        [ circle outlineColor size
        , circle fillColor (size - outlineSize)
        ]


arrowHead : Color -> Number -> Shape
arrowHead color radius =
    polygon
        color
        [ ( -(radius / 2), radius / 4 )
        , ( -(radius / 4), 0 )
        , ( -(radius / 2), -(radius / 4) )
        , ( radius, 0 )
        ]
