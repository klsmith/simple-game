module Main exposing (main)

import Browser exposing (element)
import Html exposing (Html, text)
import ListUtil
import Player exposing (Player)
import Playground exposing (..)
import Round
import Ternary exposing (tern)



-- MAIN


main =
    game view update init


init =
    { title = { text = "SIMPLE GAME", state = Start }
    , player = Player.create ( 0, 0 )
    , tick = 0
    }



-- MEMORY


type alias Memory =
    { title : Title
    , player : Player
    , tick : Number
    }


type alias Title =
    { text : String
    , state : TitleState
    }


type TitleState
    = Start
    | FadeIn
    | Linger
    | FadeOut
    | Dead



-- UPDATE


update : Computer -> Memory -> Memory
update computer ({ player, title, tick } as memory) =
    let
        newPlayer =
            Player.update computer player

        newTitle =
            animateTitle title tick
    in
    { memory
        | player = newPlayer
        , title = newTitle
        , tick = tick + 1
    }


animateTitle : Title -> Number -> Title
animateTitle ({ text, state } as title) tick =
    { title | state = animateTitleState state tick }


animateTitleState : TitleState -> Number -> TitleState
animateTitleState state tick =
    case state of
        Start ->
            tern (tick > 0) FadeIn <| state

        FadeIn ->
            tern (tick > 300) Linger <| state

        Linger ->
            tern (tick > 550) FadeOut <| state

        FadeOut ->
            tern (tick > 750) Dead <| state

        Dead ->
            state



--VIEW


view : Computer -> Memory -> List Shape
view { screen, time } { title, player, tick } =
    let
        titleShape =
            viewTitle title tick
    in
    ListUtil.appendMaybe
        [ background screen
        , Player.view player
        ]
        titleShape


background : Screen -> Shape
background { width, height } =
    group
        [ rectangle white width height
        , rectangle black (width - 2) (height - 2)
        ]


viewTitle : Title -> Number -> Maybe Shape
viewTitle { text, state } tick =
    let
        shape =
            words white text
                |> moveY 128
                |> scale 4
    in
    case state of
        Start ->
            Just (fade 0 shape)

        FadeIn ->
            Just (fade (tick / 300) shape)

        Linger ->
            Just shape

        FadeOut ->
            Just (fade (1 - ((tick - 550) / 200)) shape)

        Dead ->
            Nothing



-- Start ->
--     tern (tick > 0) FadeIn <| state
--
-- FadeIn ->
--     tern (tick > 300) Linger <| state
--
-- Linger ->
--     tern (tick > 550) FadeOut <| state
--
-- FadeOut ->
--     tern (tick > 750) Dead <| state
--
-- Dead ->
--     state
