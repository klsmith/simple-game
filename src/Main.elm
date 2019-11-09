module Main exposing (..)

import Browser exposing (element)
import Html exposing (Html, text)



-- MAIN


main =
    Browser.element
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }



-- MODEL


type Model
    = NoModel



-- MSG


type Msg
    = NoMsg



-- INIT


init : () -> ( Model, Cmd Msg )
init _ =
    ( NoModel, Cmd.none )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



--VIEW


view : Model -> Html Msg
view model =
    text "Hello World"
