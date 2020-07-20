module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

---- MODEL ----


type alias Model =
    { zapped : Bool }


init : ( Model, Cmd Msg )
init =
    ( { zapped = False}, Cmd.none )



---- UPDATE ----


type Msg
    = Zap


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( {model | zapped = not model.zapped}, Cmd.none )


---- VIEW ----


view : Model -> Html Msg
view model =

        
    div []
        [    svg
            [ width "120"
            , height "1000"
            , viewBox "0 0 120 1000"
            ]
            (encompassedRectangle model)
            
        ]

encompassedRectangle : Model -> List (Svg Msg)
encompassedRectangle model =
    let
        color = if model.zapped == True then
                  "red"
                else 
                   "black"     
        animationFactor = animate [ from "0", to "1000", begin "0s", dur "3s", repeatCount "indefinite", attributeName "y"] []    
    in    
        [   rect
                [ width "120"
                , height "120"
                , onClick Zap
                , fill color
                , opacity "0"
                ] [animationFactor] 
        ,
            rect
                [ width "100"
                , height "100"
                , onClick Zap
                , fill color
                , x "10", y "10"
                , rx "15", ry "15"
                ]
                [   animationFactor
                ]
        ]
            

---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = always Sub.none
        }
