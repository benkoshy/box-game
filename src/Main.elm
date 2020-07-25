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
        [   svg
            [ width "800"
            , height "800"
            , viewBox "0 0 800 800"
            ]
            [ (encompassedRectangle model 30 )
            , (encompassedRectangle model 400 )
            , (encompassedRectangle model 600 )
            ]
            
        ]

encompassedRectangle : Model -> Int -> Svg Msg
encompassedRectangle model xPosition =
    let
        color = if model.zapped == True then
                  "red"
                else 
                   "black"     
        animationFactor = animate [ from "200", to "800", begin "0s", dur "13s", repeatCount "indefinite", attributeName "y"] []    
    in    
        rect
                [ width "100"
                , height "100"
                , fill color
                , onClick Zap
                , x (String.fromInt xPosition)
                , y "0"
                , rx "15", ry "15"
                ]
                [ animationFactor
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
