module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

---- MODEL ----


type alias Model =
     List SmartRectangle


init : ( Model, Cmd Msg )
init =
    ( [ {id = 0 , zapped = False, xPosition = 300, yPosition = 0, duration = 7}
      , {id = 1 , zapped = False, xPosition = 600, yPosition = -50, duration = 10}
      , {id = 2 , zapped = False, xPosition = 55,  yPosition = -560, duration = 6}
      , {id = 3 , zapped = False, xPosition = 700, yPosition = -690, duration = 5}
      ]
      , Cmd.none )


type alias SmartRectangle = 
  { id : Int
  , zapped : Bool
  , xPosition : Int 
  , yPosition : Int
  , duration : Int
  }
---- UPDATE ----


type Msg
    = Zap Int


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        Zap indexNo ->
            let
                updateZappedElement smartRectangle = if smartRectangle.id == indexNo then
                                                        {smartRectangle | zapped = True}
                                                     else 
                                                        smartRectangle
                    
            in
            ( List.map updateZappedElement model, Cmd.none )


---- VIEW ----


view : Model -> Html Msg
view model =        
    div []
        [   svg
            [ width "800"
            , height "700"
            , viewBox "0 0 800 700"
            ]
            (List.map displayRectangle model)
        ]

displayRectangle : SmartRectangle -> Svg Msg
displayRectangle smartRectangle =
    if smartRectangle.zapped == False then
        encompassedRectangle smartRectangle.xPosition smartRectangle.yPosition smartRectangle.id smartRectangle.duration
    else
        Svg.text ""

encompassedRectangle : Int -> Int -> Int -> Int -> Svg Msg
encompassedRectangle xPosition yPosition id duration =
    let     
        animationFactor = animate [ from (String.fromInt yPosition), to "800", begin "0s", dur ((String.fromInt duration) ++ "s"), repeatCount "indefinite", attributeName "y"] []    
    in  
        rect
                [ width "100"
                , height "100"
                , fill "red"
                , onClick (Zap id)
                , x (String.fromInt xPosition)
                , y (String.fromInt yPosition)
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
