module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

import Random


---- MODEL ----


type alias Model =
     List SmartRectangle


init : ( Model, Cmd Msg )
init =
    ( [ ], Random.generate GenerateRandomRectangles initialRectangles )


randomRectangle : Random.Generator SmartRectangle
randomRectangle = Random.map3
    (\x y -> SmartRectangle 0 False x y)
    (Random.int 0 100)
    (Random.int 0 100)
    (Random.int 4 10)

initialRectangles : Random.Generator (List SmartRectangle)
initialRectangles =
    Random.list 10 randomRectangle


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
    | GenerateRandomRectangles (List SmartRectangle)


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
        GenerateRandomRectangles smartRectangles ->
            let
                orderedRectangles = List.indexedMap (\id x -> {x | id = id}) smartRectangles                    
            in
                (orderedRectangles, Cmd.none)       



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
                , fill "dodgerblue"
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
