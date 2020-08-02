module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick)

import Random
import Time


---- MODEL ----

type alias Model =
     List SmartRectangle


init : ( Model, Cmd Msg )
init =
    ( [ ], Random.generate GenerateRandomRectangles initialRectangles )

startingTimeRandomMax : Int
startingTimeRandomMax = 6

xRandomMaximum : Int
xRandomMaximum = 800


randomRectangle : Random.Generator RectangleWithoutId
randomRectangle = Random.map3
    (\x y z -> RectangleWithoutId False x y z)
    (Random.int 0 xRandomMaximum)
    (Random.int 0 startingTimeRandomMax)
    (Random.int 4 10)

--- calculate
--- starting position
--- ending position
--- duration
--- then work out whether: (i) it is to be displayed and secondly, (ii) whether we should end the
--- animation
-- if all of them have passed then proceed to the next level.
-- we need the current time.
-- also we need to iterate via level.


hasCrossedTheFinishLine : SmartRectangle -> Bool
hasCrossedTheFinishLine rectange = 
    False 



initialRectangles : Random.Generator (List SmartRectangle)
initialRectangles =
    Random.list 10 randomRectangle |> Random.map (List.indexedMap (\i x -> {id = i, zapped = x.zapped, xPosition = x.xPosition, startingTime = x.startingTime, duration = x.duration}))

--  Random.map (List.indexedMap (\id x -> {x | id = id}) ) (Random.list 10 randomRectangle)
generateRectangle : Model -> Random.Generator SmartRectangle
generateRectangle model = 
    randomRectangle |> Random.map (\rectangelWithoutId -> convertToRectangleWithId (List.length model) rectangelWithoutId)


convertToRectangleWithId : Int -> RectangleWithoutId -> SmartRectangle
convertToRectangleWithId id rectangelWithoutId =
    SmartRectangle id rectangelWithoutId.zapped rectangelWithoutId.xPosition rectangelWithoutId.startingTime rectangelWithoutId.duration


type alias RectangleWithoutId = 
 {
    zapped : Bool
  , xPosition : Int 
  , startingTime : Int
  , duration : Int
 }

type alias SmartRectangle = 
  { id : Int
  , zapped : Bool
  , xPosition : Int 
  , startingTime : Int
  , duration : Int
  }

---- UPDATE ----

type Msg
    = Zap Int
    | GenerateRandomRectangles (List SmartRectangle)
    | InitialiseRectangle Time.Posix
    | GenerateRandomRectangle SmartRectangle


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
        GenerateRandomRectangles rectangles ->
            (rectangles, Cmd.none)
        GenerateRandomRectangle rectangle ->
            ((model ++ [rectangle]), Cmd.none)
        InitialiseRectangle time ->
            (model, Random.generate GenerateRandomRectangle (generateRectangle model))       

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
        encompassedRectangle smartRectangle.xPosition smartRectangle.startingTime smartRectangle.id smartRectangle.duration
    else
        Svg.text ""


boxHeight : Int
boxHeight = 100

startingBoxPosition : Int
startingBoxPosition = 0 - boxHeight

encompassedRectangle : Int -> Int -> Int -> Int -> Svg Msg
encompassedRectangle xPosition startingTime id duration =
    let     
        animationFactor = animate [ from (String.fromInt startingBoxPosition), to "800", begin (String.fromInt startingTime), dur ((String.fromInt duration) ++ "s"), repeatCount "1", attributeName "y"] []    
    in  
        rect
                [ width "100"
                , height (String.fromInt boxHeight)
                , fill "dodgerblue"
                , onClick (Zap id)
                , x (String.fromInt xPosition)
                , y (String.fromInt startingBoxPosition)
                , rx "15", ry "15"
                ]
                [ animationFactor
                ]        



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model = Sub.none

  -- Time.every 1000 InitialiseRectangle


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
    }
