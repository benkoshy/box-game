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

randomRectangle : Random.Generator RectangleWithoutId
randomRectangle = Random.map3
    (\x y z -> RectangleWithoutId False x y z)
    (Random.int 0 800)
    (Random.int 0 -1400)
    (Random.int 4 10)

initialRectangles : Random.Generator (List SmartRectangle)
initialRectangles =
    Random.list 10 randomRectangle |> Random.map (List.indexedMap (\i x -> {id = i, zapped = x.zapped, xPosition = x.xPosition, yPosition = x.yPosition, duration = x.duration}))

--  Random.map (List.indexedMap (\id x -> {x | id = id}) ) (Random.list 10 randomRectangle)
generateRectangle : Model -> Random.Generator SmartRectangle
generateRectangle model = 
    randomRectangle |> Random.map (\rectangelWithoutId -> convertToRectangleWithId (List.length model) rectangelWithoutId)


convertToRectangleWithId : Int -> RectangleWithoutId -> SmartRectangle
convertToRectangleWithId id rectangelWithoutId =
    SmartRectangle id rectangelWithoutId.zapped rectangelWithoutId.xPosition rectangelWithoutId.yPosition rectangelWithoutId.duration


type alias RectangleWithoutId = 
 {
    zapped : Bool
  , xPosition : Int 
  , yPosition : Int
  , duration : Int
 }

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



---- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
  Time.every 1000 InitialiseRectangle


---- PROGRAM ----


main : Program () Model Msg
main =
    Browser.element
        { view = view
        , init = \_ -> init
        , update = update
        , subscriptions = subscriptions
        }
