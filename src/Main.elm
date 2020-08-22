module Main exposing (..)

import Browser
import Html exposing (Html, text, div, h1, img)
import Html.Attributes exposing (src)

import Svg exposing (..)
import Svg.Attributes exposing (..)
import Svg.Events exposing (onClick, on)

import Random
import Time
import Process
import Task

import Json.Decode as Decode

---- MODEL ----

type alias Model =
     { boxes : List SmartRectangle
     , level : Int
     , timer : Int
     , windowHeight : Int
     , windowWidth : Int
     }

type alias Flags =
    { windowWidth : Int
    , windowHeight : Int
    }


init : Flags -> ( Model, Cmd Msg )
init flags =
    ( {boxes = [ ], level = 1, timer = 0, windowWidth = flags.windowWidth, windowHeight = flags.windowHeight}, Cmd.batch (List.range 1 10 |> List.map (\level -> Random.generate GenerateRandomRectangles (levelListGenerator level flags.windowWidth))))


xRandomMaximum : Int -> Int
xRandomMaximum windowWidth = windowWidth - rectangleWidth

levelListGenerator : Int -> Int -> Random.Generator (List SmartRectangle)
levelListGenerator level windowWidth = 
    let
        multiplicationFactor = 5 

        finalId id = id + previousListTotal

        listTotal level_parameter =  5 + level_parameter  * multiplicationFactor            

        previousListTotal = if level == 1 then
                              1
                            else 
                                listTotal (level - 1)
    in        
    Random.list (listTotal level) (randomSmarterRectangle level windowWidth) |> Random.map  (List.indexedMap (\id x -> {id = ( finalId id), zapped = x.zapped, xPosition = x.xPosition, startingTime = x.startingTime, duration = x.duration, color = x.color}))
                            
                                                            

randomSmarterRectangle : Int -> Int ->  Random.Generator RectangleWithoutId
randomSmarterRectangle level windowWidth = Random.map4
    (\x y z -> RectangleWithoutId False x y z)
    (Random.int rectangleWidth (xRandomMaximum windowWidth)) -- x position
    (startTimeByLevel level)       -- starting time
    (Random.int (minimumDurationByLevel level )(maxDurationByLevel)) -- duration
    ( Random.uniform "black" ["burlywood", "aliceblue", "deeppink", "greenyellow", "orangered"]    ) -- color


startTimeByLevel : Int -> Random.Generator Int
startTimeByLevel level =
    Random.map (\r -> r + (level - 1)* 13) (Random.int 0 8)

minimumDurationByLevel : Int -> Int
minimumDurationByLevel level =
    if level == 1 then
       7
    else if level == 2 then
       6
    else if level == 3 then 
       5
    else if level == 4 then
       4 
    else
       3



maxDurationByLevel : Int
maxDurationByLevel = 10
  


type alias RectangleWithoutId = 
 {
    zapped : Bool
  , xPosition : Int 
  , startingTime : Int
  , duration : Int
  , color : String
 }

type alias SmartRectangle = 
  { id : Int
  , zapped : Bool
  , xPosition : Int 
  , startingTime : Int
  , duration : Int
  , color : String
  }

---- UPDATE ----

type Msg
    = Zap Int
    | Die Int
    | GenerateRandomRectangles (List SmartRectangle)
    | EndLevel
    | Tick Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of 
        Zap indexNo ->
            let
                updateZappedElement smartRectangle = if smartRectangle.id == indexNo then
                                                        {smartRectangle | zapped = True}
                                                     else 
                                                        smartRectangle

                areAllZapped rectangle = rectangle.zapped == True
            in
            if List.all (areAllZapped) (List.map updateZappedElement model.boxes) then
                ( {model | boxes = []}, Cmd.none )
            else
                ( {model | boxes = List.map updateZappedElement model.boxes}, Cmd.none )
        GenerateRandomRectangles rectangles ->
            ({model | boxes = model.boxes ++ rectangles}, Cmd.none)
        EndLevel ->
            (model, Cmd.none) 
        Tick newTime ->
            ( { model | timer = model.timer + 1 }, Cmd.none)
        Die indexNo ->
            (model, Cmd.none)


view : Model -> Html Msg
view model =        
    div []
        [   svg
            [ width (String.fromInt model.windowWidth)
            , height (String.fromInt model.windowHeight)
            , viewBox (String.concat ["0 0 " , (String.fromInt model.windowWidth) , " " , (String.fromInt model.windowHeight)])
            ]
            (List.append (List.map (displayRectangle model) model.boxes ) [ Svg.text_ [ x (String.fromInt (model.windowWidth - 100)), y (String.fromInt (model.windowHeight - 20)) ] [(Svg.text ("Missed: " ++ ( List.filter (\x -> x.zapped == True) model.boxes |> List.length |> String.fromInt  ) ))]] )
            -- ++ ( List.filter (\x -> x.zapped == True) model.boxes |> List.length |> String.fromInt  ) 
            -- (List.map (displayRectangle model) model.boxes )  
            -- [ Svg.text_ [ height =  String.fromInt (model.windowHeight - 10), width = String.fromInt (model.windowHeight - 10) ] [(Svg.text ("Missed: " ++ ( List.filter (\x -> x.zapped == True) model.boxes |> List.length |> String.fromInt  )   ))]]
            -- 
        ]

displayRectangle : Model -> SmartRectangle -> Svg Msg
displayRectangle model smartRectangle  =
    if smartRectangle.zapped == False then
        svg [] [  encompassedRectangle smartRectangle.xPosition smartRectangle.startingTime smartRectangle.id smartRectangle.duration smartRectangle.color model
               ]
    else
        Svg.text ""

rectangleWidth : Int
rectangleWidth  = 100

rectangleHeight : Int
rectangleHeight = 100

startingBoxPosition : Int
startingBoxPosition = -100 - rectangleHeight

encompassedRectangle : Int -> Int -> Int -> Int -> String -> Model -> Svg Msg
encompassedRectangle xPosition startingTime id duration color model =
    let     
        animationFactor = animate [ from (String.fromInt startingBoxPosition), to "800", begin (String.fromInt startingTime), dur ((String.fromInt duration) ++ "s"), repeatCount "1", attributeName "y", onEnd (Die id)] [] 
    in  
        svg []
            [ rect
                [ width (String.fromInt rectangleWidth)
                , height (String.fromInt rectangleHeight)
                , fill (color)
                , onClick (Zap id)
                , x (String.fromInt xPosition)
                , y (String.fromInt startingBoxPosition)
                , rx "15", ry "15"
                ]
                [ animationFactor ] 
            ]
      

---- EVENTS
onEnd : msg -> Attribute msg
onEnd message =
  on "end" (Decode.succeed message)

---- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model = Sub.none
        -- Time.every 1000 Tick 

---- PROGRAM ----

main : Program Flags Model Msg
main =
    Browser.element
        { view = view
        , init = init
        , update = update
        , subscriptions = subscriptions
    }
