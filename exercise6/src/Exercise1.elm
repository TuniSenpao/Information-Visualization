module Exercise1 exposing (..)

import Axis
import Html exposing (Html, text)
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_, polyline, polygon, line, path)
import TypedSvg.Attributes exposing (class,color, fontFamily, fontSize, textAnchor, transform, viewBox, points, d)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y, x1, x2, y1, y2, strokeWidth)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (px, AnchorAlignment(..), Length(..), Transform(..), Paint(..))
import Stat exposing (average)
import Color
import TypedSvg.Attributes exposing (stroke, pathLength)

-- für jeden Punkt mehrere Werte je Dimension
type alias MultiDimPoint =
    { pointName : String
    , value : List Float }

-- List von Dimensionen, die jeweils ihre Liste von Punkten haben
type alias MultiDimData =
    { dimDescription : List String
    , data : List (List MultiDimPoint)
    }

type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }

chosenCarType : CarType
chosenCarType = 
    SUV

chosenCarTypeString : CarType -> String
chosenCarTypeString car_type =
    case car_type of
        SUV ->
            "SUV"
        Small_Sporty_Compact_Large_Sedan ->
            "Small_Sporty_Compact_Large_Sedan"
        Sports_Car ->
            "Sports_Car"
        Wagon ->
            "Wagon"
        Minivan ->
            "Minivan"
        Pickup ->
            "Pickup"

w : Float
w =
    900


h : Float
h =
    450


padding : Float
padding =
    60


radius : Float
radius =
    5.0


tickCount : Int
tickCount =
    5


defaultExtent : ( number, number1 )
defaultExtent =
    ( 0, 100 )

filterCarsAndCarModel : List Car -> ( List Car, List Car)
filterCarsAndCarModel all_cars = 
    let 
        is_car : Car -> Bool
        is_car car = 
            if car.carType == chosenCarType then
                case (car.cityMPG , car.retailPrice) of
                    (Just _, Just _) ->
                        case (car.carLen, car.dealerCost) of 
                            (Just _, Just _) ->
                                True
                            _ -> 
                                False   
                    _ ->
                        False
            else
                False
    in 

    List.partition (\x -> is_car x) all_cars

filteredModelCars : List Car
filteredModelCars =  
            Tuple.first <| filterCarsAndCarModel cars

parallelCoordinatesPlot : Float -> Float -> MultiDimData -> Svg msg
parallelCoordinatesPlot w ar model =
    svg[][]

main : Html msg
main =
    Html.div[][]
