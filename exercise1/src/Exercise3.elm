module Exercise3 exposing (..)

import Axis
import Html exposing (Html, text)
import Scale exposing (ContinuousScale)
import Statistics
import TypedSvg exposing (circle, g, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, fontFamily, fontSize, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, height, r, width, x, y)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (px, AnchorAlignment(..), Length(..), Transform(..))


    -- my_cars |> List.filterMap ( Maybe.map3 (a,b,c) (a,b, Just c) car.cityMPG car.retailPrice car.vehicleName)
    -- XyData "CityMPG" "RetailPrice"  (List.map (\a -> (a.vehicleName, a.retailPrice, a.cityMPG)) (List.filterMap (\b -> Maybe.map3 (\b -> ) b.retailPrice b.cityMPG b.vehicleName) my_cars))

    --List.filterMap (\a -> Maybe.map2 (+) a.retailPrice a.cityMPG) my_cars    

    -- XyData "Beschriftung" "fehlt" []

    -- List.filterMap
    -- Maybe.map3
    -- my_cars
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


scatterplot : XyData -> Svg msg
scatterplot model =
    let
        {- hier können Sie die Beschriftung des Testpunkts berechnen -}
        kreisbeschriftung : String
        kreisbeschriftung =
            ""

        xValues : List Float
        xValues =
            List.map .x model.data

        yValues : List Float
        yValues =
            List.map .y model.data

        wideExtent : List Float -> ( Float, Float )
        wideExtent values =
            defaultExtent

        xScale : List Float -> ContinuousScale Float
        xScale values =
            Scale.linear ( 0, w - 2 * padding ) ( wideExtent values )


        yScale : List Float -> ContinuousScale Float
        yScale values =
            Scale.linear ( h - 2 * padding, 0 ) ( wideExtent values )

        xScaleLocal : ContinuousScale Float
        xScaleLocal =
            xScale xValues

        yScaleLocal : ContinuousScale Float
        yScaleLocal =
            yScale yValues

        xAxis : List Float -> Svg msg
        xAxis values =
            Axis.bottom [ Axis.tickCount tickCount ] (xScale values)


        yAxis : List Float -> Svg msg
        yAxis values =
            Axis.left [ Axis.tickCount tickCount ] (yScale values)
            
        half : ( Float, Float ) -> Float
        half t =
            (Tuple.second t - Tuple.first t) / 2

        labelPositions : { x : Float, y : Float }
        labelPositions =
            { x = wideExtent xValues |> half
            , y = wideExtent yValues |> Tuple.second
            }


    in
    svg [ viewBox 0 0 w h, TypedSvg.Attributes.width <| TypedSvg.Types.Percent 100, TypedSvg.Attributes.height <| TypedSvg.Types.Percent 100 ]
        [ style [] [ TypedSvg.Core.text """
            .point circle { stroke: rgba(0, 0, 0,0.4); fill: rgba(255, 255, 255,0.3); }
            .point text { display: none; }
            .point:hover circle { stroke: rgba(0, 0, 0,1.0); fill: rgb(118, 214, 78); }
            .point:hover text { display: inline; }
          """ ]
        , g
            [ transform [ Translate (padding - 1) (padding - 1) ]
            , class [ "point" ]
            , fontSize <| Px 10.0
            , fontFamily [ "sans-serif" ]
            ]
            [
            text_ [x -25, y -5, fontFamily ["Helvetica", "sans-serif"], fontSize (px 10) ] 
            [ text kreisbeschriftung],

            circle [ cx 0, cy 0, r (radius) ] [] 
            
            ]
        ]


type alias Point =
    { pointName : String, x : Float, y : Float }


type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }
 
carHasNothingValue : Car -> Maybe Car
carHasNothingValue car =
    if car.retailPrice == Nothing then
        Nothing

    else if car.cityMPG == Nothing then
        Nothing
    
    else
        Just car

pointsToXyData : List Point -> XyData
pointsToXyData points =
    XyData "" "" []
filterAndReduceCars : List Car -> XyData
filterAndReduceCars my_cars = 
    -- TODO
    {- hier kommt ihr Code zum Filtern hin -}

    -- XyData "City MPG" "Retail Price" (List.map (\car -> Maybe.map3 (\name city price -> Point name city price) car.vehicleName car.cityMPG car.retailPrice) my_cars)
    let 
       
        car_to_point : Car -> Maybe Point
        car_to_point car = 
            let
                beschriftung : Int -> Int -> String -> String
                beschriftung c r b = b ++ "(" ++ (String.fromInt c) ++ ", " ++ (String.fromInt r) ++ ")"
            in
            case (car.cityMPG , car.retailPrice) of
                (Just cityMPG, Just retailPrice) ->
                    Just (Point car.vehicleName (toFloat cityMPG) (toFloat retailPrice))    
                _ ->
                    Nothing

        filtered_cars : List Point
        filtered_cars = List.filterMap carPointMap my_cars

        -- zweite Variante von car_to_point
        carPointMap : Car -> Maybe Point
        carPointMap car = 
            let
                beschriftung : Int -> Int -> String -> String
                beschriftung c r b = b ++ "(" ++ (String.fromInt c) ++ ", " ++ (String.fromInt r) ++ ")"

                cityMPG_and_retailPrice_to_point : Int -> Int -> Point
                cityMPG_and_retailPrice_to_point c r = Point (beschriftung c r car.vehicleName) (toFloat c) (toFloat r) 
            in
            Maybe.map2 cityMPG_and_retailPrice_to_point car.cityMPG car.retailPrice
    in

    XyData "cityMPG" "retailPrice" filtered_cars



main : Html msg
main =
    let
        filteredCars =
            filterAndReduceCars cars

        numberCars =
            String.fromInt <| List.length cars

        numberFilterCars =
            String.fromInt <| List.length filteredCars.data
    in
    Html.div []
        [ Html.p []
            [
                -- TODO
                {- Code für die Ausgabe der Listenlängen kommt hierher -}
                text <| "Original Car List: " ++ numberCars  ,
                text " , ",
                text <| "Reduced Car List: " ++ numberFilterCars

            ]
        , scatterplot filteredCars
        ]


type CarType
    = Small_Sporty_Compact_Large_Sedan
    | Sports_Car
    | SUV
    | Wagon
    | Minivan
    | Pickup


type WheelDrive
    = All_Wheel_Drive
    | Rear_Wheel_Drive
    | Front_Wheel_Drive


type alias Car =
    { vehicleName : String
    , carType : CarType
    , wheelDrive : WheelDrive
    , retailPrice : Maybe Int
    , dealerCost : Maybe Int
    , engineSize : Maybe Float
    , cyl : Maybe Float
    , hp : Maybe Int
    , cityMPG : Maybe Int
    , hwyMPG : Maybe Int
    , weight : Maybe Int
    , wheelBase : Maybe Int
    , carLen : Maybe Int
    , carWidth : Maybe Int
    }


cars : List Car
cars =
    [ Car "Acura 3.5 RL 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 43755) (Just 39014) (Just 3.5) (Just 6) (Just 225) (Just 18) (Just 24) (Just 3880) (Just 115) (Just 197) (Just 72)
    , Car "Acura 3.5 RL w/Navigation 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 46100) (Just 41100) (Just 3.5) (Just 6) (Just 225) (Just 18) (Just 24) (Just 3893) (Just 115) (Just 197) (Just 72)
    , Car "Acura MDX" SUV All_Wheel_Drive (Just 36945) (Just 33337) (Just 3.5) (Just 6) (Just 265) (Just 17) (Just 23) (Just 4451) (Just 106) (Just 189) (Just 77)
    , Car "Acura NSX coupe 2dr manual S" Sports_Car Rear_Wheel_Drive (Just 89765) (Just 79978) (Just 3.2) (Just 6) (Just 290) (Just 17) (Just 24) (Just 3153) (Just 100) (Just 174) (Just 71)
    , Car "Acura RSX Type S 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 23820) (Just 21761) (Just 2) (Just 4) (Just 200) (Just 24) (Just 31) (Just 2778) (Just 101) (Just 172) (Just 68)
    , Car "Acura TL 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 33195) (Just 30299) (Just 3.2) (Just 6) (Just 270) (Just 20) (Just 28) (Just 3575) (Just 108) (Just 186) (Just 72)
    , Car "Acura TSX 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 26990) (Just 24647) (Just 2.4) (Just 4) (Just 200) (Just 22) (Just 29) (Just 3230) (Just 105) (Just 183) (Just 69)
    , Car "Audi A4 1.8T 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 25940) (Just 23508) (Just 1.8) (Just 4) (Just 170) (Just 22) (Just 31) (Just 3252) (Just 104) (Just 179) (Just 70)
    , Car "Audi A4 3.0 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 31840) (Just 28846) (Just 3) (Just 6) (Just 220) (Just 20) (Just 28) (Just 3462) (Just 104) (Just 179) (Just 70)
    , Car "Audi A4 3.0 convertible 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 42490) (Just 38325) (Just 3) (Just 6) (Just 220) (Just 20) (Just 27) (Just 3814) (Just 105) (Just 180) (Just 70)
    , Car "Audi A4 3.0 Quattro 4dr auto" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 34480) (Just 31388) (Just 3) (Just 6) (Just 220) (Just 18) (Just 25) (Just 3627) (Just 104) (Just 179) (Just 70)
    , Car "Audi A4 3.0 Quattro 4dr manual" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 33430) (Just 30366) (Just 3) (Just 6) (Just 220) (Just 17) (Just 26) (Just 3583) (Just 104) (Just 179) (Just 70)
    , Car "Audi A4 3.0 Quattro convertible 2dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 44240) (Just 40075) (Just 3) (Just 6) (Just 220) (Just 18) (Just 25) (Just 4013) (Just 105) (Just 180) (Just 70)
    , Car "Audi A41.8T convertible 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 35940) (Just 32506) (Just 1.8) (Just 4) (Just 170) (Just 23) (Just 30) (Just 3638) (Just 105) (Just 180) (Just 70)
    , Car "Audi A6 2.7 Turbo Quattro 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 42840) (Just 38840) (Just 2.7) (Just 6) (Just 250) (Just 18) (Just 25) (Just 3836) (Just 109) (Just 192) (Just 71)
    , Car "Audi A6 3.0 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 36640) (Just 33129) (Just 3) (Just 6) (Just 220) (Just 20) (Just 27) (Just 3561) (Just 109) (Just 192) (Just 71)
    , Car "Audi A6 3.0 Avant Quattro" Wagon All_Wheel_Drive (Just 40840) (Just 37060) (Just 3) (Just 6) (Just 220) (Just 18) (Just 25) (Just 4035) (Just 109) (Just 192) (Just 71)
    , Car "Audi A6 3.0 Quattro 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 39640) (Just 35992) (Just 3) (Just 6) (Just 220) (Just 18) (Just 25) (Just 3880) (Just 109) (Just 192) (Just 71)
    , Car "Audi A6 4.2 Quattro 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 49690) (Just 44936) (Just 4.2) (Just 8) (Just 300) (Just 17) (Just 24) (Just 4024) (Just 109) (Just 193) (Just 71)
    , Car "Audi A8 L Quattro 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 69190) (Just 64740) (Just 4.2) (Just 8) (Just 330) (Just 17) (Just 24) (Just 4399) (Just 121) (Just 204) (Just 75)
    , Car "Audi RS 6 4dr" Sports_Car Front_Wheel_Drive (Just 84600) (Just 76417) (Just 4.2) (Just 8) (Just 450) (Just 15) (Just 22) (Just 4024) (Just 109) (Just 191) (Just 78)
    , Car "Audi S4 Avant Quattro" Wagon All_Wheel_Drive (Just 49090) (Just 44446) (Just 4.2) (Just 8) (Just 340) (Just 15) (Just 21) (Just 3936) (Just 104) (Just 179) (Just 70)
    , Car "Audi S4 Quattro 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 48040) (Just 43556) (Just 4.2) (Just 8) (Just 340) (Just 14) (Just 20) (Just 3825) (Just 104) (Just 179) (Just 70)
    , Car "Audi TT 1.8 convertible 2dr (coupe)" Sports_Car Front_Wheel_Drive (Just 35940) (Just 32512) (Just 1.8) (Just 4) (Just 180) (Just 20) (Just 28) (Just 3131) (Just 95) (Just 159) (Just 73)
    , Car "Audi TT 1.8 Quattro 2dr (convertible)" Sports_Car All_Wheel_Drive (Just 37390) (Just 33891) (Just 1.8) (Just 4) (Just 225) (Just 20) (Just 28) (Just 2921) (Just 96) (Just 159) (Just 73)
    , Car "Audi TT 3.2 coupe 2dr (convertible)" Sports_Car All_Wheel_Drive (Just 40590) (Just 36739) (Just 3.2) (Just 6) (Just 250) (Just 21) (Just 29) (Just 3351) (Just 96) (Just 159) (Just 73)
    , Car "BMW 325Ci 2dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 30795) (Just 28245) (Just 2.5) (Just 6) (Just 184) (Just 20) (Just 29) (Just 3197) (Just 107) (Just 177) (Just 69)
    , Car "BMW 325Ci convertible 2dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 37995) (Just 34800) (Just 2.5) (Just 6) (Just 184) (Just 19) (Just 27) (Just 3560) (Just 107) (Just 177) (Just 69)
    , Car "BMW 325i 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 28495) (Just 26155) (Just 2.5) (Just 6) (Just 184) (Just 20) (Just 29) (Just 3219) (Just 107) (Just 176) (Just 69)
    , Car "BMW 325xi 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 30245) (Just 27745) (Just 2.5) (Just 6) (Just 184) (Just 19) (Just 27) (Just 3461) (Just 107) (Just 176) (Just 69)
    , Car "BMW 325xi Sport" Wagon All_Wheel_Drive (Just 32845) (Just 30110) (Just 2.5) (Just 6) (Just 184) (Just 19) (Just 26) (Just 3594) (Just 107) (Just 176) (Just 69)
    , Car "BMW 330Ci 2dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 36995) (Just 33890) (Just 3) (Just 6) (Just 225) (Just 20) (Just 30) (Just 3285) (Just 107) (Just 176) (Just 69)
    , Car "BMW 330Ci convertible 2dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 44295) (Just 40530) (Just 3) (Just 6) (Just 225) (Just 19) (Just 28) (Just 3616) (Just 107) (Just 177) (Just 69)
    , Car "BMW 330i 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 35495) (Just 32525) (Just 3) (Just 6) (Just 225) (Just 20) (Just 30) (Just 3285) (Just 107) (Just 176) (Just 69)
    , Car "BMW 330xi 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 37245) (Just 34115) (Just 3) (Just 6) (Just 225) (Just 20) (Just 29) (Just 3483) (Just 107) (Just 176) (Just 69)
    , Car "BMW 525i 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 39995) (Just 36620) (Just 2.5) (Just 6) (Just 184) (Just 19) (Just 28) (Just 3428) (Just 114) (Just 191) (Just 73)
    , Car "BMW 530i 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 44995) (Just 41170) (Just 3) (Just 6) (Just 225) (Just 20) (Just 30) (Just 3472) (Just 114) (Just 191) (Just 73)
    , Car "BMW 545iA 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 54995) (Just 50270) (Just 4.4) (Just 8) (Just 325) (Just 18) (Just 26) (Just 3814) (Just 114) (Just 191) (Just 73)
    , Car "BMW 745i 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 69195) (Just 63190) (Just 4.4) (Just 8) (Just 325) (Just 18) (Just 26) (Just 4376) (Just 118) (Just 198) (Just 75)
    , Car "BMW 745Li 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 73195) (Just 66830) (Just 4.4) (Just 8) (Just 325) (Just 18) (Just 26) (Just 4464) (Just 123) (Just 204) (Just 75)
    , Car "BMW M3 convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 56595) (Just 51815) (Just 3.2) (Just 6) (Just 333) (Just 16) (Just 23) (Just 3781) (Just 108) (Just 177) (Just 70)
    , Car "BMW M3 coupe 2dr" Sports_Car Rear_Wheel_Drive (Just 48195) (Just 44170) (Just 3.2) (Just 6) (Just 333) (Just 16) (Just 24) (Just 3415) (Just 108) (Just 177) (Just 70)
    , Car "BMW X3 3.0i" SUV All_Wheel_Drive (Just 37000) (Just 33873) (Just 3) (Just 6) (Just 225) (Just 16) (Just 23) (Just 4023) (Just 110) (Just 180) (Just 73)
    , Car "BMW X5 4.4i" SUV All_Wheel_Drive (Just 52195) (Just 47720) (Just 4.4) (Just 8) (Just 325) (Just 16) (Just 22) (Just 4824) (Just 111) (Just 184) (Just 74)
    , Car "BMW Z4 convertible 2.5i 2dr" Sports_Car Rear_Wheel_Drive (Just 33895) (Just 31065) (Just 2.5) (Just 6) (Just 184) (Just 20) (Just 28) (Just 2932) (Just 98) (Just 161) (Just 70)
    , Car "BMW Z4 convertible 3.0i 2dr" Sports_Car Rear_Wheel_Drive (Just 41045) (Just 37575) (Just 3) (Just 6) (Just 225) (Just 21) (Just 29) (Just 2998) (Just 98) (Just 161) (Just 70)
    , Car "Buick Century Custom 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 22180) (Just 20351) (Just 3.1) (Just 6) (Just 175) (Just 20) (Just 30) (Just 3353) (Just 109) (Just 195) (Just 73)
    , Car "Buick LeSabre Custom 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 26470) (Just 24282) (Just 3.8) (Just 6) (Just 205) (Just 20) (Just 29) (Just 3567) (Just 112) (Just 200) (Just 74)
    , Car "Buick LeSabre Limited 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 32245) (Just 29566) (Just 3.8) (Just 6) (Just 205) (Just 20) (Just 29) (Just 3591) (Just 112) (Just 200) (Just 74)
    , Car "Buick Park Avenue 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 35545) (Just 32244) (Just 3.8) (Just 6) (Just 205) (Just 20) (Just 29) (Just 3778) (Just 114) (Just 207) (Just 75)
    , Car "Buick Park Avenue Ultra 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 40720) (Just 36927) (Just 3.8) (Just 6) (Just 240) (Just 18) (Just 28) (Just 3909) (Just 114) (Just 207) (Just 75)
    , Car "Buick Rainier" SUV All_Wheel_Drive (Just 37895) (Just 34357) (Just 4.2) (Just 6) (Just 275) (Just 15) (Just 21) (Just 4600) (Just 113) (Just 193) (Just 75)
    , Car "Buick Regal GS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 28345) (Just 26047) (Just 3.8) (Just 6) (Just 240) (Just 18) (Just 28) (Just 3536) (Just 109) (Just 196) (Just 73)
    , Car "Buick Regal LS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 24895) (Just 22835) (Just 3.8) (Just 6) (Just 200) (Just 20) (Just 30) (Just 3461) (Just 109) (Just 196) (Just 73)
    , Car "Buick Rendezvous CX" SUV Front_Wheel_Drive (Just 26545) (Just 24085) (Just 3.4) (Just 6) (Just 185) (Just 19) (Just 26) (Just 4024) (Just 112) (Just 187) (Just 74)
    , Car "Cadillac CTS VVT 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 30835) (Just 28575) (Just 3.6) (Just 6) (Just 255) (Just 18) (Just 25) (Just 3694) (Just 113) (Just 190) (Just 71)
    , Car "Cadillac Deville 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 45445) (Just 41650) (Just 4.6) (Just 8) (Just 275) (Just 18) (Just 26) (Just 3984) (Just 115) (Just 207) (Just 74)
    , Car "Cadillac Deville DTS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 50595) (Just 46362) (Just 4.6) (Just 8) (Just 300) (Just 18) (Just 26) (Just 4044) (Just 115) (Just 207) (Just 74)
    , Car "Cadillac Escalade EXT" Pickup All_Wheel_Drive (Just 52975) (Just 48541) (Just 6) (Just 8) (Just 345) (Just 13) (Just 17) (Just 5879) (Just 130) Nothing Nothing
    , Car "Cadillac Escaladet" SUV Front_Wheel_Drive (Just 52795) (Just 48377) (Just 5.3) (Just 8) (Just 295) (Just 14) (Just 18) (Just 5367) (Just 116) (Just 199) (Just 79)
    , Car "Cadillac Seville SLS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 47955) (Just 43841) (Just 4.6) (Just 8) (Just 275) (Just 18) (Just 26) (Just 3992) (Just 112) (Just 201) (Just 75)
    , Car "Cadillac SRX V8" SUV Front_Wheel_Drive (Just 46995) (Just 43523) (Just 4.6) (Just 8) (Just 320) (Just 16) (Just 21) (Just 4302) (Just 116) (Just 195) (Just 73)
    , Car "Cadillac XLR convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 76200) (Just 70546) (Just 4.6) (Just 8) (Just 320) (Just 17) (Just 25) (Just 3647) (Just 106) (Just 178) (Just 72)
    , Car "Chevrolet Astro" Minivan All_Wheel_Drive (Just 26395) (Just 23954) (Just 4.3) (Just 6) (Just 190) (Just 14) (Just 17) (Just 4605) (Just 111) (Just 190) (Just 78)
    , Car "Chevrolet Avalanche 1500" Pickup All_Wheel_Drive (Just 36100) (Just 31689) (Just 5.3) (Just 8) (Just 295) (Just 14) (Just 18) (Just 5678) (Just 130) Nothing Nothing
    , Car "Chevrolet Aveo 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 11690) (Just 10965) (Just 1.6) (Just 4) (Just 103) (Just 28) (Just 34) (Just 2370) (Just 98) (Just 167) (Just 66)
    , Car "Chevrolet Aveo LS 4dr hatch" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 12585) (Just 11802) (Just 1.6) (Just 4) (Just 103) (Just 28) (Just 34) (Just 2348) (Just 98) (Just 153) (Just 66)
    , Car "Chevrolet Cavalier 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 14610) (Just 13697) (Just 2.2) (Just 4) (Just 140) (Just 26) (Just 37) (Just 2617) (Just 104) (Just 183) (Just 69)
    , Car "Chevrolet Cavalier 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 14810) (Just 13884) (Just 2.2) (Just 4) (Just 140) (Just 26) (Just 37) (Just 2676) (Just 104) (Just 183) (Just 68)
    , Car "Chevrolet Cavalier LS 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 16385) (Just 15357) (Just 2.2) (Just 4) (Just 140) (Just 26) (Just 37) (Just 2617) (Just 104) (Just 183) (Just 69)
    , Car "Chevrolet Colorado Z85" Pickup All_Wheel_Drive (Just 18760) (Just 17070) (Just 2.8) (Just 4) (Just 175) (Just 18) (Just 23) (Just 3623) (Just 111) Nothing Nothing
    , Car "Chevrolet Corvette 2dr" Sports_Car Rear_Wheel_Drive (Just 44535) (Just 39068) (Just 5.7) (Just 8) (Just 350) (Just 18) (Just 25) (Just 3246) (Just 105) (Just 180) (Just 74)
    , Car "Chevrolet Corvette convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 51535) (Just 45193) (Just 5.7) (Just 8) (Just 350) (Just 18) (Just 25) (Just 3248) (Just 105) (Just 180) (Just 74)
    , Car "Chevrolet Impala 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 21900) (Just 20095) (Just 3.4) (Just 6) (Just 180) (Just 21) (Just 32) (Just 3465) (Just 111) (Just 200) (Just 73)
    , Car "Chevrolet Impala LS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 25000) (Just 22931) (Just 3.8) (Just 6) (Just 200) (Just 20) (Just 30) (Just 3476) (Just 111) (Just 200) (Just 73)
    , Car "Chevrolet Impala SS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 27995) (Just 25672) (Just 3.8) (Just 6) (Just 240) (Just 18) (Just 28) (Just 3606) (Just 111) (Just 200) (Just 73)
    , Car "Chevrolet Malibu 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 18995) (Just 17434) (Just 2.2) (Just 4) (Just 145) (Just 24) (Just 34) (Just 3174) (Just 106) (Just 188) (Just 70)
    , Car "Chevrolet Malibu LS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 20370) (Just 18639) (Just 3.5) (Just 6) (Just 200) (Just 22) (Just 30) (Just 3297) (Just 106) (Just 188) (Just 70)
    , Car "Chevrolet Malibu LT 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 23495) (Just 21551) (Just 3.5) (Just 6) (Just 200) (Just 23) (Just 32) (Just 3315) (Just 106) (Just 188) (Just 70)
    , Car "Chevrolet Malibu Maxx LS" Wagon Front_Wheel_Drive (Just 22225) (Just 20394) (Just 3.5) (Just 6) (Just 200) (Just 22) (Just 30) (Just 3458) (Just 112) (Just 188) (Just 70)
    , Car "Chevrolet Monte Carlo LS 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 21825) (Just 20026) (Just 3.4) (Just 6) (Just 180) (Just 21) (Just 32) (Just 3340) (Just 111) (Just 198) (Just 73)
    , Car "Chevrolet Monte Carlo SS 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 24225) (Just 22222) (Just 3.8) (Just 6) (Just 200) (Just 18) (Just 28) (Just 3434) (Just 111) (Just 198) (Just 73)
    , Car "Chevrolet Silverado 1500 Regular Cab" Pickup Rear_Wheel_Drive (Just 20310) (Just 18480) (Just 4.3) (Just 6) (Just 200) (Just 15) (Just 21) (Just 4142) (Just 119) Nothing Nothing
    , Car "Chevrolet Silverado SS" Pickup All_Wheel_Drive (Just 40340) (Just 35399) (Just 6) (Just 8) (Just 300) (Just 13) (Just 17) (Just 4804) (Just 144) Nothing Nothing
    , Car "Chevrolet SSR" Pickup Rear_Wheel_Drive (Just 41995) (Just 39306) (Just 5.3) (Just 8) (Just 300) (Just 16) (Just 19) (Just 4760) (Just 116) Nothing Nothing
    , Car "Chevrolet Suburban 1500 LT" SUV Front_Wheel_Drive (Just 42735) (Just 37422) (Just 5.3) (Just 8) (Just 295) (Just 14) (Just 18) (Just 4947) (Just 130) (Just 219) (Just 79)
    , Car "Chevrolet Tahoe LT" SUV All_Wheel_Drive (Just 41465) (Just 36287) (Just 5.3) (Just 8) (Just 295) (Just 14) (Just 18) (Just 5050) (Just 116) (Just 197) (Just 79)
    , Car "Chevrolet Tracker" SUV Front_Wheel_Drive (Just 20255) (Just 19108) (Just 2.5) (Just 6) (Just 165) (Just 19) (Just 22) (Just 2866) (Just 98) (Just 163) (Just 67)
    , Car "Chevrolet TrailBlazer LT" SUV Front_Wheel_Drive (Just 30295) (Just 27479) (Just 4.2) (Just 6) (Just 275) (Just 16) (Just 21) (Just 4425) (Just 113) (Just 192) (Just 75)
    , Car "Chevrolet Venture LS" Minivan Front_Wheel_Drive (Just 27020) (Just 24518) (Just 3.4) (Just 6) (Just 185) (Just 19) (Just 26) (Just 3699) (Just 112) (Just 187) (Just 72)
    , Car "Chrvsler PT Cruiser GT 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 25955) (Just 24172) (Just 2.4) (Just 4) (Just 220) (Just 21) (Just 27) (Just 3217) (Just 103) (Just 169) (Just 67)
    , Car "Chrysler 300M 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 29865) (Just 27797) (Just 3.5) (Just 6) (Just 250) (Just 18) (Just 27) (Just 3581) (Just 113) (Just 198) (Just 74)
    , Car "Chrysler 300M Special Edition 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 33295) (Just 30884) (Just 3.5) (Just 6) (Just 255) (Just 18) (Just 27) (Just 3650) (Just 113) (Just 198) (Just 74)
    , Car "Chrysler Concorde LX 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 24130) (Just 22452) (Just 2.7) (Just 6) (Just 200) (Just 21) (Just 29) (Just 3479) (Just 113) (Just 208) (Just 74)
    , Car "Chrysler Concorde LXi 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 26860) (Just 24909) (Just 3.5) (Just 6) (Just 232) (Just 19) (Just 27) (Just 3548) (Just 113) (Just 208) (Just 74)
    , Car "Chrysler Crossfire 2dr" Sports_Car Rear_Wheel_Drive (Just 34495) (Just 32033) (Just 3.2) (Just 6) (Just 215) (Just 17) (Just 25) (Just 3060) (Just 95) (Just 160) (Just 70)
    , Car "Chrysler Pacifica" Wagon Rear_Wheel_Drive (Just 31230) (Just 28725) (Just 3.5) (Just 6) (Just 250) (Just 17) (Just 23) (Just 4675) (Just 116) (Just 199) (Just 79)
    , Car "Chrysler PT Cruiser 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 17985) (Just 16919) (Just 2.4) (Just 4) (Just 150) (Just 22) (Just 29) (Just 3101) (Just 103) (Just 169) (Just 67)
    , Car "Chrysler PT Cruiser Limited 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 22000) (Just 20573) (Just 2.4) (Just 4) (Just 150) (Just 22) (Just 29) (Just 3105) (Just 103) (Just 169) (Just 67)
    , Car "Chrysler Sebring 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19090) (Just 17805) (Just 2.4) (Just 4) (Just 150) (Just 22) (Just 30) (Just 3173) (Just 108) (Just 191) (Just 71)
    , Car "Chrysler Sebring convertible 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 25215) (Just 23451) (Just 2.4) (Just 4) (Just 150) (Just 22) (Just 30) (Just 3357) (Just 106) (Just 194) (Just 64)
    , Car "Chrysler Sebring Limited convertible 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 30950) (Just 28613) (Just 2.7) (Just 6) (Just 200) (Just 21) (Just 28) (Just 3448) (Just 106) (Just 194) (Just 69)
    , Car "Chrysler Sebring Touring 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 21840) (Just 20284) (Just 2.7) (Just 6) (Just 200) (Just 21) (Just 28) (Just 3222) (Just 108) (Just 191) (Just 71)
    , Car "Chrysler Town and Country Limited" Minivan Front_Wheel_Drive (Just 38380) (Just 35063) (Just 3.8) (Just 6) (Just 215) (Just 18) (Just 25) (Just 4331) (Just 119) (Just 201) (Just 79)
    , Car "Chrysler Town and Country LX" Minivan Front_Wheel_Drive (Just 27490) (Just 25371) (Just 3.3) (Just 6) (Just 180) (Just 19) (Just 26) (Just 4068) (Just 119) (Just 201) (Just 79)
    , Car "CMC Yukon 1500 SLE" SUV Front_Wheel_Drive (Just 35725) (Just 31361) (Just 4.8) (Just 8) (Just 285) (Just 16) (Just 19) (Just 5042) (Just 116) (Just 199) (Just 79)
    , Car "Dodge Caravan SE" Minivan Front_Wheel_Drive (Just 21795) (Just 20508) (Just 2.4) (Just 4) (Just 150) (Just 20) (Just 26) (Just 3862) (Just 113) (Just 189) (Just 79)
    , Car "Dodge Dakota Club Cab" Pickup Rear_Wheel_Drive (Just 20300) (Just 18670) (Just 3.7) (Just 6) (Just 210) (Just 16) (Just 22) (Just 3829) (Just 131) Nothing Nothing
    , Car "Dodge Dakota Regular Cab" Pickup Rear_Wheel_Drive (Just 17630) (Just 16264) (Just 3.7) (Just 6) (Just 210) (Just 16) (Just 22) (Just 3714) (Just 112) Nothing Nothing
    , Car "Dodge Durango SLT" SUV All_Wheel_Drive (Just 32235) (Just 29472) (Just 4.7) (Just 8) (Just 230) (Just 15) (Just 21) (Just 4987) (Just 119) (Just 201) (Just 76)
    , Car "Dodge Grand Caravan SXT" Minivan All_Wheel_Drive (Just 32660) (Just 29812) (Just 3.8) (Just 6) (Just 215) (Just 18) (Just 25) (Just 4440) (Just 119) (Just 201) (Just 79)
    , Car "Dodge Intrepid ES 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 24885) (Just 23058) (Just 3.5) (Just 6) (Just 232) (Just 18) (Just 27) (Just 3487) (Just 113) (Just 204) (Just 75)
    , Car "Dodge Intrepid SE 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 22035) (Just 20502) (Just 2.7) (Just 6) (Just 200) (Just 21) (Just 29) (Just 3469) (Just 113) (Just 204) (Just 75)
    , Car "Dodge Neon SE 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 13670) (Just 12849) (Just 2) (Just 4) (Just 132) (Just 29) (Just 36) (Just 2581) (Just 105) (Just 174) (Just 67)
    , Car "Dodge Neon SXT 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 15040) (Just 14086) (Just 2) (Just 4) (Just 132) (Just 29) (Just 36) (Just 2626) (Just 105) (Just 174) (Just 67)
    , Car "Dodge Ram 1500 Regular Cab ST" Pickup Rear_Wheel_Drive (Just 20215) (Just 18076) (Just 3.7) (Just 6) (Just 215) (Just 16) (Just 21) (Just 4542) (Just 121) Nothing Nothing
    , Car "Dodge Stratus SE 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 20220) (Just 18821) (Just 2.4) (Just 4) (Just 150) (Just 21) (Just 28) (Just 3175) (Just 108) (Just 191) (Just 71)
    , Car "Dodge Stratus SXT 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 18820) (Just 17512) (Just 2.4) (Just 4) (Just 150) (Just 21) (Just 28) (Just 3182) (Just 108) (Just 191) (Just 71)
    , Car "Dodge Viper SRT-10 convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 81795) (Just 74451) (Just 8.3) (Just 10) (Just 500) Nothing Nothing (Just 3410) (Just 99) (Just 176) (Just 75)
    , Car "Ford Crown Victoria 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 24345) (Just 22856) (Just 4.6) (Just 8) (Just 224) (Just 17) (Just 25) (Just 4057) (Just 115) (Just 212) (Just 78)
    , Car "Ford Crown Victoria LX 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 27370) (Just 25105) (Just 4.6) (Just 8) (Just 224) (Just 17) (Just 25) (Just 4057) (Just 115) (Just 212) (Just 78)
    , Car "Ford Crown Victoria LX Sport 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 30315) (Just 27756) (Just 4.6) (Just 8) (Just 239) (Just 17) (Just 25) (Just 4057) (Just 115) (Just 212) (Just 78)
    , Car "Ford Escape XLS" SUV All_Wheel_Drive (Just 22515) (Just 20907) (Just 3) (Just 6) (Just 201) (Just 18) (Just 23) (Just 3346) (Just 103) (Just 173) (Just 70)
    , Car "Ford Excursion 6.8 XLT" SUV All_Wheel_Drive (Just 41475) (Just 36494) (Just 6.8) (Just 10) (Just 310) Nothing Nothing (Just 7190) (Just 137) (Just 227) (Just 80)
    , Car "Ford Expedition 4.6 XLT" SUV Front_Wheel_Drive (Just 34560) (Just 30468) (Just 4.6) (Just 8) (Just 232) (Just 15) (Just 19) (Just 5000) (Just 119) (Just 206) (Just 79)
    , Car "Ford Explorer XLT V6" SUV All_Wheel_Drive (Just 29670) (Just 26983) (Just 4) (Just 6) (Just 210) (Just 15) (Just 20) (Just 4463) (Just 114) (Just 190) (Just 72)
    , Car "Ford F-150 Regular Cab XL" Pickup Rear_Wheel_Drive (Just 22010) (Just 19490) (Just 4.6) (Just 8) (Just 231) (Just 15) (Just 19) (Just 4788) (Just 126) Nothing Nothing
    , Car "Ford F-150 Supercab Lariat" Pickup All_Wheel_Drive (Just 33540) (Just 29405) (Just 5.4) (Just 8) (Just 300) (Just 14) (Just 18) (Just 5464) (Just 133) Nothing Nothing
    , Car "Ford Focus LX 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 13730) (Just 12906) (Just 2) (Just 4) (Just 110) (Just 27) (Just 36) (Just 2606) (Just 103) (Just 168) (Just 67)
    , Car "Ford Focus SE 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 15460) (Just 14496) (Just 2) (Just 4) (Just 130) (Just 26) (Just 33) (Just 2606) (Just 103) (Just 168) (Just 67)
    , Car "Ford Focus SVT 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19135) (Just 17878) (Just 2) (Just 4) (Just 170) (Just 21) (Just 28) (Just 2750) (Just 103) (Just 168) (Just 67)
    , Car "Ford Focus ZTW" Wagon Front_Wheel_Drive (Just 17475) (Just 16375) (Just 2) (Just 4) (Just 130) (Just 26) (Just 33) (Just 2702) (Just 103) (Just 178) (Just 67)
    , Car "Ford Focus ZX3 2dr hatch" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 13270) (Just 12482) (Just 2) (Just 4) (Just 130) (Just 26) (Just 33) (Just 2612) (Just 103) (Just 168) (Just 67)
    , Car "Ford Focus ZX5 5dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 15580) (Just 14607) (Just 2) (Just 4) (Just 130) (Just 26) (Just 33) (Just 2691) (Just 103) (Just 168) (Just 67)
    , Car "Ford Freestar SE" Minivan Front_Wheel_Drive (Just 26930) (Just 24498) (Just 3.9) (Just 6) (Just 193) (Just 17) (Just 23) (Just 4275) (Just 121) (Just 201) (Just 77)
    , Car "Ford Mustang 2dr (convertible)" Sports_Car Rear_Wheel_Drive (Just 18345) (Just 16943) (Just 3.8) (Just 6) (Just 193) (Just 20) (Just 29) (Just 3290) (Just 101) (Just 183) (Just 73)
    , Car "Ford Mustang GT Premium convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 29380) (Just 26875) (Just 4.6) (Just 8) (Just 260) (Just 17) (Just 25) (Just 3347) (Just 101) (Just 183) (Just 73)
    , Car "Ford Ranger 2.3 XL Regular Cab" Pickup Rear_Wheel_Drive (Just 14385) (Just 13717) (Just 2.3) (Just 4) (Just 143) (Just 24) (Just 29) (Just 3028) (Just 111) Nothing Nothing
    , Car "Ford Taurus LX 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 20320) (Just 18881) (Just 3) (Just 6) (Just 155) (Just 20) (Just 27) (Just 3306) (Just 109) (Just 198) (Just 73)
    , Car "Ford Taurus SE" Wagon Front_Wheel_Drive (Just 22290) (Just 20457) (Just 3) (Just 6) (Just 155) (Just 19) (Just 26) (Just 3497) (Just 109) (Just 198) (Just 73)
    , Car "Ford Taurus SES Duratec 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 22735) (Just 20857) (Just 3) (Just 6) (Just 201) (Just 19) (Just 26) (Just 3313) (Just 109) (Just 198) (Just 73)
    , Car "Ford Thunderbird Deluxe convert w/hardtop 2dr" Sports_Car Front_Wheel_Drive (Just 37530) (Just 34483) (Just 3.9) (Just 8) (Just 280) (Just 17) (Just 24) (Just 3780) (Just 107) (Just 186) (Just 72)
    , Car "GMC Canyon Z85 SL Regular Cab" Pickup Rear_Wheel_Drive (Just 16530) (Just 14877) (Just 2.8) (Just 4) (Just 175) (Just 19) (Just 24) (Just 3351) (Just 111) Nothing Nothing
    , Car "GMC Envoy XUV SLE" SUV Front_Wheel_Drive (Just 31890) (Just 28922) (Just 4.2) (Just 6) (Just 275) (Just 15) (Just 19) (Just 4945) (Just 129) (Just 208) (Just 75)
    , Car "GMC Safari SLE" Minivan Rear_Wheel_Drive (Just 25640) (Just 23215) (Just 4.3) (Just 6) (Just 190) (Just 16) (Just 20) (Just 4309) (Just 111) (Just 190) (Just 78)
    , Car "GMC Sierra Extended Cab 1500" Pickup Rear_Wheel_Drive (Just 25717) (Just 22604) (Just 4.8) (Just 8) (Just 285) (Just 17) (Just 20) (Just 4548) (Just 144) Nothing Nothing
    , Car "GMC Sierra HD 2500" Pickup All_Wheel_Drive (Just 29322) (Just 25759) (Just 6) (Just 8) (Just 300) Nothing Nothing (Just 5440) (Just 133) Nothing Nothing
    , Car "GMC Sonoma Crew Cab" Pickup All_Wheel_Drive (Just 25395) (Just 23043) (Just 4.3) (Just 6) (Just 190) (Just 15) (Just 19) (Just 4083) (Just 123) Nothing Nothing
    , Car "GMC Yukon XL 2500 SLT" SUV All_Wheel_Drive (Just 46265) (Just 40534) (Just 6) (Just 8) (Just 325) (Just 13) (Just 17) (Just 6133) (Just 130) (Just 219) (Just 79)
    , Car "Honda Accord EX 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 22260) (Just 20080) (Just 2.4) (Just 4) (Just 160) (Just 26) (Just 34) (Just 3047) (Just 105) (Just 188) (Just 71)
    , Car "Honda Accord EX V6 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 26960) (Just 24304) (Just 3) (Just 6) (Just 240) (Just 21) (Just 30) (Just 3294) (Just 105) (Just 188) (Just 71)
    , Car "Honda Accord LX 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19860) (Just 17924) (Just 2.4) (Just 4) (Just 160) (Just 26) (Just 34) (Just 2994) (Just 105) (Just 188) (Just 71)
    , Car "Honda Accord LX V6 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 23760) (Just 21428) (Just 3) (Just 6) (Just 240) (Just 21) (Just 30) (Just 3349) (Just 108) (Just 190) (Just 72)
    , Car "Honda Civic DX 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 13270) (Just 12175) (Just 1.7) (Just 4) (Just 115) (Just 32) (Just 38) (Just 2432) (Just 103) (Just 175) (Just 67)
    , Car "Honda Civic EX 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 17750) (Just 16265) (Just 1.7) (Just 4) (Just 127) (Just 32) (Just 37) (Just 2601) (Just 103) (Just 175) (Just 68)
    , Car "Honda Civic HX 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 14170) (Just 12996) (Just 1.7) (Just 4) (Just 117) (Just 36) (Just 44) (Just 2500) (Just 103) (Just 175) (Just 67)
    , Car "Honda Civic Hybrid 4dr manual (gas/electric)" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 20140) (Just 18451) (Just 1.4) (Just 4) (Just 93) (Just 46) (Just 51) (Just 2732) (Just 103) (Just 175) (Just 68)
    , Car "Honda Civic LX 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 15850) (Just 14531) (Just 1.7) (Just 4) (Just 115) (Just 32) (Just 38) (Just 2513) (Just 103) (Just 175) (Just 68)
    , Car "Honda Civic Si 2dr hatch" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19490) (Just 17849) (Just 2) (Just 4) (Just 160) (Just 26) (Just 30) (Just 2782) (Just 101) (Just 166) (Just 67)
    , Car "Honda CR-V LX" SUV All_Wheel_Drive (Just 19860) (Just 18419) (Just 2.4) (Just 4) (Just 160) (Just 21) (Just 25) (Just 3258) (Just 103) (Just 179) (Just 70)
    , Car "Honda Element LX" SUV All_Wheel_Drive (Just 18690) (Just 17334) (Just 2.4) (Just 4) (Just 160) (Just 21) (Just 24) (Just 3468) (Just 101) (Just 167) (Just 72)
    , Car "Honda Insight 2dr (gas/electric)" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19110) (Just 17911) (Just 2) (Just 3) (Just 73) (Just 60) (Just 66) (Just 1850) (Just 95) (Just 155) (Just 67)
    , Car "Honda Odyssey EX" Minivan Front_Wheel_Drive (Just 27450) (Just 24744) (Just 3.5) (Just 6) (Just 240) (Just 18) (Just 25) (Just 4365) (Just 118) (Just 201) (Just 76)
    , Car "Honda Odyssey LX" Minivan Front_Wheel_Drive (Just 24950) (Just 22498) (Just 3.5) (Just 6) (Just 240) (Just 18) (Just 25) (Just 4310) (Just 118) (Just 201) (Just 76)
    , Car "Honda Pilot LX" SUV All_Wheel_Drive (Just 27560) (Just 24843) (Just 3.5) (Just 6) (Just 240) (Just 17) (Just 22) (Just 4387) (Just 106) (Just 188) (Just 77)
    , Car "Honda S2000 convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 33260) (Just 29965) (Just 2.2) (Just 4) (Just 240) (Just 20) (Just 25) (Just 2835) (Just 95) (Just 162) (Just 69)
    , Car "Hummer H2" SUV All_Wheel_Drive (Just 49995) (Just 45815) (Just 6) (Just 8) (Just 316) (Just 10) (Just 12) (Just 6400) (Just 123) (Just 190) (Just 81)
    , Car "Hyundai Accent 2dr hatch" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 10539) (Just 10107) (Just 1.6) (Just 4) (Just 103) (Just 29) (Just 33) (Just 2255) (Just 96) (Just 167) (Just 66)
    , Car "Hyundai Accent GL 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 11839) (Just 11116) (Just 1.6) (Just 4) (Just 103) (Just 29) (Just 33) (Just 2290) (Just 96) (Just 167) (Just 66)
    , Car "Hyundai Accent GT 2dr hatch" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 11939) (Just 11209) (Just 1.6) (Just 4) (Just 103) (Just 29) (Just 33) (Just 2339) (Just 96) (Just 167) (Just 66)
    , Car "Hyundai Elantra GLS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 13839) (Just 12781) (Just 2) (Just 4) (Just 138) (Just 26) (Just 34) (Just 2635) (Just 103) (Just 178) (Just 68)
    , Car "Hyundai Elantra GT 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 15389) (Just 14207) (Just 2) (Just 4) (Just 138) (Just 26) (Just 34) (Just 2635) (Just 103) (Just 178) (Just 68)
    , Car "Hyundai Elantra GT 4dr hatch" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 15389) (Just 14207) (Just 2) (Just 4) (Just 138) (Just 26) (Just 34) (Just 2698) (Just 103) (Just 178) (Just 68)
    , Car "Hyundai Santa Fe GLS" SUV Front_Wheel_Drive (Just 21589) (Just 20201) (Just 2.7) (Just 6) (Just 173) (Just 20) (Just 26) (Just 3549) (Just 103) (Just 177) (Just 73)
    , Car "Hyundai Sonata GLS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19339) (Just 17574) (Just 2.7) (Just 6) (Just 170) (Just 19) (Just 27) (Just 3217) (Just 106) (Just 187) (Just 72)
    , Car "Hyundai Sonata LX 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 20339) (Just 18380) (Just 2.7) (Just 6) (Just 170) (Just 19) (Just 27) (Just 3217) (Just 106) (Just 187) (Just 72)
    , Car "Hyundai Tiburon GT V6 2dr" Sports_Car Front_Wheel_Drive (Just 18739) (Just 17101) (Just 2.7) (Just 6) (Just 172) (Just 19) (Just 26) (Just 3023) (Just 100) (Just 173) (Just 69)
    , Car "Hyundai XG350 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 24589) (Just 22055) (Just 3.5) (Just 6) (Just 194) (Just 17) (Just 26) (Just 3651) (Just 108) (Just 192) (Just 72)
    , Car "Hyundai XG350 L 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 26189) (Just 23486) (Just 3.5) (Just 6) (Just 194) (Just 17) (Just 26) (Just 3651) (Just 108) (Just 192) (Just 72)
    , Car "Infiniti FX35" Wagon Rear_Wheel_Drive (Just 34895) (Just 31756) (Just 3.5) (Just 6) (Just 280) (Just 16) (Just 22) (Just 4056) (Just 112) (Just 189) (Just 76)
    , Car "Infiniti FX45" Wagon All_Wheel_Drive (Just 36395) (Just 33121) (Just 4.5) (Just 8) (Just 315) (Just 15) (Just 19) (Just 4309) (Just 112) (Just 189) (Just 76)
    , Car "Infiniti G35 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 28495) (Just 26157) (Just 3.5) (Just 6) (Just 260) (Just 18) (Just 26) (Just 3336) (Just 112) (Just 187) (Just 69)
    , Car "Infiniti G35 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 32445) (Just 29783) (Just 3.5) (Just 6) (Just 260) (Just 18) (Just 26) (Just 3677) (Just 112) (Just 187) (Just 69)
    , Car "Infiniti G35 Sport Coupe 2dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 29795) (Just 27536) (Just 3.5) (Just 6) (Just 280) (Just 18) (Just 26) (Just 3416) (Just 112) (Just 182) (Just 72)
    , Car "Infiniti I35 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 31145) (Just 28320) (Just 3.5) (Just 6) (Just 255) (Just 19) (Just 26) (Just 3306) (Just 108) (Just 194) (Just 70)
    , Car "Infiniti M45 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 42845) (Just 38792) (Just 4.5) (Just 8) (Just 340) (Just 17) (Just 23) (Just 3851) (Just 110) (Just 197) (Just 70)
    , Car "Infiniti Q45 Luxury 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 52545) (Just 47575) (Just 4.5) (Just 8) (Just 340) (Just 17) (Just 23) (Just 3977) (Just 113) (Just 200) (Just 73)
    , Car "Isuzu Ascender S" SUV All_Wheel_Drive (Just 31849) (Just 29977) (Just 4.2) (Just 6) (Just 275) (Just 15) (Just 20) (Just 4967) (Just 129) (Just 208) (Just 76)
    , Car "Isuzu Rodeo S" SUV Front_Wheel_Drive (Just 20449) (Just 19261) (Just 3.2) (Just 6) (Just 193) (Just 17) (Just 21) (Just 3836) (Just 106) (Just 178) (Just 70)
    , Car "Jaguar S-Type 3.0 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 43895) (Just 40004) (Just 3) (Just 6) (Just 235) (Just 18) (Just 26) (Just 3777) (Just 115) (Just 192) (Just 72)
    , Car "Jaguar S-Type 4.2 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 49995) (Just 45556) (Just 4.2) (Just 8) (Just 294) (Just 18) (Just 28) (Just 3874) (Just 115) (Just 192) (Just 72)
    , Car "Jaguar S-Type R 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 63120) (Just 57499) (Just 4.2) (Just 8) (Just 390) (Just 17) (Just 24) (Just 4046) (Just 115) (Just 192) (Just 72)
    , Car "Jaguar Vanden Plas 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 68995) (Just 62846) (Just 4.2) (Just 8) (Just 294) (Just 18) (Just 28) (Just 3803) (Just 119) (Just 200) (Just 73)
    , Car "Jaguar XJ8 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 59995) (Just 54656) (Just 4.2) (Just 8) (Just 294) (Just 18) (Just 28) (Just 3803) (Just 119) (Just 200) (Just 73)
    , Car "Jaguar XJR 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 74995) (Just 68306) (Just 4.2) (Just 8) (Just 390) (Just 17) (Just 24) (Just 3948) (Just 119) (Just 200) (Just 73)
    , Car "Jaguar XK8 convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 74995) (Just 68306) (Just 4.2) (Just 8) (Just 294) (Just 18) (Just 26) (Just 3980) (Just 102) (Just 187) (Just 71)
    , Car "Jaguar XK8 coupe 2dr" Sports_Car Rear_Wheel_Drive (Just 69995) (Just 63756) (Just 4.2) (Just 8) (Just 294) (Just 18) (Just 26) (Just 3779) (Just 102) (Just 187) (Just 71)
    , Car "Jaguar XKR convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 86995) (Just 79226) (Just 4.2) (Just 8) (Just 390) (Just 16) (Just 23) (Just 4042) (Just 102) (Just 187) (Just 71)
    , Car "Jaguar XKR coupe 2dr" Sports_Car Rear_Wheel_Drive (Just 81995) (Just 74676) (Just 4.2) (Just 8) (Just 390) (Just 16) (Just 23) (Just 3865) (Just 102) (Just 187) (Just 71)
    , Car "Jaguar X-Type 2.5 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 29995) (Just 27355) (Just 2.5) (Just 6) (Just 192) (Just 18) (Just 26) (Just 3428) (Just 107) (Just 184) (Just 70)
    , Car "Jaguar X-Type 3.0 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 33995) (Just 30995) (Just 3) (Just 6) (Just 227) (Just 18) (Just 25) (Just 3516) (Just 107) (Just 184) (Just 70)
    , Car "Jeep Grand Cherokee Laredo" SUV Front_Wheel_Drive (Just 27905) (Just 25686) (Just 4) (Just 6) (Just 195) (Just 16) (Just 21) (Just 3790) (Just 106) (Just 181) (Just 72)
    , Car "Jeep Liberty Sport" SUV All_Wheel_Drive (Just 20130) (Just 18973) (Just 2.4) (Just 4) (Just 150) (Just 20) (Just 24) (Just 3826) (Just 104) (Just 174) (Just 72)
    , Car "Jeep Wrangler Sahara convertible 2dr" SUV All_Wheel_Drive (Just 25520) (Just 23275) (Just 4) (Just 6) (Just 190) (Just 16) (Just 19) (Just 3575) (Just 93) (Just 150) (Just 67)
    , Car "Kia Amanti 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 26000) (Just 23764) (Just 3.5) (Just 6) (Just 195) (Just 17) (Just 25) Nothing (Just 110) (Just 196) (Just 73)
    , Car "Kia Optima LX 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 16040) (Just 14910) (Just 2.4) (Just 4) (Just 138) (Just 23) (Just 30) (Just 3281) (Just 106) (Just 186) (Just 72)
    , Car "Kia Optima LX V6 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 18435) (Just 16850) (Just 2.7) (Just 6) (Just 170) (Just 20) (Just 27) (Just 3279) (Just 106) (Just 186) (Just 72)
    , Car "Kia Rio 4dr auto" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 11155) (Just 10705) (Just 1.6) (Just 4) (Just 104) (Just 25) (Just 32) (Just 2458) (Just 95) (Just 167) (Just 66)
    , Car "Kia Rio 4dr manual" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 10280) (Just 9875) (Just 1.6) (Just 4) (Just 104) (Just 26) (Just 33) (Just 2403) (Just 95) (Just 167) (Just 66)
    , Car "Kia Rio Cinco" Wagon Front_Wheel_Drive (Just 11905) (Just 11410) (Just 1.6) (Just 4) (Just 104) (Just 26) (Just 33) (Just 2447) (Just 95) (Just 167) (Just 66)
    , Car "Kia Sedona LX" Minivan Front_Wheel_Drive (Just 20615) (Just 19400) (Just 3.5) (Just 6) (Just 195) (Just 16) (Just 22) (Just 4802) (Just 115) (Just 194) (Just 75)
    , Car "Kia Sorento LX" SUV Front_Wheel_Drive (Just 19635) (Just 18630) (Just 3.5) (Just 6) (Just 192) (Just 16) (Just 19) (Just 4112) (Just 107) (Just 180) (Just 73)
    , Car "Kia Spectra 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 12360) (Just 11630) (Just 1.8) (Just 4) (Just 124) (Just 24) (Just 32) (Just 2661) (Just 101) (Just 178) (Just 68)
    , Car "Kia Spectra GS 4dr hatch" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 13580) (Just 12830) (Just 1.8) (Just 4) (Just 124) (Just 24) (Just 32) (Just 2686) (Just 101) (Just 178) (Just 68)
    , Car "Kia Spectra GSX 4dr hatch" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 14630) (Just 13790) (Just 1.8) (Just 4) (Just 124) (Just 24) (Just 32) (Just 2697) (Just 101) (Just 178) (Just 68)
    , Car "Land Rover Discovery SE" SUV All_Wheel_Drive (Just 39250) (Just 35777) (Just 4.6) (Just 8) (Just 217) (Just 12) (Just 16) (Just 4576) (Just 100) (Just 185) (Just 74)
    , Car "Land Rover Freelander SE" SUV All_Wheel_Drive (Just 25995) (Just 23969) (Just 2.5) (Just 6) (Just 174) (Just 18) (Just 21) (Just 3577) (Just 101) (Just 175) (Just 71)
    , Car "Land Rover Range Rover HSE" SUV All_Wheel_Drive (Just 72250) (Just 65807) (Just 4.4) (Just 8) (Just 282) (Just 12) (Just 16) (Just 5379) (Just 113) (Just 195) (Just 76)
    , Car "Lexus ES 330 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 32350) (Just 28755) (Just 3.3) (Just 6) (Just 225) (Just 20) (Just 29) (Just 3460) (Just 107) (Just 191) (Just 71)
    , Car "Lexus GS 300 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 41010) (Just 36196) (Just 3) (Just 6) (Just 220) (Just 18) (Just 25) (Just 3649) (Just 110) (Just 189) (Just 71)
    , Car "Lexus GS 430 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 48450) (Just 42232) (Just 4.3) (Just 8) (Just 300) (Just 18) (Just 23) (Just 3715) (Just 110) (Just 189) (Just 71)
    , Car "Lexus GX 470" SUV All_Wheel_Drive (Just 45700) (Just 39838) (Just 4.7) (Just 8) (Just 235) (Just 15) (Just 19) (Just 4740) (Just 110) (Just 188) (Just 74)
    , Car "Lexus IS 300 4dr auto" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 32415) (Just 28611) (Just 3) (Just 6) (Just 215) (Just 18) (Just 24) (Just 3285) (Just 105) (Just 177) (Just 68)
    , Car "Lexus IS 300 4dr manual" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 31045) (Just 27404) (Just 3) (Just 6) (Just 215) (Just 18) (Just 25) (Just 3255) (Just 105) (Just 177) (Just 68)
    , Car "Lexus IS 300 SportCross" Wagon Rear_Wheel_Drive (Just 32455) (Just 28647) (Just 3) (Just 6) (Just 215) (Just 18) (Just 24) (Just 3410) (Just 105) (Just 177) (Just 68)
    , Car "Lexus LS 430 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 55750) (Just 48583) (Just 4.3) (Just 8) (Just 290) (Just 18) (Just 25) (Just 3990) (Just 115) (Just 197) (Just 72)
    , Car "Lexus LX 470" SUV All_Wheel_Drive (Just 64800) (Just 56455) (Just 4.7) (Just 8) (Just 235) (Just 13) (Just 17) (Just 5590) (Just 112) (Just 193) (Just 76)
    , Car "Lexus RX 330" SUV All_Wheel_Drive (Just 39195) (Just 34576) (Just 3.3) (Just 6) (Just 230) (Just 18) (Just 24) (Just 4065) (Just 107) (Just 186) (Just 73)
    , Car "Lexus SC 430 convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 63200) (Just 55063) (Just 4.3) (Just 8) (Just 300) (Just 18) (Just 23) (Just 3840) (Just 103) (Just 178) (Just 72)
    , Car "Lincoln Aviator Ultimate" SUV Front_Wheel_Drive (Just 42915) (Just 39443) (Just 4.6) (Just 8) (Just 302) (Just 13) (Just 18) (Just 4834) (Just 114) (Just 193) (Just 76)
    , Car "Lincoln LS V6 Luxury 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 32495) (Just 29969) (Just 3) (Just 6) (Just 232) (Just 20) (Just 26) (Just 3681) (Just 115) (Just 194) (Just 73)
    , Car "Lincoln LS V6 Premium 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 36895) (Just 33929) (Just 3) (Just 6) (Just 232) (Just 20) (Just 26) (Just 3681) (Just 115) (Just 194) (Just 73)
    , Car "Lincoln LS V8 Sport 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 40095) (Just 36809) (Just 3.9) (Just 8) (Just 280) (Just 17) (Just 24) (Just 3768) (Just 115) (Just 194) (Just 73)
    , Car "Lincoln LS V8 Ultimate 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 43495) (Just 39869) (Just 3.9) (Just 8) (Just 280) (Just 17) (Just 24) (Just 3768) (Just 115) (Just 194) (Just 73)
    , Car "Lincoln Navigator Luxury" SUV All_Wheel_Drive (Just 52775) (Just 46360) (Just 5.4) (Just 8) (Just 300) (Just 13) (Just 18) (Just 5969) (Just 119) (Just 206) (Just 80)
    , Car "Lincoln Town Car Signature 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 41815) (Just 38418) (Just 4.6) (Just 8) (Just 239) (Just 17) (Just 25) (Just 4369) (Just 118) (Just 215) (Just 78)
    , Car "Lincoln Town Car Ultimate 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 44925) (Just 41217) (Just 4.6) (Just 8) (Just 239) (Just 17) (Just 25) (Just 4369) (Just 118) (Just 215) (Just 78)
    , Car "Lincoln Town Car Ultimate L 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 50470) (Just 46208) (Just 4.6) (Just 8) (Just 239) (Just 17) (Just 25) (Just 4474) (Just 124) (Just 221) (Just 78)
    , Car "Mazda B2300 SX Regular Cab" Pickup Rear_Wheel_Drive (Just 14840) (Just 14070) (Just 2.3) (Just 4) (Just 143) (Just 24) (Just 29) (Just 2960) (Just 112) Nothing Nothing
    , Car "Mazda B4000 SE Cab Plus" Pickup All_Wheel_Drive (Just 22350) (Just 20482) (Just 4) (Just 6) (Just 207) (Just 15) (Just 19) (Just 3571) (Just 126) Nothing Nothing
    , Car "Mazda MPV ES" Minivan Front_Wheel_Drive (Just 28750) (Just 26600) (Just 3) (Just 6) (Just 200) (Just 18) (Just 25) (Just 3812) (Just 112) (Just 188) (Just 72)
    , Car "Mazda MX-5 Miata convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 22388) (Just 20701) (Just 1.8) (Just 4) (Just 142) (Just 23) (Just 28) (Just 2387) (Just 89) (Just 156) (Just 66)
    , Car "Mazda MX-5 Miata LS convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 25193) (Just 23285) (Just 1.8) (Just 4) (Just 142) (Just 23) (Just 28) (Just 2387) (Just 89) (Just 156) (Just 66)
    , Car "Mazda RX-8 4dr automatic" Sports_Car Rear_Wheel_Drive (Just 25700) (Just 23794) (Just 1.3) (Just -1) (Just 197) (Just 18) (Just 25) (Just 3053) (Just 106) (Just 174) Nothing
    , Car "Mazda RX-8 4dr manual" Sports_Car Rear_Wheel_Drive (Just 27200) (Just 25179) (Just 1.3) (Just -1) (Just 238) (Just 18) (Just 24) (Just 3029) (Just 106) (Just 174) Nothing
    , Car "Mazda Tribute DX 2.0" SUV All_Wheel_Drive (Just 21087) (Just 19742) (Just 2) (Just 4) (Just 130) (Just 22) (Just 25) (Just 3091) (Just 103) (Just 173) (Just 72)
    , Car "Mazda3 i 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 15500) (Just 14525) (Just 2) (Just 4) (Just 148) Nothing Nothing (Just 2696) Nothing Nothing Nothing
    , Car "Mazda3 s 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 17200) (Just 15922) (Just 2.3) (Just 4) (Just 160) Nothing Nothing (Just 2762) Nothing Nothing Nothing
    , Car "Mazda6 i 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19270) (Just 17817) (Just 2.3) (Just 4) (Just 160) (Just 24) (Just 32) (Just 3042) (Just 105) (Just 187) (Just 70)
    , Car "Mercedes-Benz C230 Sport 2dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 26060) (Just 24249) (Just 1.8) (Just 4) (Just 189) (Just 22) (Just 30) (Just 3250) (Just 107) (Just 178) (Just 68)
    , Car "Mercedes-Benz C240" Wagon Rear_Wheel_Drive (Just 33780) (Just 31466) (Just 2.6) (Just 6) (Just 168) (Just 19) (Just 25) (Just 3470) (Just 107) (Just 179) (Just 68)
    , Car "Mercedes-Benz C240 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 32280) (Just 30071) (Just 2.6) (Just 6) (Just 168) (Just 20) (Just 25) (Just 3360) (Just 107) (Just 178) (Just 68)
    , Car "Mercedes-Benz C240 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 33480) (Just 31187) (Just 2.6) (Just 6) (Just 168) (Just 19) (Just 25) (Just 3360) (Just 107) (Just 178) (Just 68)
    , Car "Mercedes-Benz C32 AMG 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 52120) (Just 48522) (Just 3.2) (Just 6) (Just 349) (Just 16) (Just 21) (Just 3540) (Just 107) (Just 178) (Just 68)
    , Car "Mercedes-Benz C320 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 37630) (Just 35046) (Just 3.2) (Just 6) (Just 215) (Just 20) (Just 26) (Just 3450) (Just 107) (Just 178) (Just 68)
    , Car "Mercedes-Benz C320 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 38830) (Just 36162) (Just 3.2) (Just 6) (Just 215) (Just 19) (Just 27) Nothing (Just 107) (Just 178) (Just 68)
    , Car "Mercedes-Benz C320 Sport 2dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 28370) (Just 26435) (Just 3.2) (Just 6) (Just 215) (Just 19) (Just 26) (Just 3430) (Just 107) (Just 178) (Just 68)
    , Car "Mercedes-Benz C320 Sport 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 35920) (Just 33456) (Just 3.2) (Just 6) (Just 215) (Just 19) (Just 26) (Just 3430) (Just 107) (Just 178) (Just 68)
    , Car "Mercedes-Benz CL500 2dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 94820) (Just 88324) (Just 5) (Just 8) (Just 302) (Just 16) (Just 24) (Just 4085) (Just 114) (Just 196) (Just 73)
    , Car "Mercedes-Benz CL600 2dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 128420) (Just 119600) (Just 5.5) (Just 12) (Just 493) (Just 13) (Just 19) (Just 4473) (Just 114) (Just 196) (Just 73)
    , Car "Mercedes-Benz CLK320 coupe 2dr (convertible)" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 45707) (Just 41966) (Just 3.2) (Just 6) (Just 215) (Just 20) (Just 26) (Just 3770) (Just 107) (Just 183) (Just 69)
    , Car "Mercedes-Benz CLK500 coupe 2dr (convertible)" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 52800) (Just 49104) (Just 5) (Just 8) (Just 302) (Just 17) (Just 22) (Just 3585) (Just 107) (Just 183) (Just 69)
    , Car "Mercedes-Benz E320" Wagon Rear_Wheel_Drive (Just 50670) (Just 47174) (Just 3.2) (Just 6) (Just 221) (Just 19) (Just 27) (Just 3966) (Just 112) (Just 190) (Just 71)
    , Car "Mercedes-Benz E320 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 48170) (Just 44849) (Just 3.2) (Just 6) (Just 221) (Just 19) (Just 27) (Just 3635) (Just 112) (Just 190) (Just 71)
    , Car "Mercedes-Benz E500" Wagon All_Wheel_Drive (Just 60670) (Just 56474) (Just 5) (Just 8) (Just 302) (Just 16) (Just 24) (Just 4230) (Just 112) (Just 190) (Just 71)
    , Car "Mercedes-Benz E500 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 57270) (Just 53382) (Just 5) (Just 8) (Just 302) (Just 16) (Just 20) (Just 3815) (Just 112) (Just 190) (Just 71)
    , Car "Mercedes-Benz G500" SUV All_Wheel_Drive (Just 76870) (Just 71540) (Just 5) (Just 8) (Just 292) (Just 13) (Just 14) (Just 5423) (Just 112) (Just 186) (Just 71)
    , Car "Mercedes-Benz ML500" SUV All_Wheel_Drive (Just 46470) (Just 43268) (Just 5) (Just 8) (Just 288) (Just 14) (Just 17) (Just 4874) (Just 111) (Just 183) (Just 72)
    , Car "Mercedes-Benz S430 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 74320) (Just 69168) (Just 4.3) (Just 8) (Just 275) (Just 18) (Just 26) (Just 4160) (Just 122) (Just 203) (Just 73)
    , Car "Mercedes-Benz S500 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 86970) (Just 80939) (Just 5) (Just 8) (Just 302) (Just 16) (Just 24) (Just 4390) (Just 122) (Just 203) (Just 73)
    , Car "Mercedes-Benz SL500 convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 90520) (Just 84325) (Just 5) (Just 8) (Just 302) (Just 16) (Just 23) (Just 4065) (Just 101) (Just 179) (Just 72)
    , Car "Mercedes-Benz SL55 AMG 2dr" Sports_Car Rear_Wheel_Drive (Just 121770) (Just 113388) (Just 5.5) (Just 8) (Just 493) (Just 14) (Just 21) (Just 4235) (Just 101) (Just 179) (Just 72)
    , Car "Mercedes-Benz SL600 convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 126670) (Just 117854) (Just 5.5) (Just 12) (Just 493) (Just 13) (Just 19) (Just 4429) (Just 101) (Just 179) (Just 72)
    , Car "Mercedes-Benz SLK230 convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 40320) (Just 37548) (Just 2.3) (Just 4) (Just 192) (Just 21) (Just 29) (Just 3055) (Just 95) (Just 158) (Just 68)
    , Car "Mercedes-Benz SLK32 AMG 2dr" Sports_Car Rear_Wheel_Drive (Just 56170) (Just 52289) (Just 3.2) (Just 6) (Just 349) (Just 17) (Just 22) (Just 3220) (Just 95) (Just 158) (Just 68)
    , Car "Mercury Grand Marquis GS 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 24695) (Just 23217) (Just 4.6) (Just 8) (Just 224) (Just 17) (Just 25) (Just 4052) (Just 115) (Just 212) (Just 78)
    , Car "Mercury Grand Marquis LS Premium 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 29595) (Just 27148) (Just 4.6) (Just 8) (Just 224) (Just 17) (Just 25) (Just 4052) (Just 115) (Just 212) (Just 78)
    , Car "Mercury Grand Marquis LS Ultimate 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 30895) (Just 28318) (Just 4.6) (Just 8) (Just 224) (Just 17) (Just 25) (Just 4052) (Just 115) (Just 212) (Just 78)
    , Car "Mercury Marauder 4dr" Small_Sporty_Compact_Large_Sedan Rear_Wheel_Drive (Just 34495) (Just 31558) (Just 4.6) (Just 8) (Just 302) (Just 17) (Just 23) (Just 4195) (Just 115) (Just 212) (Just 78)
    , Car "Mercury Monterey Luxury" Minivan Front_Wheel_Drive (Just 33995) (Just 30846) (Just 4.2) (Just 6) (Just 201) (Just 16) (Just 23) (Just 4340) (Just 121) (Just 202) (Just 77)
    , Car "Mercury Mountaineer" SUV Front_Wheel_Drive (Just 29995) (Just 27317) (Just 4) (Just 6) (Just 210) (Just 16) (Just 21) (Just 4374) (Just 114) (Just 190) (Just 72)
    , Car "Mercury Sable GS" Wagon Front_Wheel_Drive (Just 22595) (Just 20748) (Just 3) (Just 6) (Just 155) (Just 19) (Just 26) (Just 3488) (Just 109) (Just 198) (Just 73)
    , Car "Mercury Sable GS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 21595) (Just 19848) (Just 3) (Just 6) (Just 155) (Just 20) (Just 27) (Just 3308) (Just 109) (Just 200) (Just 73)
    , Car "Mercury Sable LS Premium 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 23895) (Just 21918) (Just 3) (Just 6) (Just 201) (Just 19) (Just 26) (Just 3315) (Just 109) (Just 200) (Just 73)
    , Car "Mini Cooper" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 16999) (Just 15437) (Just 1.6) (Just 4) (Just 115) (Just 28) (Just 37) (Just 2524) (Just 97) (Just 143) (Just 67)
    , Car "Mini Cooper S" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19999) (Just 18137) (Just 1.6) (Just 4) (Just 163) (Just 25) (Just 34) (Just 2678) (Just 97) (Just 144) (Just 67)
    , Car "Mitsubishi Diamante LS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 29282) (Just 27250) (Just 3.5) (Just 6) (Just 205) (Just 18) (Just 25) (Just 3549) (Just 107) (Just 194) (Just 70)
    , Car "Mitsubishi Eclipse GTS 2dr" Sports_Car Front_Wheel_Drive (Just 25092) (Just 23456) (Just 3) (Just 6) (Just 210) (Just 21) (Just 28) (Just 3241) (Just 101) (Just 177) (Just 69)
    , Car "Mitsubishi Eclipse Spyder GT convertible 2dr" Sports_Car Front_Wheel_Drive (Just 26992) (Just 25218) (Just 3) (Just 6) (Just 210) (Just 21) (Just 28) (Just 3296) (Just 101) (Just 177) (Just 69)
    , Car "Mitsubishi Endeavor XLS" SUV All_Wheel_Drive (Just 30492) (Just 28330) (Just 3.8) (Just 6) (Just 215) (Just 17) (Just 21) (Just 4134) (Just 109) (Just 190) (Just 74)
    , Car "Mitsubishi Galant ES 2.4L 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19312) (Just 17957) (Just 2.4) (Just 4) (Just 160) Nothing Nothing (Just 3351) (Just 108) (Just 191) (Just 72)
    , Car "Mitsubishi Galant GTS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 25700) (Just 23883) (Just 3.8) (Just 6) (Just 230) (Just 18) (Just 26) (Just 3649) (Just 108) (Just 191) (Just 72)
    , Car "Mitsubishi Lancer ES 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 14622) (Just 13751) (Just 2) (Just 4) (Just 120) Nothing Nothing (Just 2656) (Just 102) (Just 181) (Just 67)
    , Car "Mitsubishi Lancer Evolution 4dr" Sports_Car Front_Wheel_Drive (Just 29562) (Just 27466) (Just 2) (Just 4) (Just 271) (Just 18) (Just 26) (Just 3263) (Just 103) (Just 179) (Just 70)
    , Car "Mitsubishi Lancer LS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 16722) (Just 15718) (Just 2) (Just 4) (Just 120) Nothing Nothing (Just 2795) (Just 102) (Just 181) (Just 67)
    , Car "Mitsubishi Lancer OZ Rally 4dr auto" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 17232) (Just 16196) (Just 2) (Just 4) (Just 120) Nothing Nothing (Just 2744) (Just 102) (Just 181) (Just 67)
    , Car "Mitsubishi Lancer Sportback LS" Wagon Front_Wheel_Drive (Just 17495) (Just 16295) (Just 2.4) (Just 4) (Just 160) Nothing Nothing (Just 3020) (Just 102) (Just 181) (Just 67)
    , Car "Mitsubishi Montero XLS" SUV All_Wheel_Drive (Just 33112) (Just 30763) (Just 3.8) (Just 6) (Just 215) (Just 15) (Just 19) (Just 4718) (Just 110) (Just 190) (Just 75)
    , Car "Mitsubishi Outlander LS" SUV Front_Wheel_Drive (Just 18892) (Just 17569) (Just 2.4) (Just 4) (Just 160) (Just 21) (Just 27) (Just 3240) (Just 103) (Just 179) (Just 69)
    , Car "Nissan 350Z coupe 2dr" Sports_Car Rear_Wheel_Drive (Just 26910) (Just 25203) (Just 3.5) (Just 6) (Just 287) (Just 20) (Just 26) (Just 3188) (Just 104) (Just 169) (Just 72)
    , Car "Nissan 350Z Enthusiast convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 34390) (Just 31845) (Just 3.5) (Just 6) (Just 287) (Just 20) (Just 26) (Just 3428) (Just 104) (Just 169) (Just 72)
    , Car "Nissan Altima S 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19240) (Just 18030) (Just 2.5) (Just 4) (Just 175) (Just 21) (Just 26) (Just 3039) (Just 110) (Just 192) (Just 70)
    , Car "Nissan Altima SE 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 23290) (Just 21580) (Just 3.5) (Just 6) (Just 245) (Just 21) (Just 26) (Just 3197) (Just 110) (Just 192) (Just 70)
    , Car "Nissan Frontier King Cab XE V6" Pickup All_Wheel_Drive (Just 19479) (Just 18253) (Just 3.3) (Just 6) (Just 180) (Just 17) (Just 20) (Just 3932) (Just 116) Nothing Nothing
    , Car "Nissan Maxima SE 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 27490) (Just 25182) (Just 3.5) (Just 6) (Just 265) (Just 20) (Just 28) (Just 3473) (Just 111) (Just 194) (Just 72)
    , Car "Nissan Maxima SL 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 29440) (Just 26966) (Just 3.5) (Just 6) (Just 265) (Just 20) (Just 28) (Just 3476) (Just 111) (Just 194) (Just 72)
    , Car "Nissan Murano SL" Wagon Rear_Wheel_Drive (Just 28739) (Just 27300) (Just 3.5) (Just 6) (Just 245) (Just 20) (Just 25) (Just 3801) (Just 111) (Just 188) (Just 74)
    , Car "Nissan Pathfinder Armada SE" SUV Front_Wheel_Drive (Just 33840) (Just 30815) (Just 5.6) (Just 8) (Just 305) (Just 13) (Just 19) (Just 5013) (Just 123) (Just 207) (Just 79)
    , Car "Nissan Pathfinder SE" SUV Front_Wheel_Drive (Just 27339) (Just 25972) (Just 3.5) (Just 6) (Just 240) (Just 16) (Just 21) (Just 3871) (Just 106) (Just 183) (Just 72)
    , Car "Nissan Quest S" Minivan Front_Wheel_Drive (Just 24780) (Just 22958) (Just 3.5) (Just 6) (Just 240) (Just 19) (Just 26) (Just 4012) (Just 124) (Just 204) (Just 78)
    , Car "Nissan Quest SE" Minivan Front_Wheel_Drive (Just 32780) (Just 30019) (Just 3.5) (Just 6) (Just 240) (Just 18) (Just 25) (Just 4175) (Just 124) (Just 204) (Just 78)
    , Car "Nissan Sentra 1.8 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 12740) (Just 12205) (Just 1.8) (Just 4) (Just 126) (Just 28) (Just 35) (Just 2513) (Just 100) (Just 178) (Just 67)
    , Car "Nissan Sentra 1.8 S 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 14740) (Just 13747) (Just 1.8) (Just 4) (Just 126) (Just 28) (Just 35) (Just 2581) (Just 100) (Just 178) (Just 67)
    , Car "Nissan Sentra SE-R 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 17640) (Just 16444) (Just 2.5) (Just 4) (Just 165) (Just 23) (Just 28) (Just 2761) (Just 100) (Just 178) (Just 67)
    , Car "Nissan Titan King Cab XE" Pickup All_Wheel_Drive (Just 26650) (Just 24926) (Just 5.6) (Just 8) (Just 305) (Just 14) (Just 18) (Just 5287) (Just 140) Nothing Nothing
    , Car "Nissan Xterra XE V6" SUV Front_Wheel_Drive (Just 20939) (Just 19512) (Just 3.3) (Just 6) (Just 180) (Just 17) (Just 20) (Just 3760) (Just 104) (Just 178) (Just 70)
    , Car "Oldsmobile Alero GLS 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 23675) (Just 21485) (Just 3.4) (Just 6) (Just 170) (Just 20) (Just 29) (Just 3085) (Just 107) (Just 187) (Just 70)
    , Car "Oldsmobile Alero GX 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 18825) (Just 17642) (Just 2.2) (Just 4) (Just 140) (Just 24) (Just 32) (Just 2946) (Just 107) (Just 187) (Just 70)
    , Car "Oldsmobile Silhouette GL" Minivan Front_Wheel_Drive (Just 28790) (Just 26120) (Just 3.4) (Just 6) (Just 185) (Just 19) (Just 26) (Just 3948) (Just 120) (Just 201) (Just 72)
    , Car "Pontiac Aztekt" SUV Front_Wheel_Drive (Just 21595) (Just 19810) (Just 3.4) (Just 6) (Just 185) (Just 19) (Just 26) (Just 3779) (Just 108) (Just 182) (Just 74)
    , Car "Pontiac Bonneville GXP 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 35995) (Just 32997) (Just 4.6) (Just 8) (Just 275) Nothing Nothing (Just 3790) (Just 112) (Just 203) (Just 74)
    , Car "Pontiac Grand Am GT 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 22450) (Just 20595) (Just 3.4) (Just 6) (Just 175) (Just 20) (Just 29) (Just 3118) (Just 107) (Just 186) (Just 70)
    , Car "Pontiac Grand Prix GT1 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 22395) (Just 20545) (Just 3.8) (Just 6) (Just 200) (Just 20) (Just 30) (Just 3477) (Just 111) (Just 198) (Just 74)
    , Car "Pontiac Grand Prix GT2 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 24295) (Just 22284) (Just 3.8) (Just 6) (Just 200) (Just 20) (Just 30) (Just 3484) (Just 111) (Just 198) (Just 74)
    , Car "Pontiac GTO 2dr" Sports_Car Rear_Wheel_Drive (Just 33500) (Just 30710) (Just 5.7) (Just 8) (Just 340) Nothing Nothing (Just 3725) (Just 110) (Just 190) (Just 73)
    , Car "Pontiac Montana" Minivan Front_Wheel_Drive (Just 23845) (Just 21644) (Just 3.4) (Just 6) (Just 185) (Just 19) (Just 26) (Just 3803) (Just 112) (Just 187) (Just 72)
    , Car "Pontiac Montana EWB" Minivan All_Wheel_Drive (Just 31370) (Just 28454) (Just 3.4) (Just 6) (Just 185) (Just 18) (Just 24) (Just 4431) (Just 121) (Just 201) (Just 72)
    , Car "Pontiac Sunfire 1SA 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 15495) (Just 14375) (Just 2.2) (Just 4) (Just 140) (Just 24) (Just 33) (Just 2771) (Just 104) (Just 182) (Just 68)
    , Car "Pontiac Sunfire 1SC 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 17735) (Just 16369) (Just 2.2) (Just 4) (Just 140) (Just 24) (Just 33) (Just 2771) (Just 104) (Just 182) (Just 68)
    , Car "Pontiac Vibe" Wagon Rear_Wheel_Drive (Just 17045) (Just 15973) (Just 1.8) (Just 4) (Just 130) (Just 29) (Just 36) (Just 2701) (Just 102) (Just 172) (Just 70)
    , Car "Porsche 911 Carrera 4S coupe 2dr (convert)" Sports_Car All_Wheel_Drive (Just 84165) (Just 72206) (Just 3.6) (Just 6) (Just 315) (Just 17) (Just 24) (Just 3240) (Just 93) (Just 175) (Just 72)
    , Car "Porsche 911 Carrera convertible 2dr (coupe)" Sports_Car Rear_Wheel_Drive (Just 79165) (Just 69229) (Just 3.6) (Just 6) (Just 315) (Just 18) (Just 26) (Just 3135) (Just 93) (Just 175) (Just 70)
    , Car "Porsche 911 GT2 2dr" Sports_Car Rear_Wheel_Drive (Just 192465) (Just 173560) (Just 3.6) (Just 6) (Just 477) (Just 17) (Just 24) (Just 3131) (Just 93) (Just 175) (Just 72)
    , Car "Porsche 911 Targa coupe 2dr" Sports_Car Rear_Wheel_Drive (Just 76765) (Just 67128) (Just 3.6) (Just 6) (Just 315) (Just 18) (Just 26) (Just 3119) (Just 93) (Just 175) (Just 70)
    , Car "Porsche Boxster convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 43365) (Just 37886) (Just 2.7) (Just 6) (Just 228) (Just 20) (Just 29) (Just 2811) (Just 95) (Just 170) (Just 70)
    , Car "Porsche Boxster S convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 52365) (Just 45766) (Just 3.2) (Just 6) (Just 258) (Just 18) (Just 26) (Just 2911) (Just 95) (Just 170) (Just 70)
    , Car "Porsche Cayenne S" SUV All_Wheel_Drive (Just 56665) (Just 49865) (Just 4.5) (Just 8) (Just 340) (Just 14) (Just 18) (Just 4950) (Just 112) (Just 188) (Just 76)
    , Car "Saab 9-3 Aero 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 33360) (Just 31562) (Just 2) (Just 4) (Just 210) (Just 20) (Just 28) (Just 3175) (Just 105) (Just 183) (Just 69)
    , Car "Saab 9-3 Aero convertible 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 43175) (Just 40883) (Just 2) (Just 4) (Just 210) (Just 21) (Just 30) (Just 3700) (Just 105) (Just 182) (Just 69)
    , Car "Saab 9-3 Arc convertible 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 40670) (Just 38520) (Just 2) (Just 4) (Just 210) (Just 21) (Just 29) (Just 3480) (Just 105) (Just 182) (Just 69)
    , Car "Saab 9-3 Arc Sport 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 30860) (Just 29269) (Just 2) (Just 4) (Just 210) (Just 20) (Just 28) (Just 3175) (Just 105) (Just 183) (Just 69)
    , Car "Saab 9-5 Aero" Wagon Front_Wheel_Drive (Just 40845) (Just 38376) (Just 2.3) (Just 4) (Just 250) (Just 19) (Just 29) (Just 3620) (Just 106) (Just 190) (Just 71)
    , Car "Saab 9-5 Aero 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 39465) (Just 37721) (Just 2.3) (Just 4) (Just 250) (Just 21) (Just 29) (Just 3470) (Just 106) (Just 190) (Just 71)
    , Car "Saab 9-5 Arc 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 35105) (Just 33011) (Just 2.3) (Just 4) (Just 220) (Just 21) (Just 29) (Just 3470) (Just 106) (Just 190) (Just 71)
    , Car "Saturn Ion1 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 10995) (Just 10319) (Just 2.2) (Just 4) (Just 140) (Just 26) (Just 35) (Just 2692) (Just 103) (Just 185) (Just 67)
    , Car "Saturn L300 2" Wagon Front_Wheel_Drive (Just 23560) (Just 21779) (Just 2.2) (Just 4) (Just 140) (Just 24) (Just 34) (Just 3109) (Just 107) (Just 190) (Just 69)
    , Car "Saturn L300-2 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 21410) (Just 19801) (Just 3) (Just 6) (Just 182) (Just 20) (Just 28) (Just 3197) (Just 107) (Just 190) (Just 69)
    , Car "Saturn lon2 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 14300) (Just 13393) (Just 2.2) (Just 4) (Just 140) (Just 26) (Just 35) (Just 2692) (Just 103) (Just 185) (Just 67)
    , Car "Saturn lon2 quad coupe 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 14850) (Just 13904) (Just 2.2) (Just 4) (Just 140) (Just 26) (Just 35) (Just 2751) (Just 103) (Just 185) (Just 68)
    , Car "Saturn lon3 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 15825) (Just 14811) (Just 2.2) (Just 4) (Just 140) (Just 26) (Just 35) (Just 2692) (Just 103) (Just 185) (Just 67)
    , Car "Saturn lon3 quad coupe 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 16350) (Just 15299) (Just 2.2) (Just 4) (Just 140) (Just 26) (Just 35) (Just 2751) (Just 103) (Just 185) (Just 68)
    , Car "Saturn VUE" SUV All_Wheel_Drive (Just 20585) (Just 19238) (Just 2.2) (Just 4) (Just 143) (Just 21) (Just 26) (Just 3381) (Just 107) (Just 181) (Just 72)
    , Car "Scion xA 4dr hatch" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 12965) (Just 12340) (Just 1.5) (Just 4) (Just 108) (Just 32) (Just 38) (Just 2340) (Just 93) (Just 154) (Just 67)
    , Car "Scion xB" Wagon Front_Wheel_Drive (Just 14165) (Just 13480) (Just 1.5) (Just 4) (Just 108) (Just 31) (Just 35) (Just 2425) (Just 98) (Just 155) (Just 67)
    , Car "Subaru Baja" Pickup All_Wheel_Drive (Just 24520) (Just 22304) (Just 2.5) (Just 4) (Just 165) (Just 21) (Just 28) (Just 3485) (Just 104) Nothing Nothing
    , Car "Subaru Forester X" Wagon All_Wheel_Drive (Just 21445) (Just 19646) (Just 2.5) (Just 4) (Just 165) (Just 21) (Just 28) (Just 3090) (Just 99) (Just 175) (Just 68)
    , Car "Subaru Impreza 2.5 RS 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 19945) (Just 18399) (Just 2.5) (Just 4) (Just 165) (Just 22) (Just 28) (Just 2965) (Just 99) (Just 174) (Just 69)
    , Car "Subaru Impreza WRX 4dr" Sports_Car All_Wheel_Drive (Just 25045) (Just 23022) (Just 2) (Just 4) (Just 227) (Just 20) (Just 27) (Just 3085) (Just 99) (Just 174) (Just 69)
    , Car "Subaru Impreza WRX STi 4dr" Sports_Car All_Wheel_Drive (Just 31545) (Just 29130) (Just 2.5) (Just 4) (Just 300) (Just 18) (Just 24) (Just 3263) (Just 100) (Just 174) (Just 69)
    , Car "Subaru Legacy GT 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 25645) (Just 23336) (Just 2.5) (Just 4) (Just 165) (Just 21) (Just 28) (Just 3395) (Just 104) (Just 184) (Just 69)
    , Car "Subaru Legacy L 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 20445) (Just 18713) (Just 2.5) (Just 4) (Just 165) (Just 21) (Just 28) (Just 3285) (Just 104) (Just 184) (Just 69)
    , Car "Subaru Outback" Wagon All_Wheel_Drive (Just 23895) (Just 21773) (Just 2.5) (Just 4) (Just 165) (Just 21) (Just 28) (Just 3430) (Just 104) (Just 187) (Just 69)
    , Car "Subaru Outback H6 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 29345) (Just 26660) (Just 3) (Just 6) (Just 212) (Just 19) (Just 26) (Just 3610) (Just 104) (Just 184) (Just 69)
    , Car "Subaru Outback H-6 VDC 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 31545) (Just 28603) (Just 3) (Just 6) (Just 212) (Just 19) (Just 26) (Just 3630) (Just 104) (Just 184) (Just 69)
    , Car "Subaru Outback Limited Sedan 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 27145) (Just 24687) (Just 2.5) (Just 4) (Just 165) (Just 20) (Just 27) (Just 3495) (Just 104) (Just 184) (Just 69)
    , Car "Suzuki Aeno S 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 12884) (Just 12719) (Just 2.3) (Just 4) (Just 155) (Just 25) (Just 31) (Just 2676) (Just 98) (Just 171) (Just 68)
    , Car "Suzuki Aerio LX 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 14500) (Just 14317) (Just 2.3) (Just 4) (Just 155) (Just 25) (Just 31) (Just 2676) (Just 98) (Just 171) (Just 68)
    , Car "Suzuki Aerio SX" Wagon All_Wheel_Drive (Just 16497) (Just 16291) (Just 2.3) (Just 4) (Just 155) (Just 24) (Just 29) (Just 2932) (Just 98) (Just 167) (Just 68)
    , Car "Suzuki Forenza EX 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 15568) (Just 15378) (Just 2) (Just 4) (Just 119) (Just 22) (Just 30) (Just 2756) (Just 102) (Just 177) (Just 68)
    , Car "Suzuki Forenza S 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 12269) (Just 12116) (Just 2) (Just 4) (Just 119) (Just 24) (Just 31) (Just 2701) (Just 102) (Just 177) (Just 68)
    , Car "Suzuki Verona LX 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 17262) (Just 17053) (Just 2.5) (Just 6) (Just 155) (Just 20) (Just 27) (Just 3380) (Just 106) (Just 188) (Just 72)
    , Car "Suzuki Vitara LX" SUV All_Wheel_Drive (Just 17163) (Just 16949) (Just 2.5) (Just 6) (Just 165) (Just 19) (Just 22) (Just 3020) (Just 98) (Just 163) (Just 67)
    , Car "Suzuki XL-7 EX" SUV Front_Wheel_Drive (Just 23699) (Just 22307) (Just 2.7) (Just 6) (Just 185) (Just 18) (Just 22) (Just 3682) (Just 110) (Just 187) (Just 70)
    , Car "Toyota 4Runner SR5 V6" SUV Front_Wheel_Drive (Just 27710) (Just 24801) (Just 4) (Just 6) (Just 245) (Just 18) (Just 21) (Just 4035) (Just 110) (Just 189) (Just 74)
    , Car "Toyota Avalon XL 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 26560) (Just 23693) (Just 3) (Just 6) (Just 210) (Just 21) (Just 29) (Just 3417) (Just 107) (Just 192) (Just 72)
    , Car "Toyota Avalon XLS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 30920) (Just 27271) (Just 3) (Just 6) (Just 210) (Just 21) (Just 29) (Just 3439) (Just 107) (Just 192) (Just 72)
    , Car "Toyota Camry LE 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19560) (Just 17558) (Just 2.4) (Just 4) (Just 157) (Just 24) (Just 33) (Just 3086) (Just 107) (Just 189) (Just 71)
    , Car "Toyota Camry LE V6 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 22775) (Just 20325) (Just 3) (Just 6) (Just 210) (Just 21) (Just 29) (Just 3296) (Just 107) (Just 189) (Just 71)
    , Car "Toyota Camry Solara SE 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19635) (Just 17722) (Just 2.4) (Just 4) (Just 157) (Just 24) (Just 33) (Just 3175) (Just 107) (Just 193) (Just 72)
    , Car "Toyota Camry Solara SE V6 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 21965) (Just 19819) (Just 3.3) (Just 6) (Just 225) (Just 20) (Just 29) (Just 3417) (Just 107) (Just 193) (Just 72)
    , Car "Toyota Camry Solara SLE V6 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 26510) (Just 23908) (Just 3.3) (Just 6) (Just 225) (Just 20) (Just 29) (Just 3439) (Just 107) (Just 193) (Just 72)
    , Car "Toyota Camry XLE V6 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 25920) (Just 23125) (Just 3) (Just 6) (Just 210) (Just 21) (Just 29) (Just 3362) (Just 107) (Just 189) (Just 71)
    , Car "Toyota Celica GT-S 2dr" Sports_Car Front_Wheel_Drive (Just 22570) (Just 20363) (Just 1.8) (Just 4) (Just 180) (Just 24) (Just 33) (Just 2500) (Just 102) (Just 171) (Just 68)
    , Car "Toyota Corolla CE 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 14085) (Just 13065) (Just 1.8) (Just 4) (Just 130) (Just 32) (Just 40) (Just 2502) (Just 102) (Just 178) (Just 67)
    , Car "Toyota Corolla LE 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 15295) (Just 13889) (Just 1.8) (Just 4) (Just 130) (Just 32) (Just 40) (Just 2524) (Just 102) (Just 178) (Just 67)
    , Car "Toyota Corolla S 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 15030) (Just 13650) (Just 1.8) (Just 4) (Just 130) (Just 32) (Just 40) (Just 2524) (Just 102) (Just 178) (Just 67)
    , Car "Toyota Echo 2dr auto" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 11560) (Just 10896) (Just 1.5) (Just 4) (Just 108) (Just 33) (Just 39) (Just 2085) (Just 93) (Just 163) (Just 65)
    , Car "Toyota Echo 2dr manual" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 10760) (Just 10144) (Just 1.5) (Just 4) (Just 108) (Just 35) (Just 43) (Just 2035) (Just 93) (Just 163) (Just 65)
    , Car "Toyota Echo 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 11290) (Just 10642) (Just 1.5) (Just 4) (Just 108) (Just 35) (Just 43) (Just 2055) (Just 93) (Just 163) (Just 65)
    , Car "Toyota Highlander V6" SUV All_Wheel_Drive (Just 27930) (Just 24915) (Just 3.3) (Just 6) (Just 230) (Just 18) (Just 24) (Just 3935) (Just 107) (Just 185) (Just 72)
    , Car "Toyota Land Cruiser" SUV All_Wheel_Drive (Just 54765) (Just 47986) (Just 4.7) (Just 8) (Just 325) (Just 13) (Just 17) (Just 5390) (Just 112) (Just 193) (Just 76)
    , Car "Toyota Matrix XR" Wagon Front_Wheel_Drive (Just 16695) (Just 15156) (Just 1.8) (Just 4) (Just 130) (Just 29) (Just 36) (Just 2679) (Just 102) (Just 171) (Just 70)
    , Car "Toyota MR2 Spyder convertible 2dr" Sports_Car Rear_Wheel_Drive (Just 25130) (Just 22787) (Just 1.8) (Just 4) (Just 138) (Just 26) (Just 32) (Just 2195) (Just 97) (Just 153) (Just 67)
    , Car "Toyota Prius 4dr (gas/electric)" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 20510) (Just 18926) (Just 1.5) (Just 4) (Just 110) (Just 59) (Just 51) (Just 2890) (Just 106) (Just 175) (Just 68)
    , Car "Toyota RAV4" SUV All_Wheel_Drive (Just 20290) (Just 18553) (Just 2.4) (Just 4) (Just 161) (Just 22) (Just 27) (Just 3119) (Just 98) (Just 167) (Just 68)
    , Car "Toyota Sequoia SR5" SUV All_Wheel_Drive (Just 35695) (Just 31827) (Just 4.7) (Just 8) (Just 240) (Just 14) (Just 17) (Just 5270) (Just 118) (Just 204) (Just 78)
    , Car "Toyota Sienna CE" Minivan Front_Wheel_Drive (Just 23495) (Just 21198) (Just 3.3) (Just 6) (Just 230) (Just 19) (Just 27) (Just 4120) (Just 119) (Just 200) (Just 77)
    , Car "Toyota Sienna XLE Limited" Minivan Front_Wheel_Drive (Just 28800) (Just 25690) (Just 3.3) (Just 6) (Just 230) (Just 19) (Just 27) (Just 4165) (Just 119) (Just 200) (Just 77)
    , Car "Toyota Tacoma" Pickup Rear_Wheel_Drive (Just 12800) (Just 11879) (Just 2.4) (Just 4) (Just 142) (Just 22) (Just 27) (Just 2750) (Just 103) Nothing Nothing
    , Car "Toyota Tundra Access Cab V6 SR5" Pickup All_Wheel_Drive (Just 25935) (Just 23520) (Just 3.4) (Just 6) (Just 190) (Just 14) (Just 17) (Just 4435) (Just 128) Nothing Nothing
    , Car "Toyota Tundra Regular Cab V6" Pickup Rear_Wheel_Drive (Just 16495) (Just 14978) (Just 3.4) (Just 6) (Just 190) (Just 16) (Just 20) (Just 3925) (Just 128) Nothing Nothing
    , Car "Volkswagen Golf GLS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 18715) (Just 17478) (Just 2) (Just 4) (Just 115) (Just 24) (Just 31) (Just 2897) (Just 99) (Just 165) (Just 68)
    , Car "Volkswagen GTI 1.8T 2dr hatch" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 19825) (Just 18109) (Just 1.8) (Just 4) (Just 180) (Just 24) (Just 31) (Just 2934) (Just 99) (Just 168) (Just 68)
    , Car "Volkswagen Jetta GL" Wagon Front_Wheel_Drive (Just 19005) (Just 17427) (Just 2) (Just 4) (Just 115) (Just 24) (Just 30) (Just 3034) (Just 99) (Just 174) (Just 68)
    , Car "Volkswagen Jetta GLI VR6 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 23785) (Just 21686) (Just 2.8) (Just 6) (Just 200) (Just 21) (Just 30) (Just 3179) (Just 99) (Just 172) (Just 68)
    , Car "Volkswagen Jetta GLS TDI 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 21055) (Just 19638) (Just 1.9) (Just 4) (Just 100) (Just 38) (Just 46) (Just 3003) (Just 99) (Just 172) (Just 68)
    , Car "Volkswagen New Beetle GLS 1.8T 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 21055) (Just 19638) (Just 1.8) (Just 4) (Just 150) (Just 24) (Just 31) (Just 2820) (Just 99) (Just 161) (Just 68)
    , Car "Volkswagen New Beetle GLS convertible 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 23215) (Just 21689) (Just 2) (Just 4) (Just 115) (Just 24) (Just 30) (Just 3082) (Just 99) (Just 161) (Just 68)
    , Car "Volkswagen Passat GLS 1.8T" Wagon Front_Wheel_Drive (Just 24955) (Just 22801) (Just 1.8) (Just 4) (Just 170) (Just 22) (Just 31) (Just 3338) (Just 106) (Just 184) (Just 69)
    , Car "Volkswagen Passat GLS 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 23955) (Just 21898) (Just 1.8) (Just 4) (Just 170) (Just 22) (Just 31) (Just 3241) (Just 106) (Just 185) (Just 69)
    , Car "Volkswagen Passat GLX V6 4MOTION 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 33180) (Just 30583) (Just 2.8) (Just 6) (Just 190) (Just 19) (Just 26) (Just 3721) (Just 106) (Just 185) (Just 69)
    , Car "Volkswagen Passat W8" Wagon Front_Wheel_Drive (Just 40235) (Just 36956) (Just 4) (Just 8) (Just 270) (Just 18) (Just 25) (Just 4067) (Just 106) (Just 184) (Just 69)
    , Car "Volkswagen Passat W8 4MOTION 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 39235) (Just 36052) (Just 4) (Just 8) (Just 270) (Just 18) (Just 25) (Just 3953) (Just 106) (Just 185) (Just 69)
    , Car "Volkswagen Phaeton 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 65000) (Just 59912) (Just 4.2) (Just 8) (Just 335) Nothing Nothing (Just 5194) (Just 118) (Just 204) (Just 75)
    , Car "Volkswagen Phaeton W12 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 75000) (Just 69130) (Just 6) (Just 12) (Just 420) Nothing Nothing (Just 5399) (Just 118) (Just 204) (Just 75)
    , Car "Volkswagen Touareg V6" SUV All_Wheel_Drive (Just 35515) (Just 32243) (Just 3.2) (Just 6) (Just 220) (Just 15) (Just 20) (Just 5086) (Just 112) (Just 187) (Just 76)
    , Car "Volvo C70 HPT convertible 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 42565) (Just 40083) (Just 2.3) (Just 5) (Just 242) (Just 20) (Just 26) (Just 3450) (Just 105) (Just 186) (Just 72)
    , Car "Volvo C70 LPT convertible 2dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 40565) (Just 38203) (Just 2.4) (Just 5) (Just 197) (Just 21) (Just 28) (Just 3450) (Just 105) (Just 186) (Just 72)
    , Car "Volvo S40 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 25135) (Just 23701) (Just 1.9) (Just 4) (Just 170) (Just 22) (Just 29) (Just 2767) (Just 101) (Just 178) (Just 68)
    , Car "Volvo S60 2.5 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 31745) (Just 29916) (Just 2.5) (Just 5) (Just 208) (Just 20) (Just 27) (Just 3903) (Just 107) (Just 180) (Just 71)
    , Car "Volvo S60 R 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 37560) (Just 35382) (Just 2.5) (Just 5) (Just 300) (Just 18) (Just 25) (Just 3571) (Just 107) (Just 181) (Just 71)
    , Car "Volvo S60 T5 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 34845) (Just 32902) (Just 2.3) (Just 5) (Just 247) (Just 20) (Just 28) (Just 3766) (Just 107) (Just 180) (Just 71)
    , Car "Volvo S80 2.5T 4dr" Small_Sporty_Compact_Large_Sedan All_Wheel_Drive (Just 37885) (Just 35688) (Just 2.5) (Just 5) (Just 194) (Just 20) (Just 27) (Just 3691) (Just 110) (Just 190) (Just 72)
    , Car "Volvo S80 2.9 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 37730) (Just 35542) (Just 2.9) (Just 6) (Just 208) (Just 20) (Just 28) (Just 3576) (Just 110) (Just 190) (Just 72)
    , Car "Volvo S80 T6 4dr" Small_Sporty_Compact_Large_Sedan Front_Wheel_Drive (Just 45210) (Just 42573) (Just 2.9) (Just 6) (Just 268) (Just 19) (Just 26) (Just 3653) (Just 110) (Just 190) (Just 72)
    , Car "Volvo V40" Wagon Front_Wheel_Drive (Just 26135) (Just 24641) (Just 1.9) (Just 4) (Just 170) (Just 22) (Just 29) (Just 2822) (Just 101) (Just 180) (Just 68)
    , Car "Volvo XC70" Wagon All_Wheel_Drive (Just 35145) (Just 33112) (Just 2.5) (Just 5) (Just 208) (Just 20) (Just 27) (Just 3823) (Just 109) (Just 186) (Just 73)
    , Car "Volvo XC90 T6" SUV All_Wheel_Drive (Just 41250) (Just 38851) (Just 2.9) (Just 6) (Just 268) (Just 15) (Just 20) (Just 4638) (Just 113) (Just 189) (Just 75)
    ]