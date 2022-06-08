-- module Exercise7.Exercise4 exposing (..)

module Main exposing (main)

import Browser
import Color
import Csv
import Csv.Decode
import Date exposing (Date, Interval(..), fromCalendarDate)
import Dict exposing (Dict)
import Html exposing (Html, br, div, h1, h3, li, p, pre, strong, table, text, ol)
import Http
import List.Extra
import Maybe.Extra
import Scale
import Scale.Color
import Statistics
import String exposing (join)
import Time exposing (Month(..))
import TypedSvg exposing (g, rect, svg, text_)
import TypedSvg.Attributes
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..))


type alias StockName =
    String


type StockData
    = Failure
    | Loading
    | Success (List ( String, Maybe Float ))

type alias CurrentRecordedData msg =
    RecordedData ( String, Maybe Float ) (List (TypedSvg.Core.Attribute msg))

type Level
    = Level Int Int

type RecordedData value result
    = RecordedData PixelPositon value result
    
type PixelPositon
    = PixelPositon Int Int
    

{-| helper type for the recursive loop call
    xOuterLoop -> x value of the outer loop
    yOuterLoop -> y value of the outer loop
    outerResult -> intermediate results
-}
type alias LoopRec =
    { xOuterLoop : Int, yOuterLoop : Int, outerResult : List PixelPositon }

w = 
    120
    
h = 
    120
    
level =
    [ Level 5 1
    , Level 1 12
    , Level 4 1
    , Level 2 3
    , Level 3 3
    , Level 1 1 
    ]

{-| constructor for the start value of the pixel positions
    affects the position within the representation 

    startPosition == (PixelPositon 0 0)
-}
startPosition : PixelPositon
startPosition =
    PixelPositon 0 0
    
pixelList : List PixelPositon
pixelList =
    createPixelMap startPosition
        (augementLevel level)

{-| creates a list of pixel positions that correspond to the left-right arrangement

    createPixelMap (PixelPosition 0 0) [ ( Level 2 2, Level 1 1 ), ( Level 1 1, Level 0 0 ) ] == [ PixelPositon 0 0, PixelPositon 1 0, PixelPositon 0 1, PixelPositon 1 1 ]
-}
createPixelMap : PixelPositon -> List ( Level, Level ) -> List PixelPositon
createPixelMap (PixelPositon x y) levels =
    case levels of
        [] ->
            [ PixelPositon x y ]

        ( Level width height, Level nextW nextH ) :: reducedLevel ->
            let
                temp =
                    List.range 1 height
                        |> List.foldl
                            (\_ { xOuterLoop, yOuterLoop, outerResult } ->
                                let
                                    innerResult =
                                        List.range 1 width
                                            |> List.foldl
                                                (\_ ( xLoop, result ) ->
                                                    ( xLoop + nextW
                                                    , List.append
                                                        result
                                                        (createPixelMap (PixelPositon xLoop yOuterLoop) reducedLevel)
                                                    )
                                                )
                                                ( xOuterLoop, [] )
                                in
                                LoopRec
                                    xOuterLoop
                                    (yOuterLoop + nextH)
                                    (List.append outerResult (Tuple.second innerResult))
                            )
                            (LoopRec x y [])
            in
            temp.outerResult
            
ourData : List (CurrentRecordedData msg)
ourData =
    List.map2 (\a b -> RecordedData a b []) pixelList firstData

{-| helper function to scale the heigth of a pixel

    scaleWidth 10 (Level 1 1) == Scale.linear ( 0.0, 10.0 ) ( 0.0, 1.0 )
-}
scaleWidth : Int -> List Level -> Scale.ContinuousScale Float
scaleWidth width levels =
    let
        maxWidth =
            levels
                |> List.map getW
                |> List.product
    in
    Scale.linear ( 0.0, toFloat width ) ( 0.0, toFloat maxWidth )
    
{-| helper function to scale the heigth of a pixel

    scaleHeight 10 (Level 1 1) == Scale.linear ( 0.0, 10.0 ) ( 0.0, 1.0 )
-}
scaleHeight : Int -> List Level -> Scale.ContinuousScale Float
scaleHeight height levels =
    let
        maxHeight =
            levels
                |> List.map getH
                |> List.product
    in
    Scale.linear ( 0.0, toFloat height ) ( 0.0, toFloat maxHeight )

{-| get the height of a specific level

    getH (Level 1 0) == 0
-}
getH : Level -> Int
getH (Level width height) =
    height
    

{-| get the width of a specific level
    
    getW (Level 1 0) == 1
-}
getW : Level -> Int
getW (Level width height) =
    width
    
{-| helper function to define the position and size of a pixel

    drawTuplePosition (10, 10) [ Level 1 1 ] (PixelPositon 0 0) == [ TypedSvg.Attributes.x (px 0), TypedSvg.Attributes.y (px 0), TypedSvg.Attributes.width (px 10), TypedSvg.Attributes.height (px 10)]
-}
drawTuplePosition : ( Int, Int ) -> List Level -> PixelPositon -> List (TypedSvg.Core.Attribute msg)
drawTuplePosition ( width, height ) levels (PixelPositon x y) =
    [ TypedSvg.Attributes.x <| TypedSvg.Types.Px <| Scale.convert (scaleWidth width levels) <| toFloat x
    , TypedSvg.Attributes.y <| TypedSvg.Types.Px <| Scale.convert (scaleHeight height levels) <| toFloat y
    , TypedSvg.Attributes.width <| TypedSvg.Types.Px <| Scale.convert (scaleWidth width levels) 1
    , TypedSvg.Attributes.height <| TypedSvg.Types.Px <| Scale.convert (scaleHeight height levels) 1
    ]
    
drawPosition : CurrentRecordedData msg -> CurrentRecordedData msg
drawPosition (RecordedData pixelPosition value _) =
    RecordedData
        pixelPosition
        value
        (drawTuplePosition ( w, h ) level pixelPosition)

currentData : List Float
currentData =
    let
        mfList : List (Maybe Float)
        mfList =
            ourData
                |> List.map (\(RecordedData _ ( _, mf ) _) -> mf)

        result =
            mfList |> List.filterMap identity
    in
        result

{-| helper function to normalize and scale data
    specific for integer values

    normalizeFloat [10.0] == Scale.linear ( 0.0, 10.0 ) ( 0.0, 1.0 )
-}
normalizeFloat : List Float -> Scale.ContinuousScale Float
normalizeFloat data =
    data
        |> Statistics.extent
        |> Maybe.withDefault ( 0, 1 )
        |> Scale.linear ( 0, 1 )

createStyle : Maybe Float -> List (TypedSvg.Core.Attribute msg)
createStyle value =
    [ TypedSvg.Attributes.title
            (Maybe.withDefault "N.A." (Maybe.map String.fromFloat value))
        , TypedSvg.Attributes.fill <|
            TypedSvg.Types.Paint <|
                Maybe.withDefault Color.darkGray <|
                    Maybe.map
                        (Scale.Color.tealBluesInterpolator
                            << Scale.convert
                                (normalizeFloat currentData)
                        )
                        value
        ]

drawStyle : CurrentRecordedData msg -> CurrentRecordedData msg
drawStyle (RecordedData pixelPosition ( dateString, value ) attributeList) =
    RecordedData
        pixelPosition
        ( dateString, value )
        (List.append attributeList (createStyle value))

draw_neu : CurrentRecordedData msg -> TypedSvg.Core.Svg msg
draw_neu (RecordedData _ _ attributeList) =
    TypedSvg.rect
    attributeList
    []
    

stockFileUrl : String -> String
stockFileUrl file =
    "https://cors-anywhere.herokuapp.com/"
        ++ "https://users.informatik.uni-halle.de/~hinnebur/Lehre/InfoVis/U06/"
        ++ file


stockUrls : Dict String String
stockUrls =
    Dict.fromList
        [ ( "Dow Jones", stockFileUrl "DJ.csv" )
        , ( "Nikkei", stockFileUrl "NIKKEI.csv" )
        , ( "Hangseng", stockFileUrl "HANGSENG.csv" )
        , ( "Dax", stockFileUrl "DAX.csv" )
        , ( "Bovespa", stockFileUrl "BOVESPA.csv" )
        ]


type alias Model =
    { stocks : Dict StockName StockData
    }


init : () -> ( Model, Cmd Msg )
init _ =
    let
        data : Dict StockName StockData
        data =
            Dict.map
                (\_ _ ->
                    Loading
                )
                stockUrls

        cmds : Dict StockName (Cmd Msg)
        cmds =
            Dict.map
                (\name url ->
                    Http.get
                        { url = url
                        , expect = Http.expectString (GotStockData name)
                        }
                )
                stockUrls
    in
    ( { stocks = data }
    , Cmd.batch (Dict.values cmds)
    )


type Msg
    = GotStockData StockName (Result Http.Error String)


csvString_to_data : String -> List ( String, Maybe Float )
csvString_to_data csvRaw =
    Csv.parse csvRaw
        |> Csv.Decode.decodeCsv decodeStockDay
        |> Result.toMaybe
        |> Maybe.withDefault []


decodeStockDay : Csv.Decode.Decoder (( String, Maybe Float ) -> a) a
decodeStockDay =
    Csv.Decode.map (\a b -> ( a, Just b ))
        (Csv.Decode.field "Date" Ok
            |> Csv.Decode.andMap
                (Csv.Decode.field "Open"
                    (String.toFloat >> Result.fromMaybe "error parsing string")
                )
        )


fillEmptyDates : List ( String, Maybe Float ) -> List ( String, Maybe Float )
fillEmptyDates rows =
    let
        dict =
            Dict.fromList rows

        allDates =
            Date.range
                Day
                1
                (fromCalendarDate 1980 Dec 23)
                (fromCalendarDate 2011 Jun 9)

        allDatesDict =
            Dict.fromList
                (List.map
                    (\date -> ( Date.toIsoString date, Nothing ))
                    allDates
                )

        mergedDict =
            Dict.union dict allDatesDict
    in
    Dict.toList mergedDict

{-| creates the augment level -> takes the last level and multiply them on the current level
    
    augmentLevel [(Level 2 2)] == [ ( Level 2 2, Level 1 1 ), ( Level 1 1, Level 0 0 ) ]
-}
augementLevel : List Level -> List ( Level, Level )
augementLevel levels =
    let
        usableLevel =
            levels ++ [ Level 1 1 ]

        next : List Level
        next =
            List.foldr
                -- change to foldl
                (\(Level currentLevelW currentLevelH) nextLevel ->
                    let
                        (Level one two) =
                            case nextLevel of
                                [] ->
                                    Level 1 1

                                (Level x y) :: _ ->
                                    Level x y
                    in
                    Level (currentLevelW * one) (currentLevelH * two) :: nextLevel
                )
                []
                usableLevel
    in
    List.map2 Tuple.pair usableLevel (List.append (List.drop 1 next) [ Level 0 0 ])

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotStockData name result ->
            let
                stock =
                    case result of
                        Ok fullText ->
                            Success (fillEmptyDates (csvString_to_data fullText))

                        Err _ ->
                            Failure
            in
            ( { model
                | stocks =
                    Dict.insert
                        name
                        stock
                        model.stocks
              }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none


view : Model -> Html Msg
view model =
    div []
        [ ol []
            (List.map
                (\( name, data ) ->
                    let
                        dataText =
                            case data of
                                Failure ->
                                    text "Unable to load stock data."

                                Loading ->
                                    text "Loading..."

                                Success rows ->
                                    let
                                        lineString : ( String, Maybe Float ) -> String
                                        lineString ( date, open ) =
                                            date
                                                ++ ": "
                                                ++ Maybe.withDefault
                                                    "???"
                                                    (Maybe.map String.fromFloat open)

                                        lines : List String
                                        lines =
                                            List.map
                                                lineString
                                                (List.take 10 rows)
                                    in
                                    pre []
                                        [ text
                                            (join "\n" lines)
                                        ]
                    in
                    li
                        []
                        [ strong [] [ text (name ++ ": ") ]
                        , br [] []
                        , dataText
                        ]
                )
                (Dict.toList model.stocks)
            )
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }
