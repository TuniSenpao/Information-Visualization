-- module Exercise7.Exercise3 exposing (..)

module Main exposing (main)

import Browser
import Csv
import Csv.Decode
import Date exposing (Date, Interval(..), fromCalendarDate)
import Dict exposing (Dict)
import Html exposing (Html, br, div, h1, li, pre, strong, text, ol)
import Http
import String exposing (join)
import Time exposing (Month(..))


type alias StockName =
    String


type StockData
    = Failure
    | Loading
    | Success (List ( String, Maybe Float ))


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
