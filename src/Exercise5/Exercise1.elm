module Exercise5.Exercise1 exposing (..)

-- For Ellie (https://ellie-app.com)
-- module Main exposing (main)

import Axis
import Color
import Html exposing (Html, a, datalist, li, text, ul)
import Path exposing (Path)
import Scale exposing (ContinuousScale)
import Shape
import Statistics
import TypedSvg exposing (circle, g, line, path, rect, style, svg, text_)
import TypedSvg.Attributes exposing (class, d, fill, fontFamily, stroke, textAnchor, transform, viewBox)
import TypedSvg.Attributes.InPx exposing (cx, cy, fontSize, height, r, strokeWidth, width, x, x1, x2, y, y1, y2)
import TypedSvg.Core exposing (Svg)
import TypedSvg.Types exposing (AnchorAlignment(..), Length(..), Paint(..), Transform(..))


mapConsecutive : (a -> a -> b) -> List a -> Maybe (List b)
mapConsecutive f l =
    Maybe.map (\l2 -> List.map2 f l l2) <| List.tail l


computeAspectRatio : List Point -> Maybe Float
computeAspectRatio data =
    let
        s : Maybe (List Float)
        s =
            mapConsecutive (\a b -> abs ((b.y - a.y) / (b.x - a.x))) data

        -- Berechnung für Median
        m_sm : Maybe Float
        m_sm =
            Maybe.andThen
                (\justS -> Statistics.quantile 0.5 (List.sort justS))
                s

        -- Berechnung für Range von x, d.h. min und max
        rx : Maybe ( Float, Float )
        rx =
            Statistics.extent (List.map (\point -> point.x) data)

        -- Berechnung für Range von y, d.h. min und max
        ry : Maybe ( Float, Float )
        ry =
            Statistics.extent (List.map (\point -> point.y) data)
    in
    Maybe.map3
        (\sm ( xmin, xmax ) ( ymin, ymax ) ->
            -- Code zu Berechnung des Aspect Ratios
            sm * (xmax - xmin) / (ymax - ymin)
        )
        m_sm
        rx
        ry


w : Float
w =
    900


padding : Float
padding =
    60


lineplot : Float -> Float -> XyData -> Svg msg
lineplot width aspectRatio model =
    let
        height =
            width / aspectRatio

        xTicks =
            10

        xScale : ContinuousScale Float
        xScale =
            model.data
                |> List.map .x
                |> Statistics.extent
                |> Maybe.withDefault ( 1700, 1991 )
                |> Scale.linear ( 0, width )
                |> Scale.nice xTicks

        yTicks =
            max 2 (round (10 / aspectRatio))

        yScale : ContinuousScale Float
        yScale =
            model.data
                |> List.map .y
                |> List.maximum
                |> Maybe.withDefault 0
                |> (\b -> ( 0, b ))
                |> Scale.linear ( height, 0 )
                |> Scale.nice yTicks

        lineGenerator : ( Float, Float ) -> Maybe ( Float, Float )
        lineGenerator ( x, y ) =
            Just ( Scale.convert xScale x, Scale.convert yScale y )

        line : Path
        line =
            List.map (\p -> ( p.x, p.y )) model.data
                |> List.map lineGenerator
                |> Shape.line Shape.monotoneInXCurve
    in
    svg
        [ viewBox 0 0 (width + 2 * padding) (height + 2 * padding)
        , TypedSvg.Attributes.width <| TypedSvg.Types.Percent 50
        , TypedSvg.Attributes.height <| TypedSvg.Types.Percent 50
        , TypedSvg.Attributes.preserveAspectRatio
            (TypedSvg.Types.Align TypedSvg.Types.ScaleMin TypedSvg.Types.ScaleMin)
            TypedSvg.Types.Slice
        ]
        [ g [ transform [ Translate (padding - 1) (height + padding) ] ]
            [ Axis.bottom [ Axis.tickCount xTicks ] xScale ]
        , g [ transform [ Translate (padding - 1) padding ] ]
            [ Axis.left [ Axis.tickCount yTicks ] yScale
            , text_ [ fontFamily [ "sans-serif" ], fontSize 10, x 5, y 5 ] [ text model.yDescription ]
            ]
        , g [ transform [ Translate padding padding ], class [ "series" ] ]
            [ Path.element line
                [ stroke <| Paint <| Color.black
                , strokeWidth 1
                , fill PaintNone
                ]
            ]
        ]


main : Html msg
main =
    let
        points : List Point
        points =
            List.map (\( x, y ) -> Point "" (toFloat x) y) sunspots

        data =
            XyData "Year" "Sun spots" points

        aspectRatio : Maybe Float
        aspectRatio =
            computeAspectRatio points
    in
    Html.div []
        [ Html.h1 [] [ Html.text "Sun spots" ]
        , Html.div
            []
            [ Html.h2 []
                [ text
                    (Maybe.withDefault
                        "Aspect-Ratio nicht berechnet."
                        (Maybe.map
                            (\ar -> "Aspect Ratio: " ++ String.fromFloat ar)
                            aspectRatio
                        )
                    )
                ]
            , lineplot
                w
                (Maybe.withDefault 1 aspectRatio)
                data
            ]
        , Html.div
            []
            [ Html.h2 [] [ text "Aspect Ratio: 2.0" ]
            , lineplot w 2 data
            ]
        , Html.div
            []
            [ Html.h2 [] [ text "Aspect Ratio: 1.0" ]
            , lineplot w 1 data
            ]
        ]


type alias Point =
    { pointName : String, x : Float, y : Float }


type alias XyData =
    { xDescription : String
    , yDescription : String
    , data : List Point
    }


sunspots : List ( Int, Float )
sunspots =
    [ ( 1700, 5 )
    , ( 1701, 11 )
    , ( 1702, 16 )
    , ( 1703, 23 )
    , ( 1704, 36 )
    , ( 1705, 58 )
    , ( 1706, 29 )
    , ( 1707, 20 )
    , ( 1708, 10 )
    , ( 1709, 8 )
    , ( 1710, 3 )
    , ( 1711, 0 )
    , ( 1712, 0 )
    , ( 1713, 2 )
    , ( 1714, 11 )
    , ( 1715, 27 )
    , ( 1716, 47 )
    , ( 1717, 63 )
    , ( 1718, 60 )
    , ( 1719, 39 )
    , ( 1720, 28 )
    , ( 1721, 26 )
    , ( 1722, 22 )
    , ( 1723, 11 )
    , ( 1724, 21 )
    , ( 1725, 40 )
    , ( 1726, 78 )
    , ( 1727, 122 )
    , ( 1728, 103 )
    , ( 1729, 73 )
    , ( 1730, 47 )
    , ( 1731, 35 )
    , ( 1732, 11 )
    , ( 1733, 5 )
    , ( 1734, 16 )
    , ( 1735, 34 )
    , ( 1736, 70 )
    , ( 1737, 81 )
    , ( 1738, 111 )
    , ( 1739, 101 )
    , ( 1740, 73 )
    , ( 1741, 40 )
    , ( 1742, 20 )
    , ( 1743, 16 )
    , ( 1744, 5 )
    , ( 1745, 11 )
    , ( 1746, 22 )
    , ( 1747, 40 )
    , ( 1748, 60 )
    , ( 1749, 80.9 )
    , ( 1750, 83.4 )
    , ( 1751, 47.7 )
    , ( 1752, 47.8 )
    , ( 1753, 30.7 )
    , ( 1754, 12.2 )
    , ( 1755, 9.6 )
    , ( 1756, 10.2 )
    , ( 1757, 32.4 )
    , ( 1758, 47.6 )
    , ( 1759, 54 )
    , ( 1760, 62.9 )
    , ( 1761, 85.9 )
    , ( 1762, 61.2 )
    , ( 1763, 45.1 )
    , ( 1764, 36.4 )
    , ( 1765, 20.9 )
    , ( 1766, 11.4 )
    , ( 1767, 37.8 )
    , ( 1768, 69.8 )
    , ( 1769, 106.1 )
    , ( 1770, 100.8 )
    , ( 1771, 81.6 )
    , ( 1772, 66.5 )
    , ( 1773, 34.8 )
    , ( 1774, 30.6 )
    , ( 1775, 7 )
    , ( 1776, 19.8 )
    , ( 1777, 92.5 )
    , ( 1778, 154.4 )
    , ( 1779, 125.9 )
    , ( 1780, 84.8 )
    , ( 1781, 68.1 )
    , ( 1782, 38.5 )
    , ( 1783, 22.8 )
    , ( 1784, 10.2 )
    , ( 1785, 24.1 )
    , ( 1786, 82.9 )
    , ( 1787, 132 )
    , ( 1788, 130.9 )
    , ( 1789, 118.1 )
    , ( 1790, 89.9 )
    , ( 1791, 66.6 )
    , ( 1792, 60 )
    , ( 1793, 46.9 )
    , ( 1794, 41 )
    , ( 1795, 21.3 )
    , ( 1796, 16 )
    , ( 1797, 6.4 )
    , ( 1798, 4.1 )
    , ( 1799, 6.8 )
    , ( 1800, 14.5 )
    , ( 1801, 34 )
    , ( 1802, 45 )
    , ( 1803, 43.1 )
    , ( 1804, 47.5 )
    , ( 1805, 42.2 )
    , ( 1806, 28.1 )
    , ( 1807, 10.1 )
    , ( 1808, 8.1 )
    , ( 1809, 2.5 )
    , ( 1810, 0 )
    , ( 1811, 1.4 )
    , ( 1812, 5 )
    , ( 1813, 12.2 )
    , ( 1814, 13.9 )
    , ( 1815, 35.4 )
    , ( 1816, 45.8 )
    , ( 1817, 41.1 )
    , ( 1818, 30.1 )
    , ( 1819, 23.9 )
    , ( 1820, 15.6 )
    , ( 1821, 6.6 )
    , ( 1822, 4 )
    , ( 1823, 1.8 )
    , ( 1824, 8.5 )
    , ( 1825, 16.6 )
    , ( 1826, 36.3 )
    , ( 1827, 49.6 )
    , ( 1828, 64.2 )
    , ( 1829, 67 )
    , ( 1830, 70.9 )
    , ( 1831, 47.8 )
    , ( 1832, 27.5 )
    , ( 1833, 8.5 )
    , ( 1834, 13.2 )
    , ( 1835, 56.9 )
    , ( 1836, 121.5 )
    , ( 1837, 138.3 )
    , ( 1838, 103.2 )
    , ( 1839, 85.7 )
    , ( 1840, 64.6 )
    , ( 1841, 36.7 )
    , ( 1842, 24.2 )
    , ( 1843, 10.7 )
    , ( 1844, 15 )
    , ( 1845, 40.1 )
    , ( 1846, 61.5 )
    , ( 1847, 98.5 )
    , ( 1848, 124.7 )
    , ( 1849, 96.3 )
    , ( 1850, 66.6 )
    , ( 1851, 64.5 )
    , ( 1852, 54.1 )
    , ( 1853, 39 )
    , ( 1854, 20.6 )
    , ( 1855, 6.7 )
    , ( 1856, 4.3 )
    , ( 1857, 22.7 )
    , ( 1858, 54.8 )
    , ( 1859, 93.8 )
    , ( 1860, 95.8 )
    , ( 1861, 77.2 )
    , ( 1862, 59.1 )
    , ( 1863, 44 )
    , ( 1864, 47 )
    , ( 1865, 30.5 )
    , ( 1866, 16.3 )
    , ( 1867, 7.3 )
    , ( 1868, 37.6 )
    , ( 1869, 74 )
    , ( 1870, 139 )
    , ( 1871, 111.2 )
    , ( 1872, 101.6 )
    , ( 1873, 66.2 )
    , ( 1874, 44.7 )
    , ( 1875, 17 )
    , ( 1876, 11.3 )
    , ( 1877, 12.4 )
    , ( 1878, 3.4 )
    , ( 1879, 6 )
    , ( 1880, 32.3 )
    , ( 1881, 54.3 )
    , ( 1882, 59.7 )
    , ( 1883, 63.7 )
    , ( 1884, 63.5 )
    , ( 1885, 52.2 )
    , ( 1886, 25.4 )
    , ( 1887, 13.1 )
    , ( 1888, 6.8 )
    , ( 1889, 6.3 )
    , ( 1890, 7.1 )
    , ( 1891, 35.6 )
    , ( 1892, 73 )
    , ( 1893, 85.1 )
    , ( 1894, 78 )
    , ( 1895, 64 )
    , ( 1896, 41.8 )
    , ( 1897, 26.2 )
    , ( 1898, 26.7 )
    , ( 1899, 12.1 )
    , ( 1900, 9.5 )
    , ( 1901, 2.7 )
    , ( 1902, 5 )
    , ( 1903, 24.4 )
    , ( 1904, 42 )
    , ( 1905, 63.5 )
    , ( 1906, 53.8 )
    , ( 1907, 62 )
    , ( 1908, 48.5 )
    , ( 1909, 43.9 )
    , ( 1910, 18.6 )
    , ( 1911, 5.7 )
    , ( 1912, 3.6 )
    , ( 1913, 1.4 )
    , ( 1914, 9.6 )
    , ( 1915, 47.4 )
    , ( 1916, 57.1 )
    , ( 1917, 103.9 )
    , ( 1918, 80.6 )
    , ( 1919, 63.6 )
    , ( 1920, 37.6 )
    , ( 1921, 26.1 )
    , ( 1922, 14.2 )
    , ( 1923, 5.8 )
    , ( 1924, 16.7 )
    , ( 1925, 44.3 )
    , ( 1926, 63.9 )
    , ( 1927, 69 )
    , ( 1928, 77.8 )
    , ( 1929, 64.9 )
    , ( 1930, 35.7 )
    , ( 1931, 21.2 )
    , ( 1932, 11.1 )
    , ( 1933, 5.7 )
    , ( 1934, 8.7 )
    , ( 1935, 36.1 )
    , ( 1936, 79.7 )
    , ( 1937, 114.4 )
    , ( 1938, 109.6 )
    , ( 1939, 88.8 )
    , ( 1940, 67.8 )
    , ( 1941, 47.5 )
    , ( 1942, 30.6 )
    , ( 1943, 16.3 )
    , ( 1944, 9.6 )
    , ( 1945, 33.2 )
    , ( 1946, 92.6 )
    , ( 1947, 151.6 )
    , ( 1948, 136.3 )
    , ( 1949, 134.7 )
    , ( 1950, 83.9 )
    , ( 1951, 69.4 )
    , ( 1952, 31.5 )
    , ( 1953, 13.9 )
    , ( 1954, 4.4 )
    , ( 1955, 38 )
    , ( 1956, 141.7 )
    , ( 1957, 190.2 )
    , ( 1958, 184.8 )
    , ( 1959, 159 )
    , ( 1960, 112.3 )
    , ( 1961, 53.9 )
    , ( 1962, 37.5 )
    , ( 1963, 27.9 )
    , ( 1964, 10.2 )
    , ( 1965, 15.1 )
    , ( 1966, 47 )
    , ( 1967, 93.8 )
    , ( 1968, 105.9 )
    , ( 1969, 105.5 )
    , ( 1970, 104.5 )
    , ( 1971, 66.6 )
    , ( 1972, 68.9 )
    , ( 1973, 38 )
    , ( 1974, 34.5 )
    , ( 1975, 15.5 )
    , ( 1976, 12.6 )
    , ( 1977, 27.5 )
    , ( 1978, 92.5 )
    , ( 1979, 155.4 )
    , ( 1980, 154.7 )
    , ( 1981, 140.5 )
    , ( 1982, 115.9 )
    , ( 1983, 66.6 )
    , ( 1984, 45.9 )
    , ( 1985, 17.9 )
    , ( 1986, 13.4 )
    , ( 1987, 29.2 )
    , ( 1988, 100.2 )
    ]
