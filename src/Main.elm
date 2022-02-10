module Main exposing (main)

import Browser
import Html exposing (Html, div, span, text)
import Html.Attributes exposing (class, style)
import Round
import String exposing (fromFloat)
import Svg exposing (Svg, animate, animateTransform, circle, g, line, rect, svg)
import Svg.Attributes exposing (additive, attributeName, attributeType, cx, cy, dur, fill, from, height, radius, repeatCount, rx, ry, stroke, strokeWidth, to, transform, type_, values, viewBox, width, x, x1, x2, y, y1, y2)
import Task
import Time


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    }


type Hand
    = Hour
    | Minute
    | Second


init : ( Model, Cmd Msg )
init =
    ( initialModel, Task.perform AdjustTimeZone Time.here )


initialModel : Model
initialModel =
    { zone = Time.utc
    , time = Time.millisToPosix 0
    }


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }, Cmd.none )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions _ =
    Time.every 10 Tick


viewTimeAnalog : Time.Zone -> Time.Posix -> Html msg
viewTimeAnalog zone time =
    svg
        [ width "120"
        , height "120"
        , viewBox "0 0 120 120"
        ]
        (viewClock zone time)


viewClock : Time.Zone -> Time.Posix -> List (Svg msg)
viewClock zone time =
    [ rect
        [ width "100%"
        , height "100%"
        , fill "transparent"
        , stroke "black"
        , strokeWidth "1"
        ]
        []
    , viewHand Hour zone time
    , viewHand Minute zone time
    , viewHand Second zone time
    , viewGrads
    ]


viewGrads : Svg msg
viewGrads =
    let
        deg hr =
            hr
                |> toFloat
                |> (\x -> x / 12 * 360)
                |> round
                |> String.fromInt

        tick hr =
            rect
                [ width "2"
                , height "10"
                , x "59"
                , y "5"
                , rx "1"
                , ry "1"
                , fill "white"
                , transform ("rotate(" ++ deg hr ++ ",60,60)")
                ]
                []
    in
    g
        []
        (List.map
            tick
            (List.range 0 11)
        )


viewHand : Hand -> Time.Zone -> Time.Posix -> Svg msg
viewHand hand zone time =
    let
        convert : Float -> Float
        convert val =
            case hand of
                Hour ->
                    val / 12 * 360

                Minute ->
                    val / 60 * 360

                Second ->
                    val
                        / 1000
                        / 60
                        * 360

        deg =
            (case hand of
                Hour ->
                    Time.toHour

                Minute ->
                    Time.toMinute

                Second ->
                    \z t ->
                        (1000 * Time.toSecond z t)
                            + Time.toMillis z t
            )
                zone
                time
                |> toFloat
                |> convert
                |> (\x -> Round.round 2 x)

        strokeColor : String
        strokeColor =
            case hand of
                Hour ->
                    "black"

                Minute ->
                    "black"

                Second ->
                    "red"

        handLength : String
        handLength =
            case hand of
                Hour ->
                    "12%"

                Minute ->
                    "8%"

                Second ->
                    "2%"

        handWidth : String
        handWidth =
            case hand of
                Hour ->
                    "5"

                Minute ->
                    "3"

                Second ->
                    "1"

        handDrift : Svg msg
        handDrift =
            case hand of
                Second ->
                    animateTransform
                        [ attributeName "transform"
                        , attributeType "XML"
                        , type_ "rotate"
                        , from "0 60 60"
                        , to "0 60 60"
                        , dur "1s"
                        , repeatCount "indefinite"
                        , additive "sum"
                        ]
                        []

                _ ->
                    animateTransform [] []

        --1/60*360
    in
    line
        [ x1 "50%"
        , x2 "50%"
        , y1 "50%"
        , y2 handLength
        , transform ("rotate(" ++ deg ++ ",60,60)")
        , stroke strokeColor
        , strokeWidth handWidth
        ]
        [ handDrift ]


viewTimeDigital : Time.Zone -> Time.Posix -> Html msg
viewTimeDigital zone time =
    let
        toString f =
            f zone time
                |> String.fromInt
                |> String.padLeft 2 '0'

        hour =
            toString Time.toHour

        minute =
            toString Time.toMinute

        second =
            toString Time.toSecond

        millis =
            toString Time.toMillis
    in
    span [ class "digital-time" ] [ text (hour ++ ":" ++ minute ++ ":" ++ second ++ "." ++ millis) ]


view : Model -> Html Msg
view model =
    div [ class "container" ]
        [ viewTimeAnalog model.zone model.time
        , viewTimeDigital model.zone model.time
        ]


main : Program () Model Msg
main =
    Browser.element
        { init = \_ -> init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
