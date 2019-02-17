module Main exposing (Model, Msg(..), init, main, subscriptions, update, view)

import Browser
import Html as H exposing (..)
import Html.Events as HE exposing (..)
import Svg as S exposing (..)
import Svg.Attributes as SA exposing (..)
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { zone : Time.Zone
    , time : Time.Posix
    , move : Bool
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Time.utc (Time.millisToPosix 0) True
    , Task.perform AdjustTimeZone Time.here
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | AdjustTimeZone Time.Zone
    | StartClock
    | StopClock


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | time = newTime }
            , Cmd.none
            )

        AdjustTimeZone newZone ->
            ( { model | zone = newZone }
            , Cmd.none
            )

        StartClock ->
            ( { model | move = True }
            , Cmd.none
            )

        StopClock ->
            ( { model | move = False }
            , Cmd.none
            )



-- SUBSCRIPTION


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.move of
        True ->
            Time.every 1000 Tick

        False ->
            Sub.none



-- VIEW


type alias HourStr =
    String


type alias MinuteStr =
    String


type alias SecondStr =
    String


type alias HourAngle =
    String


type alias MinuteAngle =
    String


type alias SecondAngle =
    String


type alias CurrentTime =
    { hour : HourStr
    , minute : MinuteStr
    , second : SecondStr
    }


view : Model -> Html Msg
view model =
    let
        currentTime =
            getCurrentTime model
    in
    div []
        [ digitalClock currentTime
        , analogClock currentTime
        , startAndStopButton model
        ]


getCurrentTime : Model -> CurrentTime
getCurrentTime model =
    CurrentTime
        (getCurrentHour model)
        (getCurrentMinute model)
        (getCurrentSecond model)


getCurrentHour : Model -> HourStr
getCurrentHour model =
    String.fromInt (Time.toHour model.zone model.time)


getCurrentMinute : Model -> MinuteStr
getCurrentMinute model =
    String.fromInt (Time.toMinute model.zone model.time)


getCurrentSecond : Model -> SecondStr
getCurrentSecond model =
    String.fromInt (Time.toSecond model.zone model.time)


digitalClock : CurrentTime -> Html Msg
digitalClock t =
    h1 [] [ H.text (t.hour ++ ":" ++ t.minute ++ ":" ++ t.second) ]


analogClock : CurrentTime -> Html Msg
analogClock t =
    div []
        [ svg [ SA.width "350", SA.height "350", SA.viewBox "0 0 350 350" ]
            [ analogClockFrame
            , analogHourScale "0"
            , analogHourScale "30"
            , analogHourScale "60"
            , analogHourScale "90"
            , analogHourScale "120"
            , analogHourScale "150"
            , analogHourScale "180"
            , analogHourScale "210"
            , analogHourScale "240"
            , analogHourScale "270"
            , analogHourScale "300"
            , analogHourScale "330"
            , analogClockSecondHand <| angleSecond t.second
            , analogClockMinuteHand <| angleMinute t.minute t.second
            , analogClockHourHand <| angleHour t.hour t.minute
            ]
        ]


angleSecond : SecondStr -> SecondAngle
angleSecond second =
    second
        |> String.toFloat
        |> Maybe.map ((*) 6)
        |> Maybe.withDefault 0
        |> String.fromFloat


angleMinute : MinuteStr -> SecondStr -> MinuteAngle
angleMinute minute second =
    let
        additionalAngle =
            second
                |> String.toFloat
                |> Maybe.map ((*) (6 / 60))
                |> Maybe.withDefault 0
    in
    minute
        |> String.toFloat
        |> Maybe.map ((*) 6)
        |> Maybe.map ((+) additionalAngle)
        |> Maybe.withDefault 0
        |> String.fromFloat


angleHour : HourStr -> MinuteStr -> HourAngle
angleHour hour minute =
    let
        additionalAngle =
            minute
                |> String.toFloat
                |> Maybe.map ((*) (30 / 60))
                |> Maybe.withDefault 0
    in
    hour
        |> String.toFloat
        |> Maybe.map ((*) 30)
        |> Maybe.map ((+) additionalAngle)
        |> Maybe.withDefault 0
        |> String.fromFloat


analogClockSecondHand : String -> Svg Msg
analogClockSecondHand angle =
    S.g
        []
        [ S.line
            [ SA.x1 "175"
            , SA.y1 "185"
            , SA.x2 "175"
            , SA.y2 "50"
            , SA.stroke "#ff0000"
            , SA.strokeWidth "2"
            , SA.transform ("rotate(" ++ angle ++ ", 175, 175)")
            ]
            []
        ]


analogClockMinuteHand : String -> Svg Msg
analogClockMinuteHand angle =
    S.g
        []
        [ S.line
            [ SA.x1 "175"
            , SA.y1 "185"
            , SA.x2 "175"
            , SA.y2 "60"
            , SA.stroke "#000000"
            , SA.strokeWidth "5"
            , SA.transform ("rotate(" ++ angle ++ ", 175, 175)")
            ]
            []
        ]


analogClockHourHand : String -> Svg Msg
analogClockHourHand angle =
    S.g
        []
        [ S.line
            [ SA.x1 "175"
            , SA.y1 "185"
            , SA.x2 "175"
            , SA.y2 "80"
            , SA.stroke "#000000"
            , SA.strokeWidth "10"
            , SA.transform ("rotate(" ++ angle ++ ", 175, 175)")
            ]
            []
        ]


analogClockFrame : Svg Msg
analogClockFrame =
    S.circle
        [ SA.cx "175"
        , SA.cy "175"
        , SA.r "150"
        , SA.fill "none"
        , SA.stroke "#000000"
        , SA.strokeWidth "5"
        ]
        []


analogHourScale : String -> Svg Msg
analogHourScale angle =
    S.circle
        [ cx "175"
        , cy "40"
        , r "5"
        , stroke "none"
        , fill "#000000"
        , transform ("rotate(" ++ angle ++ ", 175, 175)")
        ]
        []


startAndStopButton : Model -> Html Msg
startAndStopButton model =
    div []
        [ button [ onClick (toggleMoveButtonMsg model) ]
            [ H.text (toggleMoveButtonCaption model) ]
        ]


toggleMoveButtonMsg : Model -> Msg
toggleMoveButtonMsg model =
    case model.move of
        True ->
            StopClock

        False ->
            StartClock


toggleMoveButtonCaption : Model -> String
toggleMoveButtonCaption model =
    case model.move of
        True ->
            "Stop"

        False ->
            "Start"
