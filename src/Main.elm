module Main exposing (main, update, view)

import Browser
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)


main =
    Browser.sandbox { init = init, update = update, view = view }


getFieldWidthInMeters =
    55.0


getFieldLengthInMeters =
    91.4


getGoalWidthInMeters =
    3.66


getScoringCircleRadiusInMeters =
    14.63


getDefensivePenaltyCornerOffsetInMeters =
    5.0


getOffensivePenaltyCornerOffsetInMeters =
    10.0


getQuarterLineOffsetInMeters =
    22.85


getMidLineOffsetInMeters =
    2.0 * 22.85


type alias Player =
    { posnXMeters : Float
    , posnYMeters : Float
    , headingDeg : Int
    , speedMetersPerSec : Float
    }


type alias Model =
    { viewX : Int
    , viewY : Int
    , originX : Int
    , originY : Int
    , players : List Player
    }



--type Msg
--    = Increment
--    | Decrement


init =
    Model 800 800 20 20 [ Player 10.0 23.0 0 0.0 ]


update msg model =
    case msg of
        _ ->
            model


view model =
    let
        ( pxWidth, pxHeight ) =
            getViewWidthAndHeightInPixelsAsStrings model

        ( pxOriginX, pxOriginY ) =
            getOriginInPixelsAsStrings model

        ( pxFieldWidth, pxFieldHeight ) =
            getFieldWidthAndHeightInPixelsAsStrings model

        scoringArcPath =
            getScoringArcPath model

        quarterLinePath =
            getLine model 0.25

        midLinePath =
            getLine model 0.5

        threeQuarterLine =
            getLine model 0.75

        oppScoringArcPath =
            getOppositeEndScoringArcPath model

        goalPath =
            getGoalPath model

        oppGoalPath =
            getOppositeEndGoalPath model

        playerPosns =
            getPlayerPosnsInPixels model
    in
    div []
        [ svg
            [ width pxWidth
            , height pxHeight
            , viewBox (String.join " " [ "0", "0", pxWidth, pxHeight ])
            ]
            [ rect
                [ x pxOriginX
                , y pxOriginY
                , width pxFieldWidth
                , height pxFieldHeight
                , rx "0"
                , ry "0"
                , fill "#ff00ff"
                , fillOpacity "0.5"
                ]
                []

            -- , circle
            --     [ cx "50"
            --     , cy "50"
            --     , r "50"
            --     , fill "#ff0000"
            --     , fillOpacity "0.5"
            --     ]
            --     []
            , Svg.path
                [ d scoringArcPath

                --"M 10 315 L 110 215 A 30 50 0 0 1 162.55 162.45 L 172.55 152.45 A 30 50 -45 0 1 215.1 109.9 L 315 10"
                --"M 10 10 A 100 100 0 0 0 140 140"
                , stroke "black"
                , fill "white"
                , strokeWidth "2"
                , fillOpacity "0.5"
                ]
                []
            , Svg.path
                [ d quarterLinePath

                --"M 10 315 L 110 215 A 30 50 0 0 1 162.55 162.45 L 172.55 152.45 A 30 50 -45 0 1 215.1 109.9 L 315 10"
                --"M 10 10 A 100 100 0 0 0 140 140"
                , stroke "black"
                , strokeWidth "2"
                ]
                []
            , Svg.path
                [ d midLinePath

                --"M 10 315 L 110 215 A 30 50 0 0 1 162.55 162.45 L 172.55 152.45 A 30 50 -45 0 1 215.1 109.9 L 315 10"
                --"M 10 10 A 100 100 0 0 0 140 140"
                , stroke "black"
                , strokeWidth "3"
                ]
                []
            , Svg.path
                [ d threeQuarterLine
                , stroke "black"
                , strokeWidth "2"
                ]
                []
            , Svg.path
                [ d oppScoringArcPath

                --"M 10 315 L 110 215 A 30 50 0 0 1 162.55 162.45 L 172.55 152.45 A 30 50 -45 0 1 215.1 109.9 L 315 10"
                --"M 10 10 A 100 100 0 0 0 140 140"
                , stroke "black"
                , fill "blue"
                , strokeWidth "2"
                , fillOpacity "0.5"
                ]
                []
            , Svg.path
                [ d goalPath
                , stroke "white"
                , strokeWidth "2"
                ]
                []
            , Svg.path
                [ d oppGoalPath
                , stroke "white"
                , strokeWidth "2"
                ]
                []
            , renderPlayer playerPosns
            ]
        ]


renderPlayer posns =
    case posns of
        [] ->
            circle
                [ cx "50"
                , cy "50"
                , r "50"
                , fill "#ff0000"
                , fillOpacity "0.5"
                ]
                []

        first :: rest ->
            circle
                [ cx (Tuple.first first)
                , cy (Tuple.second first)
                , r "10"
                , fill "#ff0000"
                , fillOpacity "0.5"
                ]
                []


getPlayerPosnsInPixels model =
    List.map (\p -> getPlayerPosnInPixels model p) model.players


getPlayerPosnInPixels model player =
    let
        pX =
            toPixelsX model getFieldWidthInMeters player.posnXMeters

        pY =
            toPixelsY model getFieldLengthInMeters player.posnYMeters
    in
    ( pX, pY )


getGoalPath model =
    let
        pX1 =
            toPixelsX model getFieldWidthInMeters (getFieldWidthInMeters / 2.0 - getGoalWidthInMeters / 2.0)

        pY1 =
            toPixelsY model getFieldLengthInMeters 0.0

        pX2 =
            pX1

        pY2 =
            toPixelsY model getFieldLengthInMeters -1.0

        pX3 =
            toPixelsX model getFieldWidthInMeters (getFieldWidthInMeters / 2.0 + getGoalWidthInMeters / 2.0)

        pY3 =
            pY2

        pX4 =
            pX3

        pY4 =
            pY1
    in
    String.join " "
        [ "M"
        , pX1
        , pY1
        , "L"
        , pX2
        , pY2
        , "L"
        , pX3
        , pY3
        , "L"
        , pX4
        , pY4
        ]


getOppositeEndGoalPath model =
    let
        pX1 =
            toPixelsX model getFieldWidthInMeters (getFieldWidthInMeters / 2.0 - getGoalWidthInMeters / 2.0)

        pY1 =
            toPixelsY model getFieldLengthInMeters getFieldLengthInMeters

        pX2 =
            pX1

        pY2 =
            toPixelsY model getFieldLengthInMeters (getFieldLengthInMeters + 1.0)

        pX3 =
            toPixelsX model getFieldWidthInMeters (getFieldWidthInMeters / 2.0 + getGoalWidthInMeters / 2.0)

        pY3 =
            pY2

        pX4 =
            pX3

        pY4 =
            pY1
    in
    String.join " "
        [ "M"
        , pX1
        , pY1
        , "L"
        , pX2
        , pY2
        , "L"
        , pX3
        , pY3
        , "L"
        , pX4
        , pY4
        ]


getLine model percentage =
    let
        pX1 =
            toPixelsX model getFieldWidthInMeters 0.0

        pY1 =
            toPixelsY model getFieldLengthInMeters (percentage * getFieldLengthInMeters)

        pX2 =
            toPixelsX model getFieldWidthInMeters getFieldWidthInMeters

        pY2 =
            pY1
    in
    String.join " "
        [ "M"
        , pX1
        , pY1
        , "L"
        , pX2
        , pY2
        ]


toPixelsX model physicalRange value =
    String.fromFloat (toFloat model.originX + (toFloat model.viewX - 2.0 * toFloat model.originX) / physicalRange * value)


toPixelsY model physicalRange value =
    String.fromFloat (toFloat model.originY + (toFloat model.viewY - 2.0 * toFloat model.originY) / physicalRange * value)


getScoringArcPath model =
    let
        pX1 =
            toPixelsX model getFieldWidthInMeters (getFieldWidthInMeters / 2.0 - getGoalWidthInMeters / 2.0 - getScoringCircleRadiusInMeters)

        pY1 =
            toPixelsY model getFieldLengthInMeters 0.0

        pX2 =
            toPixelsX model getFieldWidthInMeters (getFieldWidthInMeters / 2.0 - getGoalWidthInMeters / 2.0)

        pY2 =
            toPixelsY model getFieldLengthInMeters getScoringCircleRadiusInMeters

        arcRadiusX =
            toPixelsX model getFieldWidthInMeters getScoringCircleRadiusInMeters

        arcRadiusY =
            toPixelsY model getFieldLengthInMeters getScoringCircleRadiusInMeters

        pX3 =
            toPixelsX model getFieldWidthInMeters (getFieldWidthInMeters / 2.0 + getGoalWidthInMeters / 2.0)

        pY3 =
            pY2

        pX4 =
            toPixelsX model getFieldWidthInMeters (getFieldWidthInMeters / 2.0 + getGoalWidthInMeters / 2.0 + getScoringCircleRadiusInMeters)

        pY4 =
            pY1
    in
    String.join " "
        [ "M"
        , pX1
        , pY1
        , "A"
        , arcRadiusX
        , arcRadiusY
        , "0"
        , "0"
        , "0"
        , pX2
        , pY2
        , "L"
        , pX3
        , pY3
        , "A"
        , arcRadiusX
        , arcRadiusY
        , "0"
        , "0"
        , "0"
        , pX4
        , pY4
        ]


getOppositeEndScoringArcPath model =
    let
        pX1 =
            toPixelsX model getFieldWidthInMeters (getFieldWidthInMeters / 2.0 + getGoalWidthInMeters / 2.0 + getScoringCircleRadiusInMeters)

        pY1 =
            toPixelsY model getFieldLengthInMeters getFieldLengthInMeters

        pX2 =
            toPixelsX model getFieldWidthInMeters (getFieldWidthInMeters / 2.0 + getGoalWidthInMeters / 2.0)

        pY2 =
            toPixelsY model getFieldLengthInMeters (getFieldLengthInMeters - getScoringCircleRadiusInMeters)

        arcRadiusX =
            toPixelsX model getFieldWidthInMeters getScoringCircleRadiusInMeters

        arcRadiusY =
            toPixelsY model getFieldLengthInMeters getScoringCircleRadiusInMeters

        pX3 =
            toPixelsX model getFieldWidthInMeters (getFieldWidthInMeters / 2.0 - getGoalWidthInMeters / 2.0)

        pY3 =
            pY2

        pX4 =
            toPixelsX model getFieldWidthInMeters (getFieldWidthInMeters / 2.0 - getGoalWidthInMeters / 2.0 - getScoringCircleRadiusInMeters)

        pY4 =
            pY1
    in
    String.join " "
        [ "M"
        , pX1
        , pY1
        , "A"
        , arcRadiusX
        , arcRadiusY
        , "0"
        , "0"
        , "0"
        , pX2
        , pY2
        , "L"
        , pX3
        , pY3
        , "A"
        , arcRadiusX
        , arcRadiusY
        , "0"
        , "0"
        , "0"
        , pX4
        , pY4
        ]



--    "M 10 10 A 100 100 0 0 0 210 10"


getViewWidthAndHeightInPixelsAsStrings model =
    ( String.fromInt model.viewX, String.fromInt model.viewY )


getOriginInPixelsAsStrings model =
    ( String.fromInt 0, String.fromInt 0 )


getFieldWidthAndHeightInPixelsAsStrings model =
    ( String.fromInt model.viewX, String.fromInt model.viewY )
