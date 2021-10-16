module Main exposing (main, update, view)

import Browser
import Debug
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Svg exposing (..)
import Svg.Attributes exposing (..)
import Time


main =
    Browser.element { init = init, update = update, subscriptions = subscriptions, view = view }


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
    { uid : String
    , posnXMeters : Float
    , posnYMeters : Float
    , headingDeg : Float
    , speedMetersPerSec : Float
    }


type alias Model =
    { viewX : Int
    , viewY : Int
    , originX : Int
    , originY : Int
    , players : List Player
    }


type alias Speed =
    { for : String
    , relativeTo : String
    , speedX : Float
    , speedY : Float
    }



--type Msg
--    = Increment
--    | Decrement


type Msg
    = TimeUpdate Time.Posix


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 800 800 20 20 [ Player "1" 10.0 23.0 0 1.0, Player "2" 11.0 23.0 0 0.0 ], Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        TimeUpdate tposix ->
            let
                temp =
                    updateSpeeds model.players

                updated =
                    updatePlayers temp []

                _ =
                    Debug.log "speed updated" temp

                _ =
                    Debug.log "player updated" updated
            in
            ( { model | players = updated }, Cmd.none )


updateSpeeds : List Player -> List Player
updateSpeeds players =
    let
        targetSpeeds =
            evaluateSpeeds [] (List.head players) (List.drop 1 players)

        -- create dict : String --> List Speed
        speedsToApplyByPlayer =
            combineByPlayer targetSpeeds (initSpeedsToZero players Dict.empty)

        updated =
            mapTargetSpeedsToPlayers speedsToApplyByPlayer players []
    in
    List.reverse updated



--    mapTargetSpeedsToPlayers speedsToApplyByPlayer players []


initSpeedsToZero : List Player -> Dict String ( Float, Float ) -> Dict String ( Float, Float )
initSpeedsToZero players initializedSoFar =
    case players of
        [] ->
            initializedSoFar

        first :: rest ->
            let
                updatedSoFar =
                    Dict.insert first.uid ( 0.0, 0.0 ) initializedSoFar
            in
            initSpeedsToZero rest updatedSoFar


combineByPlayer : List ( Speed, Speed ) -> Dict String ( Float, Float ) -> Dict String ( Float, Float )
combineByPlayer targetSpeeds targetByPlayerSoFar =
    case targetSpeeds of
        [] ->
            targetByPlayerSoFar

        first :: rest ->
            let
                relSpeed1 =
                    Tuple.first first

                relSpeed2 =
                    Tuple.second first

                updatedTargetByPlayer1 =
                    Dict.update relSpeed1.for (Maybe.map (\p -> ( Tuple.first p + relSpeed1.speedX, Tuple.second p + relSpeed1.speedY ))) targetByPlayerSoFar

                updatedTargetByPlayer2 =
                    Dict.update relSpeed2.for (Maybe.map (\p -> ( Tuple.first p + relSpeed2.speedX, Tuple.second p + relSpeed2.speedY ))) updatedTargetByPlayer1
            in
            combineByPlayer rest updatedTargetByPlayer2


mapTargetSpeedsToPlayers : Dict String ( Float, Float ) -> List Player -> List Player -> List Player
mapTargetSpeedsToPlayers speedsToApplyByPlayer players updatedSoFar =
    case players of
        [] ->
            updatedSoFar

        first :: rest ->
            let
                targetSpeedForPlayer =
                    Dict.get first.uid speedsToApplyByPlayer
            in
            case targetSpeedForPlayer of
                Nothing ->
                    mapTargetSpeedsToPlayers speedsToApplyByPlayer rest updatedSoFar

                Just aTuple ->
                    let
                        targetX =
                            Tuple.first aTuple

                        targetY =
                            Tuple.second aTuple

                        headingDeg =
                            180.0 / pi * atan2 targetY targetX

                        speedMetersPerSec =
                            sqrt (targetX * targetX + targetY * targetY)

                        updated =
                            Player first.uid first.posnXMeters first.posnYMeters headingDeg speedMetersPerSec
                    in
                    mapTargetSpeedsToPlayers speedsToApplyByPlayer rest (updated :: updatedSoFar)


evaluateSpeeds : List ( Speed, Speed ) -> Maybe Player -> List Player -> List ( Speed, Speed )
evaluateSpeeds resultsSoFar current others =
    case current of
        Nothing ->
            resultsSoFar

        Just aPlayer ->
            let
                speeds =
                    determineRelativeSpeeds aPlayer others

                updatedSpeeds =
                    List.append speeds resultsSoFar
            in
            evaluateSpeeds updatedSpeeds (List.head others) (List.drop 1 others)


determineRelativeSpeeds : Player -> List Player -> List ( Speed, Speed )
determineRelativeSpeeds current others =
    List.map (\o -> determineRelativeSpeed current o) others


determineRelativeSpeed : Player -> Player -> ( Speed, Speed )
determineRelativeSpeed current other =
    let
        speed1 =
            Speed current.uid other.uid 1.0 0.0

        speed2 =
            Speed other.uid current.uid -1.0 0.0
    in
    ( speed1, speed2 )


updatePlayers players updatedSoFar =
    case players of
        first :: rest ->
            let
                updated =
                    updatePlayer first
            in
            updatePlayers rest (updated :: updatedSoFar)

        [] ->
            List.reverse updatedSoFar


updatePlayer player =
    -- compute dx, dy given heading
    let
        _ =
            Debug.log "player" player.uid

        _ =
            Debug.log "player speed" player.speedMetersPerSec

        _ =
            Debug.log "player heading deg" player.headingDeg

        dx =
            player.speedMetersPerSec * sin (degrees player.headingDeg)

        dy =
            player.speedMetersPerSec * cos (degrees player.headingDeg)

        newHeading =
            updateAndLimitHeading player
    in
    { player
        | posnXMeters = player.posnXMeters + dx
        , posnYMeters = player.posnYMeters + dy

        --        , headingDeg = newHeading
    }


updateAndLimitHeading player =
    let
        newHeading =
            player.headingDeg + 5
    in
    if newHeading > 180.0 then
        newHeading - 360.0

    else if newHeading < -180.0 then
        newHeading + 360.0

    else
        newHeading


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 1000 TimeUpdate


view model =
    let
        ( pxWidth, pxHeight ) =
            getViewWidthAndHeightInPixelsAsStrings model

        fieldEntities =
            renderField model

        playerEntities =
            renderPlayers model
    in
    div []
        [ svg
            [ width pxWidth
            , height pxHeight
            , viewBox (String.join " " [ "0", "0", pxWidth, pxHeight ])
            ]
            (List.append playerEntities fieldEntities)
        ]


renderPlayers model =
    let
        playerEntities =
            List.map (\p -> renderPlayer model p) model.players
    in
    List.concat playerEntities


renderField model =
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
    in
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
    ]


renderPlayer model player =
    let
        posn =
            getPlayerPosnInPixels model player
    in
    [ circle
        [ cx (Tuple.first posn)
        , cy (Tuple.second posn)
        , r "10"
        , fill "#ff0000"
        , fillOpacity "0.5"
        ]
        []
    , Svg.text_
        [ x (Tuple.first posn)
        , y (Tuple.second posn)
        ]
        [ Svg.text player.uid ]
    ]


getPlayerPosnInPixels : Model -> Player -> ( String, String )
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
