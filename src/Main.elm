module Main exposing (main, update, view)

import Browser
import Browser.Events exposing (onKeyPress)
import Debug
import Dict exposing (Dict)
import Html exposing (Html, button, div, text)
import Html.Events exposing (onClick)
import Json.Decode as Decode
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
    , hasBall : Bool
    , state : State
    }


type alias Model =
    { viewX : Int
    , viewY : Int
    , originX : Int
    , originY : Int
    , players : List Player
    , passingTarget : Maybe Player
    , ball : Ball
    , epoch : Int
    }


type alias Ball =
    { posnXMeters : Float
    , posnYMeters : Float
    , speedX : Float
    , speedY : Float
    }


type alias Speed =
    { for : String
    , relativeTo : String
    , speedX : Float
    , speedY : Float
    }


type State
    = PursuingBall
    | SpreadingOut
    | PossessingBall
    | HoldingPosn



--type Msg
--    = Increment
--    | Decrement


type Msg
    = TimeUpdate Time.Posix
    | CharacterKey Char
    | ControlKey String


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model 800 800 20 20 [ Player "1" 10.0 23.0 0 0.0 False HoldingPosn, Player "2" 11.0 24.0 0 0.0 False HoldingPosn, Player "3" 12.0 23.0 0 0.0 False HoldingPosn ] Nothing (Ball 11.0 30.0 0.0 0.0) 0, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        CharacterKey c ->
            ( model, Cmd.none )

        ControlKey string ->
            ( model, Cmd.none )

        TimeUpdate tposix ->
            let
                ( updatedPlayers, updatedPassingTarget, updatedBall ) =
                    updateAll model
            in
            ( { model
                | players = updatedPlayers
                , passingTarget = updatedPassingTarget
                , ball = updatedBall
                , epoch = model.epoch + 1
              }
            , Cmd.none
            )


updateAll : Model -> ( List Player, Maybe Player, Ball )
updateAll model =
    let
        playerSpeeds =
            updateSpeeds model.players model.ball model.passingTarget

        updatedPlayers =
            updatePlayers playerSpeeds []

        ( updatedPlayers2, updatedPassingTarget, updatedBall ) =
            handlePotentialPass updatedPlayers model.passingTarget model.ball model.epoch

        --        ( updatedPlayers3, updatedPassingTarget3, updatedBall3 ) =
        --            handlePassReceipt updatedPlayers2 updatedPassingTarget model.ball
        _ =
            Debug.log "speed updated" playerSpeeds

        _ =
            Debug.log "player updated" updatedPlayers
    in
    ( updatedPlayers2, updatedPassingTarget, updatedBall )


handlePassReceipt : List Player -> Maybe Player -> Ball -> ( List Player, Maybe Player, Ball )
handlePassReceipt players currentPassingTarget ball =
    case currentPassingTarget of
        Nothing ->
            ( players, currentPassingTarget, ball )

        Just aPlayer ->
            let
                hasPossession =
                    List.filter (\p -> p.hasBall) players

                others =
                    List.filter (\p -> not p.hasBall) players
            in
            case hasPossession of
                first :: rest ->
                    ( players, currentPassingTarget, ball )

                [] ->
                    let
                        ( withinReach, notWithinReach ) =
                            determinePlayersWithinReachOfBall players ball
                    in
                    case withinReach of
                        [] ->
                            ( players, currentPassingTarget, { ball | posnXMeters = ball.posnXMeters + ball.speedX, posnYMeters = ball.posnYMeters + ball.speedY } )

                        first :: rest ->
                            ( List.append ({ first | hasBall = True } :: rest) notWithinReach
                            , Nothing
                            , { ball
                                | posnXMeters = first.posnXMeters
                                , posnYMeters = first.posnYMeters
                                , speedX = 0.0
                                , speedY = 0.0
                              }
                            )


handlePotentialPass : List Player -> Maybe Player -> Ball -> Int -> ( List Player, Maybe Player, Ball )
handlePotentialPass players currentPassingTarget ball epoch =
    if epoch == 10 then
        let
            hasPossession =
                List.filter (\p -> p.hasBall) players

            others =
                List.filter (\p -> not p.hasBall) players
        in
        case hasPossession of
            first :: rest ->
                let
                    noLongerPossessing =
                        { first | hasBall = False }

                    target =
                        identifyPassingTarget others

                    ( ballSpeedX, ballSpeedY ) =
                        determineBallSpeedToInterceptTarget noLongerPossessing target

                    -- pass the ball
                in
                ( noLongerPossessing :: others
                , target
                , { ball
                    | speedX = ballSpeedX
                    , speedY = ballSpeedY
                    , posnXMeters = first.posnXMeters
                    , posnYMeters = first.posnYMeters
                  }
                )

            [] ->
                -- check to see if any player can "reach" the current ball
                let
                    ( withinReach, notWithinReach ) =
                        determinePlayersWithinReachOfBall players ball
                in
                case withinReach of
                    [] ->
                        ( players, currentPassingTarget, { ball | posnXMeters = ball.posnXMeters + ball.speedX, posnYMeters = ball.posnYMeters + ball.speedY } )

                    first :: rest ->
                        ( List.append ({ first | hasBall = True } :: rest) notWithinReach
                        , Nothing
                        , { ball
                            | posnXMeters = first.posnXMeters
                            , posnYMeters = first.posnYMeters
                            , speedX = 0.0
                            , speedY = 0.0
                          }
                        )

    else
        let
            hasPossession =
                List.filter (\p -> p.hasBall) players
        in
        case hasPossession of
            first :: rest ->
                ( players, currentPassingTarget, { ball | posnXMeters = first.posnXMeters, posnYMeters = first.posnYMeters } )

            [] ->
                -- check to see if any player can "reach" the current ball
                let
                    ( withinReach, notWithinReach ) =
                        determinePlayersWithinReachOfBall players ball
                in
                case withinReach of
                    [] ->
                        ( players
                        , currentPassingTarget
                        , { ball | posnXMeters = ball.posnXMeters + ball.speedX, posnYMeters = ball.posnYMeters + ball.speedY }
                        )

                    first :: rest ->
                        ( List.append ({ first | hasBall = True } :: rest) notWithinReach
                        , currentPassingTarget
                        , { ball
                            | posnXMeters = first.posnXMeters
                            , posnYMeters = first.posnYMeters
                            , speedX = 0.0
                            , speedY = 0.0
                          }
                        )



--                ( players, { ball | posnXMeters = ball.posnXMeters + ball.speedX, posnYMeters = ball.posnYMeters + ball.speedY } )


determinePlayersWithinReachOfBall players ball =
    let
        withinReach =
            List.filter (\p -> isWithinRadiusOf p ball 2.0) players

        notWithinReach =
            List.filter (\p -> not (isWithinRadiusOf p ball 2.0)) players
    in
    ( withinReach, notWithinReach )


isWithinRadiusOf player ball radius =
    let
        dx =
            ball.posnXMeters - player.posnXMeters

        dy =
            ball.posnYMeters - player.posnYMeters

        dd =
            sqrt (dx * dx + dy * dy)

        -- is ball moving TOWARDS this player
        bx2 =
            ball.posnXMeters + ball.speedX

        by2 =
            ball.posnYMeters + ball.speedY

        dx2 =
            bx2 - player.posnXMeters

        dy2 =
            by2 - player.posnYMeters

        dd2 =
            sqrt (dx2 * dx2 + dy2 * dy2)

        u1 =
            dx / dd

        u2 =
            dy / dd

        b1 =
            ball.speedX / 2.0

        b2 =
            ball.speedY / 2.0

        dp =
            u1 * b1 + u2 * b2
    in
    dd < radius && dp <= 0


determineBallSpeedToInterceptTarget : Player -> Maybe Player -> ( Float, Float )
determineBallSpeedToInterceptTarget player target =
    case target of
        Just targetPlayer ->
            let
                dx =
                    targetPlayer.posnXMeters - player.posnXMeters

                dy =
                    targetPlayer.posnYMeters - player.posnYMeters

                d =
                    sqrt (dx * dx + dy * dy)

                u1 =
                    dx / d

                u2 =
                    dy / d

                v1 =
                    targetPlayer.speedMetersPerSec * sin (degrees targetPlayer.headingDeg) / 1.5

                v2 =
                    targetPlayer.speedMetersPerSec * cos (degrees targetPlayer.headingDeg) / 1.5

                w1 =
                    u1 + v1

                w2 =
                    u2 + v2
            in
            ( 2.0 * w1, 2.0 * w2 )

        Nothing ->
            ( 0.0, 0.0 )


identifyPassingTarget : List Player -> Maybe Player
identifyPassingTarget players =
    List.head players


updateBall : List Player -> Ball -> Ball
updateBall players ball =
    let
        hasPossession =
            List.filter (\p -> p.hasBall) players
    in
    case hasPossession of
        first :: rest ->
            { ball | posnXMeters = first.posnXMeters, posnYMeters = first.posnYMeters }

        [] ->
            { ball | posnXMeters = ball.posnXMeters + ball.speedX, posnYMeters = ball.posnYMeters + ball.speedY }


limitSpeed : Float -> Float -> Float
limitSpeed inputSpeed maxSpeed =
    if inputSpeed > 0.0 then
        Basics.min maxSpeed inputSpeed

    else
        inputSpeed


updateSpeeds : List Player -> Ball -> Maybe Player -> List Player
updateSpeeds players ball passingTarget =
    let
        targetSpeeds =
            evaluateSpeeds [] (List.head players) (List.drop 1 players) ball players passingTarget

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


combineByPlayer : List Speed -> Dict String ( Float, Float ) -> Dict String ( Float, Float )
combineByPlayer targetSpeeds targetByPlayerSoFar =
    case targetSpeeds of
        [] ->
            targetByPlayerSoFar

        first :: rest ->
            let
                relSpeed1 =
                    first

                updatedTargetByPlayer1 =
                    Dict.update relSpeed1.for (Maybe.map (\p -> ( Tuple.first p + relSpeed1.speedX, Tuple.second p + relSpeed1.speedY ))) targetByPlayerSoFar
            in
            combineByPlayer rest updatedTargetByPlayer1


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

                        speedLimited =
                            limitSpeed speedMetersPerSec 1.5

                        updated =
                            { first
                                | headingDeg = headingDeg
                                , speedMetersPerSec = speedLimited
                                , hasBall = first.hasBall
                            }
                    in
                    mapTargetSpeedsToPlayers speedsToApplyByPlayer rest (updated :: updatedSoFar)


computeDistanceToBall player ball =
    let
        dx =
            ball.posnXMeters - player.posnXMeters

        dy =
            ball.posnYMeters - player.posnYMeters
    in
    { player = player
    , distance = sqrt (dx * dx + dy * dy)
    }


playerAlreadyInPossession players =
    let
        hasPossession =
            List.filter (\p -> p.hasBall) players
    in
    case hasPossession of
        [] ->
            False

        first :: rest ->
            True


ballIsBeingPassed passingTarget =
    case passingTarget of
        Nothing ->
            False

        Just aPlayer ->
            True


checkIfShouldPursueBall player players ball passingTarget =
    if playerAlreadyInPossession players || ballIsBeingPassed passingTarget then
        False

    else
        let
            playerDistances =
                List.map (\p -> computeDistanceToBall p ball) players

            sortedDistances =
                List.sortBy .distance playerDistances
        in
        case List.head sortedDistances of
            Nothing ->
                False

            Just aDistance ->
                if aDistance.player.uid == player.uid then
                    True

                else
                    False


computeBallPursuitSpeed shouldPursue aPlayer ball =
    if shouldPursue then
        let
            ( sx, sy ) =
                computeSpeedToBall aPlayer ball
        in
        Speed aPlayer.uid aPlayer.uid sx sy

    else
        Speed aPlayer.uid aPlayer.uid 0.0 0.0


computeSpeedToBall player ball =
    -- determine course to ball
    let
        dx =
            ball.posnXMeters - player.posnXMeters

        dy =
            ball.posnYMeters - player.posnYMeters

        courseToBall =
            atan2 dy dx

        dd =
            sqrt (dx * dx + dy * dy)

        spd =
            Basics.min dd 1.5
    in
    ( spd * sin courseToBall, spd * cos courseToBall )


evaluateSpeeds : List Speed -> Maybe Player -> List Player -> Ball -> List Player -> Maybe Player -> List Speed
evaluateSpeeds resultsSoFar current others ball allPlayers passingTarget =
    case current of
        Nothing ->
            resultsSoFar

        Just aPlayer ->
            let
                shouldMoveToBall =
                    checkIfShouldPursueBall aPlayer allPlayers ball passingTarget

                pursuitSpeed =
                    computeBallPursuitSpeed shouldMoveToBall aPlayer ball

                speeds =
                    determineRelativeSpeeds aPlayer others

                updatedSpeeds =
                    List.append speeds resultsSoFar

                updatedSpeeds2 =
                    pursuitSpeed :: updatedSpeeds
            in
            evaluateSpeeds updatedSpeeds2 (List.head others) (List.drop 1 others) ball allPlayers passingTarget


determineRelativeSpeeds : Player -> List Player -> List Speed
determineRelativeSpeeds current others =
    List.concat (List.map (\o -> determineRelativeSpeed current o) others)


determineRelativeSpeed : Player -> Player -> List Speed
determineRelativeSpeed current other =
    let
        targetDistanceMeters =
            10

        maxSpeedMetersPerSec =
            1.5

        distanceBetween =
            computeDistanceBetween current other

        pairString =
            String.append (String.append current.uid " <-> ") other.uid

        _ =
            Debug.log (String.append "distance between" pairString) distanceBetween

        courseBetweenRadians =
            computeCourseBetween current other

        targetSpeedMetersPerSec =
            getTargetSpeed distanceBetween

        speedX1 =
            targetSpeedMetersPerSec * sin courseBetweenRadians

        speedY1 =
            targetSpeedMetersPerSec * cos courseBetweenRadians

        speedX2 =
            targetSpeedMetersPerSec * sin (courseBetweenRadians + pi)

        speedY2 =
            targetSpeedMetersPerSec * cos (courseBetweenRadians + pi)

        speed1 =
            Speed current.uid other.uid speedX1 speedY1

        speed2 =
            Speed other.uid current.uid speedX2 speedY2
    in
    [ speed1, speed2 ]


getTargetSpeed distanceBetween =
    let
        targetDistanceMeters =
            10

        maxSpeedMetersPerSec =
            1.5

        dR =
            distanceBetween - targetDistanceMeters
    in
    if abs dR < targetDistanceMeters then
        dR * dR / (targetDistanceMeters * targetDistanceMeters) * maxSpeedMetersPerSec

    else
        0.0


computeDistanceBetween : Player -> Player -> Float
computeDistanceBetween player1 player2 =
    let
        dx =
            player1.posnXMeters - player2.posnXMeters

        dy =
            player1.posnYMeters - player2.posnYMeters
    in
    sqrt (dx * dx + dy * dy)


computeCourseBetween : Player -> Player -> Float
computeCourseBetween player1 player2 =
    let
        dx =
            player1.posnXMeters - player2.posnXMeters

        dy =
            player1.posnYMeters - player2.posnYMeters
    in
    atan2 dy dx


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
    Sub.batch [ Time.every 1000 TimeUpdate, onKeyPress keyDecoder ]


view model =
    let
        ( pxWidth, pxHeight ) =
            getViewWidthAndHeightInPixelsAsStrings model

        fieldEntities =
            renderField model

        playerEntities =
            renderPlayers model

        ballEntity =
            renderBall model
    in
    div []
        [ svg
            [ width pxWidth
            , height pxHeight
            , viewBox (String.join " " [ "0", "0", pxWidth, pxHeight ])
            ]
            (List.append
                (List.append playerEntities fieldEntities)
                ballEntity
            )
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


getEndpointInPixels : Model -> Float -> Float -> Float -> Float -> ( String, String )
getEndpointInPixels model originX originY distance headingDeg =
    let
        dx =
            distance * sin (degrees headingDeg)

        dy =
            distance * cos (degrees headingDeg)

        pX2 =
            toPixelsX model getFieldWidthInMeters (originX + dx)

        pY2 =
            toPixelsY model getFieldLengthInMeters (originY + dy)
    in
    ( pX2, pY2 )


renderPlayer model player =
    -- first render things that will ALWAYS be rendered
    let
        posn =
            getPlayerPosnInPixels model player

        pointerEndpoint =
            getEndpointInPixels model player.posnXMeters player.posnYMeters 1.0 player.headingDeg

        hdg =
            line
                [ x1 (Tuple.first posn)
                , y1 (Tuple.second posn)
                , x2 (Tuple.first pointerEndpoint)
                , y2 (Tuple.second pointerEndpoint)
                , stroke "black"
                ]
                []

        icon =
            circle
                [ cx (Tuple.first posn)
                , cy (Tuple.second posn)
                , r "10"
                , fill "#ff0000"
                , fillOpacity "0.5"
                ]
                []

        label =
            Svg.text_
                [ x (Tuple.first posn)
                , y (Tuple.second posn)
                ]
                [ Svg.text player.uid ]
    in
    if player.hasBall then
        let
            icon2 =
                circle
                    [ cx (Tuple.first posn)
                    , cy (Tuple.second posn)
                    , r "15"
                    , strokeWidth "2"
                    , stroke "#335533"
                    , strokeOpacity "1.0"
                    , fillOpacity "0.0"
                    ]
                    []
        in
        [ label, icon, icon2, hdg ]

    else
        case model.passingTarget of
            Nothing ->
                [ icon, label, hdg ]

            Just aPlayer ->
                if aPlayer.uid == player.uid then
                    let
                        targetIcon =
                            circle
                                [ cx (Tuple.first posn)
                                , cy (Tuple.second posn)
                                , r "15"
                                , strokeWidth "2"
                                , fillOpacity "0.0"
                                , strokeDasharray "4"
                                , stroke "red"
                                ]
                                []
                    in
                    [ icon, targetIcon, label, hdg ]

                else
                    [ icon, label, hdg ]



-- case player.hasBall of
--     True ->
--         [ icon, label, ball ]
--
--     False ->
--         [ icon, label ]


renderBall model =
    let
        posn =
            getBallPosnInPixels model
    in
    [ circle
        [ cx (Tuple.first posn)
        , cy (Tuple.second posn)
        , r "4"
        , fill "#ffffff"
        , fillOpacity "1.0"
        ]
        []
    ]


getBallPosnInPixels : Model -> ( String, String )
getBallPosnInPixels model =
    let
        pX =
            toPixelsX model getFieldWidthInMeters model.ball.posnXMeters

        pY =
            toPixelsY model getFieldLengthInMeters model.ball.posnYMeters
    in
    ( pX, pY )


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


keyDecoder : Decode.Decoder Msg
keyDecoder =
    Decode.map toKey (Decode.field "key" Decode.string)


toKey : String -> Msg
toKey keyValue =
    case String.uncons keyValue of
        Just ( char, "" ) ->
            CharacterKey char

        _ ->
            ControlKey keyValue
