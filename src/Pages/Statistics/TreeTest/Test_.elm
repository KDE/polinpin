module Pages.Statistics.TreeTest.Test_ exposing (Model, Msg, page)

import Chart as C
import Chart.Attributes as CA
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Gen.Params.Statistics.TreeTest.Test_ exposing (Params)
import HTTPExt
import Http
import Page
import Request
import Shared
import TreeTest
import UI
import View exposing (View)
import UI exposing (edges)
import Tree


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.protected.element
        (\user ->
            { init = init user req.params.test
            , update = update
            , view = view shared
            , subscriptions = subscriptions
            }
        )



-- INIT


type Model
    = Loading
    | LoadFailed Http.Error
    | Loaded LoadedModel


type alias LoadedModel =
    { statistics : TreeTest.TreeTestStatistics
    , activeTab : Tab
    }

type Tab
    = Overview
    | PerTaskStats

init : Shared.User -> String -> ( Model, Cmd Msg )
init user test =
    ( Loading, TreeTest.treeTestStatistics user.token test FinishedLoading )



-- UPDATE


type Msg
    = FinishedLoading (Result Http.Error TreeTest.TreeTestStatistics)
    | TabClicked Tab


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( FinishedLoading (Ok stats), Loading ) ->
            ( Loaded (LoadedModel stats Overview), Cmd.none )

        ( FinishedLoading (Err error), Loading ) ->
            ( LoadFailed error, Cmd.none )

        ( TabClicked _, Loading ) ->
            ( model, Cmd.none )

        ( _, Loaded loadedModel ) ->
            let
                ( new, cmd ) =
                    updateLoaded msg loadedModel
            in
            ( Loaded new, cmd )

        ( _, LoadFailed _ ) ->
            ( model, Cmd.none )


updateLoaded : Msg -> LoadedModel -> ( LoadedModel, Cmd Msg )
updateLoaded msg model =
    case msg of
        FinishedLoading _ ->
            ( model, Cmd.none )

        TabClicked tab ->
            ( { model | activeTab = tab }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    case model of
        Loading ->
            View
                "Loading"
                none
                Nothing

        Loaded loaded ->
            viewLoaded shared loaded

        LoadFailed err ->
            View "Loading failed" (text (HTTPExt.errorToString err)) Nothing


titledCard : String -> List (Attribute msg) -> List (Element msg) -> Element msg
titledCard title attrs els =
    column
        (spacing 20 :: attrs)
        ((el [ centerX ] <| UI.labelScaled 2 title) :: els)


cardCont : String -> Float -> String -> Color -> Element msg
cardCont title percent desc color =
    titledCard title
        [ width fill ]
        [ el [ centerX, Background.color color, Font.color <| rgb255 0xff 0xff 0xff, padding 12 ] <| UI.labelScaled 5 (String.fromInt (round (percent * 100.0)) ++ "%")
        , el [ centerX ] <| UI.labelScaled -1 desc
        ]


taskStatistics : Int -> Int -> TreeTest.TaskStatistics -> Element Msg
taskStatistics userCount num stats =
    let
        item percent color =
            el [ width (fillPortion (round (percent * 1000))), height (px 16), Background.color color ] none
    in
    column [ width fill, spacing 8 ]
        [ UI.labelScaled -1 ("Task " ++ String.fromInt num)
        , row [ width fill, Border.color <| rgb255 0x00 0x00 0x00, Border.width 4 ]
            [ item ((toFloat stats.incorrectDirect) / (toFloat userCount)) colorIncorrectDirect
            , item ((toFloat stats.incorrectIndirect) / (toFloat userCount)) colorIncorrectIndirect
            , item ((toFloat stats.correctIndirect) / (toFloat userCount)) colorCorrectIndirect
            , item ((toFloat stats.correctDirect) / (toFloat userCount)) colorCorrectDirect
            ]
        ]


colorIncorrectDirect : Color
colorIncorrectDirect =
    rgb255 255 0 0


colorIncorrectIndirect : Color
colorIncorrectIndirect =
    rgb255 0xFF 0xA5 0x00


colorCorrectIndirect : Color
colorCorrectIndirect =
    rgb255 0x99 0x32 0xCC


colorCorrectDirect : Color
colorCorrectDirect =
    rgb255 0 255 0


legend : Color -> String -> Element msg
legend color key =
    let
        square =
            el [ Background.color color, width (px <| 16 + (4*2)), height (px <| 16 + (4*2)), Border.width 4, Border.color <| rgb255 0x00 0x00 0x00 ] none
    in
    row [ spacing 4 ]
        [ square, UI.labelScaled -1 key ]

viewTabs : LoadedModel -> Element Msg
viewTabs model =
    let
        make label role =
            UI.tab label (model.activeTab == role) (TabClicked role)

        roles =
            [ make "Overview" Overview
            , make "Per-Task Statistics" PerTaskStats
            ]
    in
    row [ paddingEach { edges | left = 8 }, spacing 6 ]
        roles


viewStatistics : Shared.Model -> LoadedModel -> Element Msg
viewStatistics shared model =
    column [ centerX, padding 16, width (fill |> maximum 800), spacing 16 ]
        [ wrappedRow [ width fill, spacing 32 ]
            [ UI.card [ width fill ]
                (cardCont "Success" model.statistics.percentCorrect "Average success rate across all tasks." (rgb255 0x00 0x93 0x56))
            , UI.card [ width fill ]
                (cardCont "Directness" model.statistics.percentDirect "Percentage of tasks completed without backtracking." (rgb255 0x00 0x92 0x7E))
            ]
        , UI.card [ width fill ]
            (titledCard "Completion Time"
                [ width fill ]
                [ paragraph []
                    [ UI.labelScaled 1
                        ("The median time for completion was "
                            ++ String.fromInt (round (model.statistics.medianTime / 1000))
                            ++ " seconds."
                        )
                    , UI.labelScaled 1
                        (" The minimum time was "
                            ++ String.fromInt (round (model.statistics.minimumTime / 1000))
                            ++ " seconds and the maximum time was "
                            ++ String.fromInt (round (model.statistics.maximumTime / 1000))
                            ++ " seconds."
                        )
                    ]
                ]
            )
        , UI.card [ width fill ]
            (titledCard "Per-Task Statistics"
                [ width fill ]
                (wrappedRow
                    [ centerX, spacing 16 ]
                    [ legend colorIncorrectDirect "Direct Failure"
                    , legend colorIncorrectIndirect "Indirect Failure"
                    , legend colorCorrectIndirect "Indirect Success"
                    , legend colorCorrectDirect "Direct Success"
                    ]
                    :: (model.statistics.taskStatistics |> List.indexedMap (taskStatistics model.statistics.userCount))
                )
            )
        ]

zip : List a -> List b -> List (a, b)
zip =
    List.map2 Tuple.pair

viewTaskStats : Tree.Node TreeTest.Item -> Int -> (TreeTest.Task, TreeTest.TaskStatistics) -> Element Msg
viewTaskStats tree idx (task, stats) =
    let
        gText (Tree.Node _ cont _) =
            cont.text

        label =
            Tree.nodeByIDWithParents task.correctAnswer tree
                |> Maybe.map (\( node, parents ) -> (parents ++ [ node ]) |> List.map gText |> String.join " / ")
                |> Maybe.withDefault "Failed to find node"
    in
    UI.card [ width fill ]
        ( column [ spacing 16, width fill ]
            [ UI.labelScaled -2 ("Task " ++ (String.fromInt (idx + 1)))
            , paragraph [] [UI.label [] task.text]
            , (UI.labelScaled -1 label)
            , UI.separator [ width fill ]
            , UI.label [] "Correct"
            , column [ paddingEach { edges | left = 8 }, spacing 10 ]
                [ UI.label [] ("Direct " ++ (String.fromInt stats.correctDirect))
                , UI.label [] ("Indirect " ++ (String.fromInt stats.correctIndirect))
                ]
            , UI.label [] "Incorrect"
            , column [ paddingEach { edges | left = 8 }, spacing 10 ]
                [ UI.label [] ("Direct " ++ (String.fromInt stats.incorrectDirect))
                , UI.label [] ("Indirect " ++ (String.fromInt stats.incorrectIndirect))
                ]
            ]
        )

viewPerTaskStats : Shared.Model -> LoadedModel -> Element Msg
viewPerTaskStats shared model =
    column [ centerX, padding 16, width (fill |> maximum 800), spacing 16 ]
        (
            (zip model.statistics.study.tasks model.statistics.taskStatistics)
            |> List.indexedMap (viewTaskStats model.statistics.study.tree)
        )

viewLoaded : Shared.Model -> LoadedModel -> View Msg
viewLoaded shared model =
    View
        "Statistics"
        (UI.with shared
            [ UI.subToolbar [ text "Statistics" ]
            , viewTabs model
            , case model.activeTab of
                Overview ->
                    viewStatistics shared model

                PerTaskStats ->
                    viewPerTaskStats shared model
            ]
        )
        Nothing
