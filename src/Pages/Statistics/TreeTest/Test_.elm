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
    }


init : Shared.User -> String -> ( Model, Cmd Msg )
init user test =
    ( Loading, TreeTest.treeTestStatistics user.token test FinishedLoading )



-- UPDATE


type Msg
    = FinishedLoading (Result Http.Error TreeTest.TreeTestStatistics)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( FinishedLoading (Ok stats), Loading ) ->
            ( Loaded (LoadedModel stats), Cmd.none )

        ( FinishedLoading (Err error), Loading ) ->
            ( LoadFailed error, Cmd.none )

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
        [ el [ centerX, Font.color color ] <| UI.labelScaled 5 (String.fromInt (round (percent * 100.0)) ++ "%")
        , el [ centerX ] <| UI.labelScaled -1 desc
        ]


viewLoaded : Shared.Model -> LoadedModel -> View Msg
viewLoaded shared model =
    View
        "Statistics"
        (UI.with shared
            [ UI.subToolbar [ text "Statistics" ]
            , column [ centerX, padding 16, width (fill |> maximum 800), spacing 16 ]
                [ row [ width fill, spacing 32 ]
                    [ UI.card [ width fill ]
                        (cardCont "Success" model.statistics.percentCorrect "Average sucecss rate across all tasks." (rgb255 0x00 0x93 0x56))
                    , UI.card [ width fill ]
                        (cardCont "Directness" model.statistics.percentDirect "Percentage of tasks completed without backtracking." (rgb255 0x00 0x92 0x7E))
                    ]
                , UI.card [ width fill ]
                    (titledCard "Completion Time"
                        [ width fill ]
                        [ paragraph []
                            [ UI.labelScaled 1
                                ("The median time for completion was "
                                    ++ String.fromInt ( round (model.statistics.medianTime / 1000) )
                                    ++ " seconds."
                                )
                            , UI.labelScaled 1
                                (" The minimum time was "
                                    ++ String.fromInt ( round (model.statistics.minimumTime / 1000) )
                                    ++ " seconds and the maximum time was "
                                    ++ String.fromInt ( round (model.statistics.maximumTime / 1000) )
                                    ++ " seconds."
                                )
                            ]
                        ]
                    )
                ]
            ]
        )
        Nothing
