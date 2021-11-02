module Pages.Statistics.TreeTest.Test_ exposing (Model, Msg, page)

import Gen.Params.Statistics.TreeTest.Test_ exposing (Params)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Page
import Request
import Shared
import View exposing (View)
import Http
import TreeTest
import HTTPExt


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.protected.element
        (\user -> { init = init user req.params.test
        , update = update
        , view = view
        , subscriptions = subscriptions
        })



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
    case (msg, model) of
        (FinishedLoading (Ok stats), Loading) ->
            ( Loaded (LoadedModel stats), Cmd.none )

        (FinishedLoading (Err error), Loading) ->
            ( LoadFailed error, Cmd.none )

        (_, Loaded loadedModel) ->
            let
                (new, cmd) =
                    updateLoaded msg loadedModel
            in
            (Loaded new, cmd)

        (_, LoadFailed _) ->
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


view : Model -> View Msg
view model =
    case model of
        Loading ->
            View
                "Loading" none Nothing

        Loaded loaded ->
            viewLoaded loaded

        LoadFailed err ->
            View "Loading failed" (text (HTTPExt.errorToString err)) Nothing

viewLoaded : LoadedModel -> View Msg
viewLoaded model =
    View
        "Loaded"
        (text (Debug.toString model))
        Nothing