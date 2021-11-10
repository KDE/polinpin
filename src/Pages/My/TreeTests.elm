module Pages.My.TreeTests exposing (Model, Msg, page)

import Browser.Navigation as Nav
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Gen.Params.My.TreeTests exposing (Params)
import Gen.Route
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
            { init = init user
            , update = update user
            , view = view user shared req
            , subscriptions = subscriptions
            }
        )



-- INIT


type Model
    = Loading
    | Loaded LoadedModel
    | Failed Http.Error


type alias LoadedModel =
    { tests : TreeTest.MyTreeTests
    , dialogState : DialogState
    , creationStatus : CreationStatus
    }

type DialogState
    = Closed
    | OpenCreate String
    | OpenDelete String String

type CreationStatus
    = CreationIdle
    | CreationPending
    | CreationFailed Http.Error


init : Shared.User -> ( Model, Cmd Msg )
init user =
    ( Loading, TreeTest.myTreeTests user.token MyTreeTestsLoaded )



-- UPDATE


type Msg
    = MyTreeTestsLoaded (Result Http.Error TreeTest.MyTreeTests)
    | TreeTestClicked String
    | CreateTestClicked
    | CloseDialog
    | DoCreate String
    | DoDelete String
    | ChangeCurrentStudyName String
    | TestMade (Result Http.Error String)
    | TestDeleted (Result Http.Error ())
    | Delete String String
    | DoCmd (Cmd Msg)


update : Shared.User -> Msg -> Model -> ( Model, Cmd Msg )
update user msg model =
    case ( msg, model ) of
        ( MyTreeTestsLoaded (Ok tests), _ ) ->
            ( Loaded (LoadedModel tests Closed CreationIdle), Cmd.none )

        ( MyTreeTestsLoaded (Err err), _ ) ->
            ( Failed err, Cmd.none )

        ( DoCmd cmd, _ ) ->
            ( model, cmd )

        ( _, Loaded loaded ) ->
            let
                ( newModel, cmd ) =
                    updateLoaded user msg loaded
            in
            ( Loaded newModel, cmd )

        _ ->
            ( model, Cmd.none )


updateLoaded : Shared.User -> Msg -> LoadedModel -> ( LoadedModel, Cmd Msg )
updateLoaded user msg model =
    case msg of
        TreeTestClicked id ->
            ( model, Nav.load (Gen.Route.toHref (Gen.Route.Editor__TreeTest__Test_ { test = id })) )

        CreateTestClicked ->
            ( { model | dialogState = OpenCreate "" }, Cmd.none )

        CloseDialog ->
            ( { model | dialogState = Closed }, Cmd.none )

        ChangeCurrentStudyName text ->
            ( { model | dialogState = OpenCreate text }, Cmd.none )

        DoCreate name ->
            ( { model | creationStatus = CreationPending }, TreeTest.createTreeTest user.token name TestMade )

        Delete name id ->
            ( { model | dialogState = OpenDelete name id }, Cmd.none )

        DoDelete id ->
            ( model, TreeTest.deleteTreeTest user.token id TestDeleted )

        TestDeleted (Ok id) ->
            ( model, Nav.reload )

        TestDeleted (Err err) ->
            ( model, Nav.reload )

        TestMade (Ok id) ->
            ( model, Nav.load (Gen.Route.toHref (Gen.Route.Editor__TreeTest__Test_ { test = id })) )

        TestMade (Err error) ->
            ( { model | creationStatus = CreationFailed error }, Cmd.none )

        _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.User -> Shared.Model  -> Request.With Params -> Model -> View Msg
view user shared req model =
    case model of
        Loaded loaded ->
            viewLoaded user shared loaded req

        Loading ->
            viewLoading shared

        Failed error ->
            viewLoadFailed shared error


viewLoaded : Shared.User -> Shared.Model -> LoadedModel -> Request.With Params -> View Msg
viewLoaded user shared model req =
    let
        device =
            classifyDevice shared.dimensions
    in
    View
        "My Tree Tests"
        (UI.with shared
            [ UI.subToolbar
                [ case device.class of
                    Phone ->
                        none

                    _ ->
                        if List.length model.tests.tests > 0 then
                            text <| "welcome back, " ++ user.name ++ ". here are your tree tests."

                        else
                            text <| "welcome back, " ++ user.name ++ ". ready to create a tree test?"
                , case model.creationStatus of
                    CreationIdle ->
                        el [ alignRight ] (UI.button True "create test" CreateTestClicked)

                    CreationPending ->
                        el [ alignRight ] (UI.button False "creating..." CreateTestClicked)

                    CreationFailed _ ->
                        el [ alignRight ] (UI.destructiveButton True "creation failed! try again?" CreateTestClicked)
                ]
            , viewTests model.tests.tests req
            ]
        )
        (case model.dialogState of
            Closed ->
                Nothing
                
            _ ->
                Just (viewDialog model model.dialogState))


viewTests : List TreeTest.TreeTestOverview -> Request.With Params -> Element Msg
viewTests tests req =
    column [ centerX, padding 16, width (fill |> maximum 800), spacing 16 ]
        (List.map (viewTest req) tests)


viewTest : Request.With Params -> TreeTest.TreeTestOverview -> Element Msg
viewTest req test =
    row
        [ Border.width 4
        , width fill
        , padding 16
        , spacing 16
        ]
        [ text test.name
        , el [ alignRight ] <| UI.destructiveButton True "delete test" (Delete test.name test.id)
        , el [ alignRight ] <| UI.button True "edit test" (DoCmd (Request.pushRoute (Gen.Route.Editor__TreeTest__Test_ { test = test.id }) req))
        ]


viewDialog : LoadedModel -> DialogState -> Element Msg
viewDialog model state =
    UI.dialog <|
        column [ spacing 16 ]
            (case state of
                OpenCreate name ->
                    [ row [ spacing 20, width fill ] [ UI.labelScaled 2 "create a new tree test", el [ alignRight ] (UI.subduedButton True "close" CloseDialog) ]
                    , UI.textField name ChangeCurrentStudyName "study name"
                    , el [ alignRight ] (UI.button True "create" (DoCreate name))
                    ]

                OpenDelete name id ->
                    [ row [ spacing 20, width fill ] [ UI.labelScaled 2 "delete a tree test", el [ alignRight ] (UI.subduedButton True "close" CloseDialog) ]
                    , text ("are you sure you want to delete " ++ name ++ "?")
                    , el [ alignRight ] (UI.destructiveButton True "delete" (DoDelete id))
                    ]

                _ ->
                    [])


viewLoading : Shared.Model -> View Msg
viewLoading shared =
    View
        "Loading..."
        (UI.with shared
            [ UI.subToolbar
                [ text "Loading tree tests..."
                ]
            ]
        )
        Nothing


viewLoadFailed : Shared.Model -> Http.Error -> View Msg
viewLoadFailed shared error =
    View
        "Loading..."
        (UI.with shared
            [ UI.subToolbar
                [ text "Loading your tree tests failed"
                ]
            , text <| HTTPExt.errorToString error
            ]
        )
        Nothing
