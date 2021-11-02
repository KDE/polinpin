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
            , view = view user shared
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
    , dialogOpen : Bool
    , currentStudyName : String
    , creationStatus : CreationStatus
    }


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
    | DoCreate
    | ChangeCurrentStudyName String
    | TestMade (Result Http.Error String)


update : Shared.User -> Msg -> Model -> ( Model, Cmd Msg )
update user msg model =
    case ( msg, model ) of
        ( MyTreeTestsLoaded (Ok tests), _ ) ->
            ( Loaded (LoadedModel tests False "" CreationIdle), Cmd.none )

        ( MyTreeTestsLoaded (Err err), _ ) ->
            ( Failed err, Cmd.none )

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
            ( { model | dialogOpen = True }, Cmd.none )

        CloseDialog ->
            ( { model | dialogOpen = False }, Cmd.none )

        ChangeCurrentStudyName text ->
            ( { model | currentStudyName = text }, Cmd.none )

        DoCreate ->
            ( { model | creationStatus = CreationPending }, TreeTest.createTreeTest user.token model.currentStudyName TestMade )

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


view : Shared.User -> Shared.Model -> Model -> View Msg
view user shared model =
    case model of
        Loaded loaded ->
            viewLoaded user shared loaded

        Loading ->
            viewLoading shared

        Failed error ->
            viewLoadFailed shared error


viewLoaded : Shared.User -> Shared.Model -> LoadedModel -> View Msg
viewLoaded user shared model =
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
                            text <| "Welcome back, " ++ user.name ++ ". Here are your tree tests."

                        else
                            text <| "Welcome back, " ++ user.name ++ ". Ready to create a tree test?"
                , case model.creationStatus of
                    CreationIdle ->
                        el [ alignRight ] (UI.button True "Create Test" CreateTestClicked)

                    CreationPending ->
                        el [ alignRight ] (UI.button False "Creating..." CreateTestClicked)

                    CreationFailed _ ->
                        el [ alignRight ] (UI.destructiveButton True "Creation failed! Try again?" CreateTestClicked)
                ]
            , viewTests model.tests.tests
            ]
        )
        (if model.dialogOpen then
            Just (viewDialog model)

         else
            Nothing
        )


viewTests : List TreeTest.TreeTestOverview -> Element Msg
viewTests tests =
    column [ centerX, padding 16, width (fill |> maximum 800), spacing 16 ]
        (List.map viewTest tests)


viewTest : TreeTest.TreeTestOverview -> Element Msg
viewTest test =
    row
        [ Background.color <| rgb255 0xEE 0xF1 0xF5
        , Border.rounded 8
        , width fill
        , padding 16
        ]
        [ text test.name
        , el [ alignRight ] <| UI.viewLink "edit test" (Gen.Route.Editor__TreeTest__Test_ { test = test.id })
        ]


viewDialog : LoadedModel -> Element Msg
viewDialog model =
    UI.dialog <|
        column [ spacing 8 ]
            [ row [ spacing 20, width fill ] [ UI.labelScaled 2 "Create a new tree test", el [ alignRight ] (UI.button True "Close" CloseDialog) ]
            , UI.textField model.currentStudyName ChangeCurrentStudyName "Study Name"
            , el [ alignRight ] (UI.button True "Create" DoCreate)
            ]


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
