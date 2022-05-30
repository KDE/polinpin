module Pages.MyStudies exposing (Model, Msg, page)

import Auth
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Input as Input
import Gen.Params.MyStudies exposing (Params)
import Gen.Route
import Http
import Network
import Page
import Request
import Shared
import SharedUI
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.protected.advanced
        (\user ->
            { init = init user
            , update = update user req
            , view = view shared
            , subscriptions = subscriptions
            }
        )



-- INIT


type alias CreatingDialog =
    { kind : Maybe Network.StudyKind
    , name : String
    , outgoing : Network.RequestStatus String
    }


type alias Model =
    { studies : Maybe (Result Http.Error (List Network.StudyData))
    , creatingDialog : CreatingDialog
    }


init : Auth.User -> ( Model, Effect Msg )
init user =
    ( Model Nothing (CreatingDialog Nothing "" Network.NoRequest)
    , Effect.fromCmd <|
        Network.myTreeTests (Network.UserSession user.token) StudiesObtained
    )



-- UPDATE


type Msg
    = StudiesObtained (Result Http.Error (List Network.StudyData))
    | OpenCreatingDialog Network.StudyKind
    | CloseCreatingDialog
    | SetCreatingName String
    | CreateStudy Network.StudyKind
    | CreateStudyObtained (Result Http.Error String)
    | OpenStudy String


update : Auth.User -> Request.With Params -> Msg -> Model -> ( Model, Effect Msg )
update user req msg model =
    case msg of
        StudiesObtained res ->
            ( { model | studies = Just res }, Effect.none )

        OpenCreatingDialog kind ->
            let
                c =
                    model.creatingDialog
            in
            ( { model | creatingDialog = { c | kind = Just kind } }, Effect.none )

        CloseCreatingDialog ->
            ( { model | creatingDialog = CreatingDialog Nothing "" Network.NoRequest }, Effect.none )

        SetCreatingName name ->
            let
                c =
                    model.creatingDialog
            in
            ( { model | creatingDialog = { c | name = name } }, Effect.none )

        CreateStudy kind ->
            case kind of
                Network.TreeTest ->
                    ( let
                        c =
                            model.creatingDialog
                      in
                      { model | creatingDialog = { c | outgoing = Network.PendingRequest } }
                    , Effect.fromCmd <|
                        Network.newTreeTest
                            (Network.UserSession user.token)
                            user.username
                            model.creatingDialog.name
                            CreateStudyObtained
                    )

        CreateStudyObtained ((Ok slug) as result) ->
            ( let
                c =
                    model.creatingDialog
              in
              { model | creatingDialog = { c | outgoing = Network.ResultReceived result } }
            , Effect.fromCmd <|
                Request.pushRoute
                    (Gen.Route.TreeTests__User___Slug___Edit { user = user.username, slug = slug })
                    req
            )

        CreateStudyObtained ((Err _) as result) ->
            ( let
                c =
                    model.creatingDialog
              in
              { model | creatingDialog = { c | outgoing = Network.ResultReceived result } }
            , Effect.none
            )

        OpenStudy slug ->
            ( model
            , Effect.fromCmd <|
                Request.pushRoute
                    (Gen.Route.TreeTests__User___Slug___Edit { user = user.username, slug = slug })
                    req
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


loadingView : Element Msg
loadingView =
    el [ centerX ] (text "Loading studies...")


errorView : Http.Error -> List (Element Msg)
errorView why =
    [ el [ centerX ] (text "Error loading studies") ]


noStudiesView : List (Element Msg)
noStudiesView =
    [ paragraph [ centerX ] [ text "You haven't created any studies yet. Why not create one?" ] ]


viewStudy : Network.StudyData -> Element Msg
viewStudy data =
    row
        [ width fill, paddingXY 0 8 ]
        [ column [ alignLeft ] 
            [ text data.title
            , UI.sizedLabel -1 [] (SharedUI.kindToString data.kind SharedUI.TitleCase)
            ]
        , UI.textButton (Just (OpenStudy data.slug)) [ alignRight ] "View"
        ]


studiesView : List Network.StudyData -> List (Element Msg)
studiesView studies =
    List.map viewStudy studies
        |> List.intersperse (UI.blackLine [ width fill ])


loadedView : List Network.StudyData -> List (Element Msg)
loadedView studies =
    case List.length studies of
        0 ->
            noStudiesView

        _ ->
            studiesView studies


creatingDialog : CreatingDialog -> Network.StudyKind -> Element Msg
creatingDialog model kind =
    column
        [ centerX
        , centerY
        , width (fill |> maximum 400)
        , Background.color <| rgb255 225 221 210
        , behindContent (UI.grayBox [ width fill, height fill ] none)
        , padding 20
        , spacing 20
        ]
        [ row [ width fill ]
            [ text ("Create A " ++ SharedUI.kindToString kind SharedUI.TitleCase)
            , UI.textButton (Just CloseCreatingDialog) [ alignRight ] "Close"
            ]
        , UI.textInput []
            { onChange = SetCreatingName
            , text = model.name
            , placeholder = Nothing
            , label = Input.labelAbove [] (text "Study Name")
            }
        , case model.outgoing of
            Network.NoRequest ->
                UI.textButton (Just (CreateStudy kind)) [ width fill ] "Create"

            Network.PendingRequest ->
                UI.textButton Nothing [ width fill ] "Creating..."

            Network.ResultReceived (Ok _) ->
                UI.textButton Nothing [ width fill ] "Created!"

            Network.ResultReceived (Err _) ->
                UI.textButton (Just (CreateStudy kind)) [ width fill ] "Create failed :/"
        ]


view : Shared.Model -> Model -> View Msg
view shared model =
    SharedUI.sharedFrame shared
        { title = "My Studies"
        , body =
            el [ width fill, height fill ] <|
                column
                    [ centerX
                    , centerY
                    , width (fill |> maximum 700)
                    , behindContent (UI.grayBox [ width fill, height fill ] none)
                    , padding 20
                    , spacing 20
                    ]
                    ((case model.studies of
                        Just (Ok studies) ->
                            loadedView studies

                        Just (Err why) ->
                            errorView why

                        Nothing ->
                            [ loadingView ]
                     )
                        ++ [ UI.textButton (Just (OpenCreatingDialog Network.TreeTest)) [ width fill ] "Create A Tree Test"
                           ]
                    )
        , over = Maybe.map (creatingDialog model.creatingDialog) model.creatingDialog.kind
        }
