module Pages.Auth.Login exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Home_ exposing (Params)
import Http
import Network
import Page
import Request
import Shared
import SharedUI
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.advanced
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { username : String
    , password : String
    , services : List Network.OAuth2Service
    , normalLoginPossible : Bool
    }


init : ( Model, Effect Msg )
init =
    ( Model "" "" [] True, Effect.fromCmd <| Network.oauth2Services GotServices )



-- UPDATE


type Msg
    = SetUsername String
    | SetPassword String
    | Login
    | LoginResult (Result Http.Error Network.UserSession)
    | GotServices (Result Http.Error Network.OAuth2ServiceConfig)


update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SetUsername username ->
            ( { model | username = username }, Effect.none )

        SetPassword password ->
            ( { model | password = password }, Effect.none )

        Login ->
            ( model, Effect.fromCmd <| Network.login model.username model.password LoginResult )

        LoginResult (Ok session) ->
            ( model, Effect.fromShared <| Shared.signIn (Shared.User model.username model.username session.token) )

        LoginResult (Err _) ->
            ( model, Effect.none )

        GotServices (Ok config) ->
            ( { model | services = config.services, normalLoginPossible = config.normalLoginPossible }, Effect.none )

        GotServices (Err _) ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


loginBox : Model -> Element Msg
loginBox model =
    column [ centerX, centerY, width (fill |> maximum 500), spacing 40 ]
        (column
            [ centerX
            , centerY
            , width (fill |> maximum 500)
            , behindContent (UI.grayBox [ width fill, height fill ] none)
            , padding 20
            , spacing 20
            ]
            (if model.normalLoginPossible then
                [ text "Log Into Polinpin"
                , UI.usernameInput
                    []
                    { onChange = SetUsername
                    , text = model.username
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (text "Username")
                    }
                , UI.currentPasswordInput
                    []
                    { onChange = SetPassword
                    , text = model.password
                    , placeholder = Nothing
                    , label = Input.labelAbove [] (text "Password")
                    , show = False
                    }
                , row [ width fill ]
                    [ link [ Font.color (rgb255 0x32 0x7F 0xA2), Font.underline ] { url = "register", label = text "Register An Account" }
                    , UI.textButton (Just Login) [ alignRight ] "Login"
                    ]
                ]

             else
                [ text "Log Into Polinpin"
                , UI.par "Your Polinpin administrator has disabled username-based login. Please use one of the options below to log in."
                ]
            )
            :: List.map serviceButton model.services
        )


serviceButton : Network.OAuth2Service -> Element msg
serviceButton service =
    UI.linkButton service.url [ width fill ] ("Sign in with " ++ service.name)


view : Shared.Model -> Model -> View Msg
view shared model =
    SharedUI.sharedFrame shared
        { title = "Login"
        , body =
            el [ width fill, height fill ] <|
                loginBox model
        , over = Nothing
        }
