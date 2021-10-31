module Pages.Login exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Login exposing (Params)
import Page
import Request
import Shared
import UI
import View exposing (View)
import Http
import Web
import Json.Decode as D
import Json.Encode as E
import HTTPExt
import Storage


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
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
    , createName : String
    , createUsername : String
    , createPassword : String
    , error : Maybe Http.Error
    }


init : ( Model, Effect Msg )
init =
    ( Model "" "" "" "" "" Nothing, Effect.none )



-- UPDATE


type Msg
    = SetUsername String
    | SetPassword String
    | SetCreateName String
    | SetCreateUsername String
    | SetCreatePassword String
    | Login
    | Register
    | GotAccount (Result Http.Error Shared.User)

encodeLoginRequest : String -> String -> E.Value
encodeLoginRequest username password =
    E.object
        [ ("username", E.string username)
        , ("password", E.string password)
        ]

encodeRegisterRequest : String -> String -> String -> E.Value
encodeRegisterRequest name username password =
    E.object
        [ ("username", E.string username)
        , ("password", E.string password)
        , ("name", E.string name)
        ]

login : String -> String -> Cmd Msg
login username password =
    Web.post
        { url = Web.host ++ "/login"
        , body = Http.jsonBody (encodeLoginRequest username password)
        , expect = Http.expectJson GotAccount Storage.userDecoder
        , headers = []
        }

register : String -> String -> String -> Cmd Msg
register name username password =
    Web.post
        { url = Web.host ++ "/login"
        , body = Http.jsonBody (encodeRegisterRequest name username password)
        , expect = Http.expectJson GotAccount Storage.userDecoder
        , headers = []
        }

update : Msg -> Model -> ( Model, Effect Msg )
update msg model =
    case msg of
        SetUsername username ->
            ( { model | username = username }, Effect.none )

        SetPassword password ->
            ( { model | password = password }, Effect.none )

        SetCreateName name ->
            ( { model | createName = name }, Effect.none )

        SetCreateUsername username ->
            ( { model | createUsername = username }, Effect.none )

        SetCreatePassword password ->
            ( { model | createPassword = password }, Effect.none )

        Login ->
            ( model, Effect.fromCmd (login model.username model.password))

        Register ->
            ( model, Effect.fromCmd (register model.createName model.createUsername model.createPassword))

        GotAccount (Ok user) ->
            ( { model | error = Nothing }, Effect.fromShared (Shared.SignIn user) )

        GotAccount (Err err) ->
            ( { model | error = Just err }, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


textField : String -> (String -> Msg) -> String -> Element Msg
textField currentText onChange label =
    Input.username
        []
        { onChange = onChange
        , text = currentText
        , placeholder = Nothing
        , label = Input.labelAbove [ UI.fontScaled 1 ] (text label)
        }

passwordField : String -> (String -> Msg) -> String -> Element Msg
passwordField currentText onChange label =
    Input.currentPassword
        []
        { onChange = onChange
        , text = currentText
        , placeholder = Nothing
        , label = Input.labelAbove [ UI.fontScaled 1 ] (text label)
        , show = False
        }

newPasswordField : String -> (String -> Msg) -> String -> Element Msg
newPasswordField currentText onChange label =
    Input.newPassword
        []
        { onChange = onChange
        , text = currentText
        , placeholder = Nothing
        , label = Input.labelAbove [ UI.fontScaled 1 ] (text label)
        , show = False
        }

viewLogin : Model -> Element Msg
viewLogin model =
    column [ width (px 300), padding 24, spacing (UI.scaledInt 2), alignTop ]
        [ UI.labelScaled 4 "Sign in"
        , textField model.username SetUsername "Username"
        , passwordField model.password SetPassword "Password"
        , el [ alignRight ] (UI.button True "Login" Login)
        ]

viewRegister : Model -> Element Msg
viewRegister model =
    column [ width (px 300), padding 24, spacing (UI.scaledInt 2), alignTop ]
        [ UI.labelScaled 4 "Create an Account"
        , textField model.createName SetCreateName "Name"
        , textField model.createUsername SetCreateUsername "Username"
        , newPasswordField model.createPassword SetCreatePassword "New Password"
        , el [ alignRight ] (UI.button True "Register" Register)
        ]

view : Shared.Model -> Model -> View Msg
view shared model =
    View
        "Login" <|
        UI.with shared
            [ row
                [ padding 24, spacing (UI.scaledInt 2), centerX ]
                [ viewLogin model, viewRegister model ]
            , case model.error of
                Just it ->
                    UI.label [ centerX ] (HTTPExt.errorToString it)

                Nothing ->
                    none
            ]
