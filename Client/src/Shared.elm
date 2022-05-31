port module Shared exposing
    ( Flags
    , Model
    , Msg
    , User
    , encodeUser
    , init
    , signIn
    , subscriptions
    , update
    , userDecoder
    )

import Http
import Json.Decode as D
import Json.Encode as E
import Network
import Request exposing (Request)



--- MODEL


type alias Flags =
    D.Value


type alias Model =
    { user : Maybe User
    }


type alias User =
    { name : String
    , username : String
    , token : String
    }


encodeUser : User -> E.Value
encodeUser user =
    E.object
        [ ( "username", E.string user.username )
        , ( "name", E.string user.name )
        , ( "token", E.string user.token )
        ]


userDecoder : D.Decoder User
userDecoder =
    D.map3 User
        (D.field "username" D.string)
        (D.field "name" D.string)
        (D.field "token" D.string)


type Msg
    = SignIn User
    | SignOut
    | CheckLoginResult String (Result Http.Error Network.UserInformation)



--- INIT


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    let
        (user, msg) =
            case D.decodeValue userDecoder flags of
                Ok u ->
                    (Just u, Network.me (Network.UserSession u.token) (CheckLoginResult u.token))

                Err _ ->
                    (Nothing, Cmd.none)
    in
    ( Model user, msg )


signIn : User -> Msg
signIn user =
    SignIn user



--- UPDATE


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update _ msg model =
    case msg of
        SignIn user ->
            ( { model | user = Just user }, storeUser (encodeUser user) )

        SignOut ->
            ( model, Cmd.none )

        CheckLoginResult tok (Ok info) ->
            ( { model | user = Just (User info.name info.username tok) }, Cmd.none )

        CheckLoginResult _ (Err _) ->
            ( { model | user = Nothing }, Cmd.none )



--- SUBSCRIPTIONS


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none



--- PORTS


port storeUser : E.Value -> Cmd msg
