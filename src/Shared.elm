module Shared exposing
    ( Flags
    , Model
    , Msg(..)
    , User
    , init
    , subscriptions
    , update
    )

import Gen.Route
import Json.Decode as Json
import Request exposing (Request)
import Storage


type alias Flags =
    Json.Value


type alias Model =
    { storage : Storage.Storage
    }


type alias User =
    Storage.User


type Msg
    = SignIn User
    | SignOut


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( Model (Storage.fromJson flags), Cmd.none )


update : Request -> Msg -> Model -> ( Model, Cmd Msg )
update req msg model =
    let
        storage =
            model.storage
    in
    case msg of
        SignIn user ->
            let
                newModel =
                    { model | storage = { storage | user = Just user } }
            in
            ( newModel
            , Cmd.batch
                [ Request.pushRoute Gen.Route.Home_ req
                , Storage.saveStorage newModel.storage
                ]
            )

        SignOut ->
            let
                newModel =
                    { model | storage = { storage | user = Nothing } }
            in
            ( newModel
            , Storage.saveStorage model.storage
            )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Sub.none
