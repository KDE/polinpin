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
import Browser.Events
import Browser.Dom
import Task


type alias Flags =
    Json.Value


type alias Model =
    { storage : Storage.Storage
    , dimensions : { width : Int , height : Int }
    }


type alias User =
    Storage.User


type Msg
    = SignIn User
    | SignOut
    | DimensionsChanged Int Int


init : Request -> Flags -> ( Model, Cmd Msg )
init _ flags =
    ( Model (Storage.fromJson flags) { width = -1, height = -1}, Task.perform (\it -> DimensionsChanged (round it.viewport.width) (round it.viewport.height)) Browser.Dom.getViewport )


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

        DimensionsChanged w h ->
            ( { model | dimensions = { width = w, height = h } }, Cmd.none )


subscriptions : Request -> Model -> Sub Msg
subscriptions _ _ =
    Browser.Events.onResize (\x y -> DimensionsChanged x y)
