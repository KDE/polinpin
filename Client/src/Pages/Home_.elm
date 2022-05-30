module Pages.Home_ exposing (Model, Msg, page)

import Element exposing (..)
import Element.Input as Input
import Gen.Params.Home_ exposing (Params)
import Page
import Request
import Shared
import SharedUI
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )



-- UPDATE


type Msg
    = ReplaceMe String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ReplaceMe _ ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared _ =
    SharedUI.sharedFrame shared
        { title = "Homepage"
        , body =
            el [ width fill, height fill ] <|
                column
                    [ centerX
                    , centerY
                    , width (fill |> maximum 500)
                    , behindContent (UI.grayBox [ width fill, height fill ] none)
                    , padding 20
                    , spacing 20
                    ]
                    [ el [ centerX ] (text "Welcome to Polinpin")
                    ]
        , over = Nothing
        }
