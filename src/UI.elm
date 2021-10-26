module UI exposing (button, viewLink, with)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Elements.Header
import Gen.Route as Route exposing (Route)
import Shared


with : Shared.Model -> List (Element msg) -> Element msg
with shared els =
    column
        [ width fill, height fill ]
        ( Elements.Header.view shared :: els)


viewLink : String -> Route -> Element msg
viewLink label route =
    link
        [ Font.color (rgb255 0 0 255)
        ]
        { url = Route.toHref route
        , label = text label
        }


button : Bool -> String -> msg -> Element msg
button enabled label msg =
    Input.button
        [ if enabled then
            Background.color <| rgb255 0 0 0
        else
            Background.color <| rgb255 100 100 100
        , Font.color <| rgb255 255 255 255
        , padding 5
        , Element.focused
            [ Background.color <| rgb255 10 10 10 ]
        , Element.mouseDown
            [ Background.color <| rgb255 100 100 200 ]
        ]
        { onPress = Just msg
        , label = el [ centerX ] (text label)
        }
