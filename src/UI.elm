module UI exposing (button, viewLink, with, destructiveButton)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Elements.Header
import Gen.Route as Route exposing (Route)
import Shared


with : Shared.Model -> List (Element msg) -> Element msg
with shared els =
    column
        [ width fill, height fill ]
        (Elements.Header.view shared :: els)


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
            Background.color <| rgb255 0xFF 0xE2 0x47

          else
            Background.color <| rgb255 100 100 100
        , Font.color <| rgb255 0 0 0
        , paddingXY 10 6
        , Border.rounded 30
        , Element.focused
            [ Background.color <| rgb255 0xE8 0xCB 0x2D ]
        , Element.mouseDown
            [ Background.color <| rgb255 0xC4 0xAB 0x00 ]
        ]
        { onPress = Just msg
        , label = el [ centerX ] (text label)
        }

destructiveButton : Bool -> String -> msg -> Element msg
destructiveButton enabled label msg =
    Input.button
        [ if enabled then
            Background.color <| rgb255 0xe9 0x3d 0x58

          else
            Background.color <| rgb255 100 100 100
        , Font.color <| rgb255 255 255 255
        , paddingXY 10 6
        , Border.rounded 30
        , Element.focused
            [ Background.color <| rgb255 0xbf 0x00 0x39 ]
        , Element.mouseDown
            [ Background.color <| rgb255 0x99 0x00 0x2e ]
        ]
        { onPress = Just msg
        , label = el [ centerX ] (text label)
        }
