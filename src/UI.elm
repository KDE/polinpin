module UI exposing (button, destructiveButton, fontScaled, label, labelScaled, scaled, scaledInt, subToolbar, viewLink, with)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Elements.Header
import Gen.Route as Route exposing (Route)
import Shared


scaled : Int -> Float
scaled =
    Element.modular 16 1.25


scaledInt : Int -> Int
scaledInt at =
    round <| scaled at


fontScaled : Int -> Attribute msg
fontScaled at =
    Font.size (scaled at |> round)


label : List (Attribute msg) -> String -> Element msg
label attrs str =
    el attrs (text str)


labelScaled : Int -> String -> Element msg
labelScaled at =
    label [ fontScaled at ]


with : Shared.Model -> List (Element msg) -> Element msg
with shared els =
    column
        [ width fill, height fill ]
        (Elements.Header.view shared :: els)


viewLink : String -> Route -> Element msg
viewLink textLabel route =
    link
        [ Font.color (rgb255 0 0 255)
        ]
        { url = Route.toHref route
        , label = text textLabel
        }


button : Bool -> String -> msg -> Element msg
button enabled textLabel msg =
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
        , label = el [ centerX ] (text textLabel)
        }


destructiveButton : Bool -> String -> msg -> Element msg
destructiveButton enabled textLabel msg =
    Input.button
        [ if enabled then
            Background.color <| rgb255 0xE9 0x3D 0x58

          else
            Background.color <| rgb255 100 100 100
        , Font.color <| rgb255 255 255 255
        , paddingXY 10 6
        , Border.rounded 30
        , Element.focused
            [ Background.color <| rgb255 0xBF 0x00 0x39 ]
        , Element.mouseDown
            [ Background.color <| rgb255 0x99 0x00 0x2E ]
        ]
        { onPress = Just msg
        , label = el [ centerX ] (text textLabel)
        }


subToolbar : List (Element msg) -> Element msg
subToolbar =
    row
        [ padding 24
        , Background.color <| rgb255 0x03 0x66 0x88
        , Font.color <| rgb255 255 255 255
        , width fill
        ]
