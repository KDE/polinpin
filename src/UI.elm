module UI exposing (button, card, destructiveButton, dialog, edges, focus, fontScaled, inputStyles, label, labelScaled, scaled, scaledInt, separator, subToolbar, tab, textField, with, withScrim, subduedButton)

import Browser.Navigation
import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Elements.Header
import Gen.Route as Route exposing (Route)
import Request
import Shared


focus : Attribute msg
focus =
    Element.focused
        [ Border.color <| rgb255 0x3D 0xAE 0xE9
        ]


inputStyles : List (Attribute msg)
inputStyles =
    [ focus, Border.width 4, Border.rounded 0, Border.color <| rgb255 0 0 0 ]


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
    el (Font.size (scaled 0 |> round) :: attrs) (text str)


labelScaled : Int -> String -> Element msg
labelScaled at =
    label [ fontScaled at ]


with : Shared.Model -> List (Element msg) -> Element msg
with shared els =
    column
        [ width fill, height fill ]
        (Elements.Header.view shared :: els)


btn : { disabled : Color, pressed : Color, idle : Color, text : Color } -> Bool -> String -> msg -> Element msg
btn colors enabled textlabel msg =
    Input.button
        [ if enabled then
            Background.color colors.idle

          else
            Background.color colors.disabled
        , Font.color colors.text
        , paddingXY 10 6
        , focus
        , Border.color <| rgb255 0 0 0
        , Border.width 4
        , Element.mouseDown [ Background.color colors.pressed ]
        ]
        { onPress =
            if enabled then
                Just msg

            else
                Nothing
        , label = el [ centerX ] (text textlabel)
        }


button : Bool -> String -> msg -> Element msg
button =
    btn { idle = rgb255 0xFF 0xE2 0x47, text = rgb255 0 0 0, pressed = rgb255 0xC4 0xAB 0x00, disabled = rgb255 0xE8 0xCB 0x2D }

subduedButton : Bool -> String -> msg -> Element msg
subduedButton =
    btn { disabled = rgb255 0x99 0x99 0x99, idle = rgb255 0xdd 0xdd 0xdd, text = rgb255 0 0 0, pressed = rgb255 0xcc 0xcc 0xcc}

destructiveButton : Bool -> String -> msg -> Element msg
destructiveButton =
    btn { idle = rgb255 0xE9 0x3D 0x58, text = rgb255 255 255 255, pressed = rgb255 0x99 0x00 0x2E, disabled = rgb255 100 100 100 }


subToolbar : List (Element msg) -> Element msg
subToolbar =
    row
        [ padding 24
        , Background.color <| rgb255 0xEE 0xEE 0xEE
        , Font.color <| rgb255 0 0 0
        , width fill
        ]


withScrim : Element msg -> Element msg
withScrim child =
    el [ width fill, height fill, Background.color <| rgba255 0x00 0x00 0x00 0.4 ] child


dialog : Element msg -> Element msg
dialog child =
    el
        [ paddingXY 20 16
        , Background.color <| rgb255 255 255 255
        , Border.width 4
        , Border.color <| rgb255 0 0 0
        , centerX
        , centerY
        ]
        child


textField : String -> (String -> msg) -> String -> Element msg
textField currentText onChange textLabel =
    Input.username
        inputStyles
        { onChange = onChange
        , text = currentText
        , placeholder = Nothing
        , label = Input.labelAbove [ fontScaled 1 ] (text textLabel)
        }


card : List (Attribute msg) -> Element msg -> Element msg
card attrs =
    el ([ padding 16, Border.color <| rgb255 0x00 0x00 0x00, Border.width 4 ] ++ attrs)


separator : List (Attribute msg) -> Element msg
separator attrs =
    el ([ Background.color <| rgb255 0x00 0x00 0x00, width (px 4), height (px 4) ] ++ attrs) none


tab : String -> Bool -> msg -> Element msg
tab txt active onClicked =
    Input.button
        [ paddingXY 10 8
        , Background.color <|
            if active then
                rgb255 0xAA 0xAA 0xAA

            else
                rgb255 0xEE 0xEE 0xEE
        , onClick onClicked
        , pointer
        , focus
        , Border.color <| rgb255 0x00 0x00 0x00
        , Border.widthEach { bottom = 4, left = 4, right = 4, top = 0 }
        ]
        { onPress = Just onClicked
        , label = text txt
        }


edges : { left : number, right : number, bottom : number, top : number }
edges =
    { left = 0
    , right = 0
    , bottom = 0
    , top = 0
    }
