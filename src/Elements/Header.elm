module Elements.Header exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Gen.Route as Route exposing (Route)
import Shared


viewLink : String -> Route -> Element msg
viewLink label route =
    link
        [ Font.color (rgb255 0x8A 0xDC 0xFF)
        , alignRight
        ]
        { url = Route.toHref route
        , label = text label
        }


view : Shared.Model -> Element msg
view _ =
    row
        [ width fill
        , Background.color (rgb255 0x00 0x37 0x56)
        , Font.color (rgb255 255 255 255)
        , padding 12
        , spacing 4
        ]
        [ el
            [ alignLeft ]
            (text "the ux thing")
        , row
            [ alignRight, spacing 20 ]
            [ viewLink "home" Route.Home_
            , viewLink "tree test" (Route.TreeTest__Test_ { test = "demo" })
            ]
        ]
