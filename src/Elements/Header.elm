module Elements.Header exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Gen.Route as Route exposing (Route)
import Shared


viewLink : String -> Route -> Element msg
viewLink label route =
    link
        [ Font.color (rgb255 150 150 255)
        , Font.underline
        , alignRight
        ]
        { url = Route.toHref route
        , label = text label
        }


view : Shared.Model -> Element msg
view _ =
    row
        [ width fill
        , Background.color (rgb255 0 0 0)
        , Font.color (rgb255 255 255 255)
        , padding 8
        , spacing 4
        ]
        [ el
            [ alignLeft ]
            (text "the ux thing")
        , viewLink "home" Route.Home_
        , viewLink "tree test" Route.TreeTest
        ]
