module Elements.Header exposing (view)

import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Gen.Route as Route exposing (Route)
import Shared


viewLink : String -> Route -> Element msg
viewLink label route =
    link
        [ Font.color (rgb255 0x30 0x6F 0x91)
        , alignRight
        ]
        { url = Route.toHref route
        , label = text label
        }


view : Shared.Model -> Element msg
view shared =
    row
        [ width fill
        , Background.color (rgb255 0xDD 0xDD 0xDD)
        , Font.color (rgb255 0 0 0)
        , padding 12
        , spacing 4
        ]
        [ el
            [ alignLeft ]
            (text "polinpin")
        , row
            [ alignRight, spacing 20 ]
            (viewLink "home" Route.Home_
                :: (case shared.storage.user of
                        Just _ ->
                            [ viewLink "my tree tests" Route.My__TreeTests
                            ]

                        Nothing ->
                            [ viewLink "login" Route.Login
                            ]
                   )
            )
        ]
