module SharedUI exposing (..)

import Element exposing (..)
import Gen.Route
import Shared
import UI
import View exposing (View)
import Network


sharedFrame : Shared.Model -> View msg -> View msg
sharedFrame shared view =
    { title = view.title
    , body =
        column [ spacing 10, width fill, height fill ]
            [ header view.title shared, view.body ]
    , over = view.over
    }


shadedRow : List (Attribute msg) -> List (Element msg) -> Element msg
shadedRow attrs =
    row ([ padding 10, behindContent (UI.grayBox [ width fill, height fill ] none) ] ++ attrs)


header : String -> Shared.Model -> Element msg
header title shared =
    row [ padding 10, width fill ]
        [ shadedRow [ alignLeft ]
            [ UI.link [] { url = "/", label = text "Polinpin" }
            ]
        , el [ centerX ] (text title)
        , shadedRow [ alignRight, spacing 20 ]
            (case shared.user of
                Just _ ->
                    [ UI.link [] { url = Gen.Route.toHref Gen.Route.MyStudies, label = text "My Studies" }
                    ]

                Nothing ->
                    [ UI.link [] { url = Gen.Route.toHref Gen.Route.Auth__Login, label = text "Login" }
                    , UI.link [] { url = Gen.Route.toHref Gen.Route.Auth__Register, label = text "Register" }
                    ]
            )
        ]

type NameKind
    = TitleCase

kindToString : Network.StudyKind -> NameKind -> String
kindToString kind nameKind =
    case (kind, nameKind) of
        (Network.TreeTest, TitleCase) ->
            "Tree Test"

        (Network.DesirabilityTest, TitleCase) ->
            "Desirability Study"


tabButton : a -> a -> (a -> msg) -> String -> Element msg
tabButton currentTab forTab msg text =
    (if currentTab == forTab then
        UI.darkTextButton

     else
        UI.textButton
    )
        (Just (msg forTab))
        []
        text