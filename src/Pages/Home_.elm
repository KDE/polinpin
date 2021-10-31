module Pages.Home_ exposing (page)

import Element exposing (..)
import Page exposing (Page)
import Request exposing (Request)
import Shared
import UI
import View exposing (View)


page : Shared.Model -> Request -> Page
page shared _ =
    Page.static
        { view = view shared
        }


loggedInView : Shared.User -> Element msg
loggedInView user =
    UI.subToolbar
        [ UI.labelScaled 2 ("Welcome back, " ++ user.name ++ "!")
        ]


notLoggedInView : Element msg
notLoggedInView =
    UI.subToolbar
        [ UI.labelScaled 2 "The open source UX research site."
        ]


view : Shared.Model -> View msg
view shared =
    { title = "Homepage"
    , element =
        UI.with shared
            [ case shared.storage.user of
                Just user ->
                    loggedInView user

                Nothing ->
                    notLoggedInView
            ]
    }
