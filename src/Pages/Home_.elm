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


view : Shared.Model -> View msg
view shared =
    { title = "Homepage"
    , element =
        UI.with shared
            [ text "hi, welcome to this!"
            ]
    }
