module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Element exposing (Element)
import UI


type alias View msg =
    { title : String
    , element : Element msg
    , over : Maybe (Element msg)
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , element = Element.text str
    , over = Nothing
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , element = Element.map fn view.element
    , over = Maybe.map (Element.map fn) view.over
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body =
        [ Element.layout
            (case view.over of
                Nothing ->
                    []

                Just elm ->
                    [ Element.inFront (UI.withScrim elm) ]
            )
            view.element
        ]
    }
