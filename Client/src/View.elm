module View exposing (View, map, none, placeholder, toBrowserDocument)

import Browser
import Element exposing (Element)
import Element.Font as Font
import UI


type alias View msg =
    { title : String
    , body : Element msg
    , over : Maybe (Element msg)
    }


placeholder : String -> View msg
placeholder str =
    { title = str
    , body = Element.text str
    , over = Nothing
    }


none : View msg
none =
    placeholder ""


map : (a -> b) -> View a -> View b
map fn view =
    { title = view.title
    , body = Element.map fn view.body
    , over = Maybe.map (Element.map fn) view.over
    }


toBrowserDocument : View msg -> Browser.Document msg
toBrowserDocument view =
    { title = view.title
    , body =
        [ Element.layout
            (Font.family
                [ Font.typeface "Fuzzy Bubbles"
                ]
                :: (case view.over of
                        Nothing ->
                            []

                        Just elm ->
                            [ Element.inFront (UI.withScrim elm) ]
                   )
            )
            view.body
        ]
    }
