port module Drag exposing (Coordinates, DragMsg(..), DraggedNode, beacon, beaconAttr, closestBeacon, dragMsgDecoder, onDragStart, subDragEvents)

import Element
import Html.Attributes
import Html.Events as Events
import Json.Decode as D
import Json.Encode as E


port dragEvents : (D.Value -> msg) -> Sub msg


collapse : (a -> c) -> (b -> c) -> Result b a -> c
collapse fa fb result =
    case result of
        Ok v ->
            fa v

        Err v ->
            fb v


subDragEvents : D.Decoder b -> (DragMsg a b -> msg) -> Sub msg
subDragEvents beaconDecoder msg =
    dragEvents (\k -> D.decodeValue (dragMsgDecoder beaconDecoder) k |> collapse identity Invalid |> msg)


type DragMsg dragStart beacon
    = Start (DraggedNode dragStart)
    | Move Coordinates (List (Beacon beacon))
    | Stop
    | Invalid D.Error


distance : Coordinates -> Coordinates -> Float
distance coords1 coords2 =
    let
        dx =
            coords1.x - coords2.x

        dy =
            coords1.y - coords2.y
    in
    sqrt ((dx ^ 2) + (dy ^ 2))


center : Rectangle -> Coordinates
center { x, y, width, height } =
    { x = x + (width / 2.0)
    , y = y + (height / 2.0)
    }


closestBeacon : Coordinates -> List (Beacon beacon) -> Maybe beacon
closestBeacon coordinates beacons =
    beacons
        |> List.map (Tuple.mapSecond (distance coordinates << center))
        |> List.sortBy Tuple.second
        |> List.head
        |> Maybe.map Tuple.first


dragMsgDecoder : D.Decoder b -> D.Decoder (DragMsg a b)
dragMsgDecoder beaconDecoder =
    D.oneOf
        [ D.field "type" (expectString "move")
            |> D.andThen
                (\_ ->
                    D.map2 Move
                        (D.field "cursor" coordinateDecoder)
                        (D.field "beacons" (D.list (D.map2 Tuple.pair (D.field "data" beaconDecoder) rectDecoder)))
                )
        , D.field "type" (expectString "stop")
            |> D.andThen (\_ -> D.succeed Stop)
        ]


expectString : String -> D.Decoder ()
expectString str =
    D.string
        |> D.andThen
            (\k ->
                if k == str then
                    D.succeed ()

                else
                    D.fail ("Expecting " ++ str)
            )


type alias Beacon a =
    ( a, Rectangle )


type alias Rectangle =
    { x : Float
    , y : Float
    , width : Float
    , height : Float
    }


type alias Coordinates =
    { x : Float
    , y : Float
    }


type alias DraggedNode a =
    ( a
    , { cursorOnScreen : Coordinates
      , cursorOnDraggable : Coordinates
      }
    )


onDragStart : (DragMsg a b -> msg) -> a -> Element.Attribute msg
onDragStart msg fn =
    Events.on "pointerdown"
        (D.map2
            (\cursorOnScreen cursorOffset ->
                (Start <| ( fn, { cursorOnScreen = cursorOnScreen, cursorOnDraggable = cursorOffset } ))
                    |> msg
            )
            cursorPositionDecoder
            cursorOffsetDecoder
        )
        |> Element.htmlAttribute


beaconAttr : E.Value -> Element.Attribute msg
beaconAttr data =
    Html.Attributes.attribute "data-beacon" (E.encode 0 data)
        |> Element.htmlAttribute


beacon : E.Value -> List (Element.Attribute msg) -> Element.Element msg
beacon data attrs =
    Element.el (beaconAttr data :: attrs) Element.none


coordinateDecoder : D.Decoder Coordinates
coordinateDecoder =
    D.map2 Coordinates
        (D.field "x" D.float)
        (D.field "y" D.float)


cursorPositionDecoder : D.Decoder Coordinates
cursorPositionDecoder =
    D.map2 Coordinates
        (D.field "clientX" D.float)
        (D.field "clientY" D.float)


cursorOffsetDecoder : D.Decoder Coordinates
cursorOffsetDecoder =
    D.map2 Coordinates
        (D.field "offsetX" D.float)
        (D.field "offsetY" D.float)


rectDecoder : D.Decoder Rectangle
rectDecoder =
    D.map4
        Rectangle
        (D.field "x" D.float)
        (D.field "y" D.float)
        (D.field "width" D.float)
        (D.field "height" D.float)
