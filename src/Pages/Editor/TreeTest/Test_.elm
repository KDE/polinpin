module Pages.Editor.TreeTest.Test_ exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Editor.TreeTest.Test_ exposing (Params)
import Page
import Request
import Shared
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- MODEL


type Model
    = Loaded LoadedModel
    | Loading
    | Failed


type alias LoadedModel =
    { rootNode : Node
    , count : Int
    }


type Node
    = Node Item (List Node)


type alias Item =
    { id : String
    , text : String
    }


map : (Item -> Item) -> Node -> Node
map f (Node item children) =
    Node (f item) (List.map (map f) children)


updateByID : String -> (Item -> Item) -> Item -> Item
updateByID id f item =
    if item.id == id then
        f item

    else
        item


mapByID : String -> (Item -> Item) -> Node -> Node
mapByID id f node =
    map (updateByID id f) node


appendByID : String -> Node -> Node -> Node
appendByID id new replaceIn =
    let
        f =
            \(Node n children) ->
                if n.id == id then
                    Node n (children ++ [ new ])

                else
                    Node n children

        (Node nid replaced) =
            f replaceIn
    in
    Node nid (List.map (appendByID id new) replaced)



-- INIT


init : ( Model, Cmd Msg )
init =
    ( Loaded (LoadedModel (Node (Item "hi" "hoi?") []) 0), Cmd.none )



-- UPDATE


type Msg
    = EditItem String String
    | NewNode String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( msg, model ) of
        ( _, Loaded m ) ->
            let
                ( new, cmd ) =
                    updateLoaded msg m
            in
            ( Loaded new, cmd )

        _ ->
            ( model, Cmd.none )


updateLoaded : Msg -> LoadedModel -> ( LoadedModel, Cmd Msg )
updateLoaded msg model =
    case msg of
        EditItem id cont ->
            ( { model | rootNode = mapByID id (\it -> { it | text = cont }) model.rootNode }, Cmd.none )

        NewNode id ->
            ( { model | rootNode = appendByID id (Node (Item (String.fromInt model.count) "") []) model.rootNode, count = model.count + 1 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    case model of
        Loaded m ->
            viewLoaded shared m

        _ ->
            View.placeholder "not loaded"


viewLoaded : Shared.Model -> LoadedModel -> View Msg
viewLoaded shared model =
    View "Editing..."
        (UI.with shared
            [ column [ width fill, padding 16, spacing 24 ]
                [ viewNode model.rootNode ]
            ]
        )


edges =
    { left = 0, top = 0, bottom = 0, right = 0 }


viewNode : Node -> Element Msg
viewNode (Node it children) =
    let
        isEmpty =
            List.length children == 0
    in
    column
        [ paddingEach { edges | left = 20 }
        , Border.color <| rgb255 0xD1 0xD5 0xD9
        , Border.widthEach { edges | left = 2 }
        , Border.dotted
        , spacing 8
        ]
        (row [ spacing 4 ]
            [ Input.text []
                { onChange = \str -> EditItem it.id str
                , text = it.text
                , placeholder = Nothing
                , label = Input.labelHidden "node name"
                }
            , if isEmpty then UI.button True "add children" (NewNode it.id) else none
            ]
            :: (List.map viewNode children)
            ++ [if not isEmpty then UI.button True "+" (NewNode it.id) else none]
        )
