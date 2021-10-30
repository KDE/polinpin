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

type ActiveTab
    = EditTree
    | EditTasks

type alias LoadedModel =
    { study : Study
    , count : Int
    , activeTab : ActiveTab
    }

type alias Study =
    { tree : Node
    , name : String
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
    let
        defaultStudy =
            { tree = Node (Item "hi" "hoi?") []
            , name = "Example Treetest Study"
            }
    in
    ( Loaded (LoadedModel defaultStudy 0 EditTree), Cmd.none )



-- UPDATE


type Msg
    = EditItem String String
    | NewNode String
    | TabClicked ActiveTab


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

setRootNode : LoadedModel -> Node -> LoadedModel
setRootNode model node =
    let
        study
            = model.study
    in
    { model | study = { study | tree = node }}

incrementCount : LoadedModel -> LoadedModel
incrementCount model =
    { model | count = model.count + 1 }

updateLoaded : Msg -> LoadedModel -> ( LoadedModel, Cmd Msg )
updateLoaded msg model =
    case msg of
        EditItem id cont ->
            ( setRootNode model <| mapByID id (\it -> { it | text = cont }) model.study.tree, Cmd.none )

        NewNode id ->
            ( (setRootNode model <| appendByID id (Node (Item (String.fromInt model.count) "") []) model.study.tree)
              |> incrementCount, Cmd.none )

        TabClicked id ->
            ( { model | activeTab = id }, Cmd.none )



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
            [ column [ width fill, spacing 0 ]
                [ viewHeader model
                , viewTabs model
                , el [ padding 16 ] (viewNode model.study.tree)
                ]
            ]
        )

tab : String -> Bool -> Msg -> Element Msg
tab label active onClicked =
    el
        [ paddingXY 10 8
        , Border.roundEach { bottomLeft = 4, bottomRight = 4, topLeft = 0, topRight = 0}
        , Background.color <| if active then
            rgb255 0x03 0x66 0x88
        else
            rgb255 0xd1 0xd5 0xd9
        , Font.color <| if active then
            rgb255 0xff 0xff 0xff
        else
            rgb255 0x00 0x00 0x00
        , onClick onClicked
        , pointer
        ]
        (text label)

viewTabs : LoadedModel -> Element Msg
viewTabs model =
    let
        make label role =
            tab label (model.activeTab == role) (TabClicked role)

        roles =
            [ make "Tree" EditTree
            , make "Tasks" EditTasks
            ]
    in
    row [ paddingEach { edges | left = 8 }, spacing 6 ]
        roles

viewHeader : LoadedModel -> Element Msg
viewHeader model =
    row
        [ padding 24
        , Background.color <| rgb255 0x03 0x66 0x88
        , Font.color <| rgb255 255 255 255
        , width fill
        ]
        [ text <| "Editing " ++ model.study.name
        ]

edges : { left : number, top : number, bottom : number, right : number }
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
