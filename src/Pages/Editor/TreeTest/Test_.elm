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
import Tree
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
    , showingTaskTree : Int
    }


type alias Study =
    { tree : Tree.Node Item
    , name : String
    , tasks : List Task
    }


type alias Item =
    { text : String
    }


type alias Task =
    { text : String
    , correctAnswer : Tree.ID
    }



-- INIT


init : ( Model, Cmd Msg )
init =
    let
        defaultStudy =
            { tree = Tree.Node (Tree.ID "hi") (Item "hoi?") []
            , name = "Example Treetest Study"
            , tasks = []
            }
    in
    ( Loaded (LoadedModel defaultStudy 0 EditTree -1), Cmd.none )



-- UPDATE


type Msg
    = EditItem Tree.ID String
    | NewNode Tree.ID
    | TabClicked ActiveTab
    | NewTask
    | EditTaskText Int String
    | EditTaskAnswer Int Tree.ID
    | ShowTaskTree Int


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


setRootNode : LoadedModel -> Tree.Node Item -> LoadedModel
setRootNode model node =
    let
        study =
            model.study
    in
    { model | study = { study | tree = node } }


incrementCount : LoadedModel -> LoadedModel
incrementCount model =
    { model | count = model.count + 1 }


updateIndex : (b -> b) -> Int -> List b -> List b
updateIndex f index list =
    List.indexedMap
        (\idx old ->
            if idx == index then
                f old

            else
                old
        )
        list


updateLoaded : Msg -> LoadedModel -> ( LoadedModel, Cmd Msg )
updateLoaded msg model =
    let
        study =
            model.study
    in
    case msg of
        EditItem id cont ->
            ( setRootNode model <| Tree.mapID id (\it -> { it | text = cont }) model.study.tree, Cmd.none )

        NewNode id ->
            ( (setRootNode model <|
                Tree.appendID id
                    (Tree.makeLeaf
                        (Tree.ID <| String.fromInt model.count)
                        (Item "")
                    )
                    model.study.tree
              )
                |> incrementCount
            , Cmd.none
            )

        TabClicked id ->
            ( { model | activeTab = id }, Cmd.none )

        NewTask ->
            ( { model
                | study =
                    { study
                        | tasks = study.tasks ++ [ Task "" (Tree.ID "") ]
                    }
              }
            , Cmd.none
            )

        EditTaskText idx text ->
            ( { model
                | study =
                    { study
                        | tasks = updateIndex (\task -> { task | text = text }) idx study.tasks
                    }
              }
            , Cmd.none
            )

        EditTaskAnswer idx ans ->
            ( { model
                | study =
                    { study
                        | tasks = updateIndex (\task -> { task | correctAnswer = ans }) idx study.tasks
                    }
                , showingTaskTree = -1
              }
            , Cmd.none
            )

        ShowTaskTree idx ->
            ( { model | showingTaskTree = idx }, Cmd.none )



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
                , el [ padding 16 ]
                    (case model.activeTab of
                        EditTree ->
                            viewNode model.study.tree

                        EditTasks ->
                            viewTasks model
                    )
                ]
            ]
        )


viewTasks : LoadedModel -> Element Msg
viewTasks model =
    column
        [ spacing 8 ]
        (List.indexedMap (viewTask model) model.study.tasks
            ++ [ UI.button True "New Task" NewTask
               ]
        )


viewTask : LoadedModel -> Int -> Task -> Element Msg
viewTask model idx task =
    column
        [ padding 8
        , Background.color <| rgb255 0xEE 0xF1 0xF5
        , Border.color <| rgb255 0x9B 0x9E 0xA2
        , Border.width 1
        , Border.rounded 6
        , width fill
        , spacing 8
        ]
        [ Input.text
            []
            { onChange = EditTaskText idx
            , text = task.text
            , placeholder = Nothing
            , label = Input.labelAbove [ Font.size 16 ] (text "Task text")
            }
        , if model.showingTaskTree == idx then
            viewTaskNode idx task model.study.tree

          else
            UI.button True "Set Correct Answer" (ShowTaskTree idx)
        ]


viewTaskNode : Int -> Task -> Tree.Node Item -> Element Msg
viewTaskNode idx task (Tree.Node nID nData nChildren) =
    column
        [ paddingEach { edges | left = 10 }
        , Border.color <| rgb255 0xD1 0xD5 0xD9
        , Border.widthEach { edges | left = 2 }
        , Border.dotted
        , spacing 8
        ]
        (row [ spacing 4 ]
            [ el
                ([ Background.color
                    (if nID == task.correctAnswer then
                        rgb255 0 146 126

                     else
                        rgb255 0xD1 0xD5 0xD9
                    )
                 , Font.color
                    (if nID == task.correctAnswer then
                        rgb255 255 255 255

                     else
                        rgb255 0 0 0
                    )
                 , Border.rounded 20
                 , paddingXY 10 6
                 ]
                    ++ (if List.length nChildren > 0 then
                            [ alpha 0.5 ]

                        else
                            [ pointer, onClick (EditTaskAnswer idx nID) ]
                       )
                )
                (text nData.text)
            ]
            :: List.map (viewTaskNode idx task) nChildren
        )


tab : String -> Bool -> Msg -> Element Msg
tab label active onClicked =
    el
        [ paddingXY 10 8
        , Border.roundEach { bottomLeft = 4, bottomRight = 4, topLeft = 0, topRight = 0 }
        , Background.color <|
            if active then
                rgb255 0x03 0x66 0x88

            else
                rgb255 0xD1 0xD5 0xD9
        , Font.color <|
            if active then
                rgb255 0xFF 0xFF 0xFF

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


viewNode : Tree.Node Item -> Element Msg
viewNode (Tree.Node id data children) =
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
                { onChange = \str -> EditItem id str
                , text = data.text
                , placeholder = Nothing
                , label = Input.labelHidden "node name"
                }
            , if isEmpty then
                UI.button True "add children" (NewNode id)

              else
                none
            ]
            :: List.map viewNode children
            ++ [ if not isEmpty then
                    UI.button True "+" (NewNode id)

                 else
                    none
               ]
        )
