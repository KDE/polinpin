module Pages.Editor.TreeTest.Test_ exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Element.Input as Input
import Gen.Params.Editor.TreeTest.Test_ exposing (Params)
import HTTPExt
import Http
import Json.Decode as D
import Json.Encode as E
import Page
import Process
import Request
import Shared
import Task
import Tree
import TreeTest
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.protected.element
        (\user ->
            { init = init req.params.test
            , update = update user req.params.test
            , view = view shared
            , subscriptions = subscriptions
            }
        )



-- MODEL


type Model
    = Loaded LoadedModel
    | Loading
    | Failed Http.Error


type ActiveTab
    = EditTree
    | EditTasks


type SaveNotificationState
    = SaveIdle
    | SaveSucceeded
    | SaveFailed


type alias LoadedModel =
    { study : TreeTest.Study
    , oldStudy : TreeTest.Study
    , count : Int
    , activeTab : ActiveTab
    , showingTaskTree : Int
    , saveNotificationState : SaveNotificationState
    , onShelf : Maybe Tree.ID
    }



-- INIT


init : String -> ( Model, Cmd Msg )
init id =
    ( Loading, TreeTest.getStudy id GotStudy )



-- UPDATE


type Msg
    = EditItem Tree.ID String
    | PutOnShelf Tree.ID
    | TakeOffShelfBefore Tree.ID
    | TakeOffShelfAfter Tree.ID
    | PutBack
    | NewNode Tree.ID
    | DeleteNode Tree.ID
    | TabClicked ActiveTab
    | NewTask
    | EditTaskText Int String
    | EditTaskAnswer Int Tree.ID
    | ShowTaskTree Int
    | GotStudy (Result Http.Error TreeTest.Study)
    | Save
    | FinishedSave (Result Http.Error ())
    | SaveGoIdle


update : Shared.User -> String -> Msg -> Model -> ( Model, Cmd Msg )
update user studyID msg model =
    case ( msg, model ) of
        ( GotStudy (Ok study), _ ) ->
            ( Loaded <| LoadedModel study study 0 EditTree -1 SaveIdle Nothing, Cmd.none )

        ( GotStudy (Err error), _ ) ->
            ( Failed error, Cmd.none )

        ( _, Loaded m ) ->
            let
                ( new, cmd ) =
                    updateLoaded user studyID msg m
            in
            ( Loaded new, cmd )

        _ ->
            ( model, Cmd.none )


setRootNode : LoadedModel -> Tree.Node TreeTest.Item -> LoadedModel
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


delay : Float -> msg -> Cmd msg
delay time msg =
    Process.sleep time
        |> Task.andThen (always <| Task.succeed msg)
        |> Task.perform identity


goIdleMsg : Cmd Msg
goIdleMsg =
    delay 5000 SaveGoIdle


updateLoaded : Shared.User -> String -> Msg -> LoadedModel -> ( LoadedModel, Cmd Msg )
updateLoaded user studyID msg model =
    let
        study =
            model.study
    in
    case msg of
        PutOnShelf item ->
            ( { model | onShelf = Just item }, Cmd.none )

        TakeOffShelfBefore at ->
            case model.onShelf of
                Just item ->
                    ( { model | onShelf = Nothing, study = { study | tree = study.tree |> Tree.moveBefore item at } }, Cmd.none )

                _ ->
                    ( { model | onShelf = Nothing }, Cmd.none )

        TakeOffShelfAfter at ->
            case model.onShelf of
                Just item ->
                    ( { model | onShelf = Nothing, study = { study | tree = study.tree |> Tree.moveAfter item at } }, Cmd.none )

                _ ->
                    ( { model | onShelf = Nothing }, Cmd.none )

        PutBack ->
            ( { model | onShelf = Nothing }, Cmd.none )

        EditItem id cont ->
            ( setRootNode model <| Tree.mapID id (\it -> { it | text = cont }) model.study.tree, Cmd.none )

        DeleteNode id ->
            ( { model
                | study =
                    { study
                        | tree = study.tree |> Tree.delete id
                    }
              }
            , Cmd.none
            )

        NewNode id ->
            ( (setRootNode model <|
                Tree.appendID id
                    (Tree.makeLeaf
                        (Tree.ID <|
                            String.fromInt (Tree.uniqueIntID (model.count + Tree.count model.study.tree) model.study.tree)
                        )
                        (TreeTest.Item "")
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
                        | tasks = study.tasks ++ [ TreeTest.Task "" (Tree.ID "") ]
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

        Save ->
            ( model, TreeTest.setStudy user.token studyID model.study FinishedSave )

        FinishedSave (Ok _) ->
            ( { model | saveNotificationState = SaveSucceeded, oldStudy = model.study }, goIdleMsg )

        FinishedSave (Err _) ->
            ( { model | saveNotificationState = SaveFailed }, goIdleMsg )

        SaveGoIdle ->
            ( { model | saveNotificationState = SaveIdle }, goIdleMsg )

        _ ->
            ( model, Cmd.none )



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

        Loading ->
            viewLoading shared

        Failed error ->
            viewError shared error


viewLoading : Shared.Model -> View Msg
viewLoading shared =
    View "Loading..."
        (UI.with shared [])
        Nothing


viewError : Shared.Model -> Http.Error -> View Msg
viewError shared error =
    View "Error!"
        (UI.with shared [ text <| HTTPExt.errorToString error ])
        Nothing


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
                            viewNode True model model.study.tree

                        EditTasks ->
                            viewTasks model
                    )
                ]
            ]
        )
        Nothing


viewTasks : LoadedModel -> Element Msg
viewTasks model =
    column
        [ spacing 8 ]
        (List.indexedMap (viewTask model) model.study.tasks
            ++ [ UI.button True "new task" NewTask
               ]
        )


viewTask : LoadedModel -> Int -> TreeTest.Task -> Element Msg
viewTask model idx task =
    column
        [ padding 16
        , Border.width 4
        , width fill
        , spacing 8
        ]
        [ Input.multiline
            (width (fill |> maximum 500) :: UI.inputStyles)
            { onChange = EditTaskText idx
            , text = task.text
            , placeholder = Nothing
            , label = Input.labelAbove [ Font.size 16 ] (text "Task text")
            , spellcheck = True
            }
        , el [ Font.size 16 ] (text "Task answer")
        , if model.showingTaskTree == idx then
            viewTaskNode idx task model.study.tree

          else
            row [ width fill, spacing 6 ]
                [ let
                    gText (Tree.Node _ cont _) =
                        cont.text

                    label =
                        Tree.nodeByIDWithParents task.correctAnswer model.study.tree
                            |> Maybe.map (\( node, parents ) -> (parents ++ [ node ]) |> List.map gText |> String.join " / ")
                            |> Maybe.withDefault "Failed to find node"
                  in
                  el [ width fill ] (text label)
                , UI.button True "change" (ShowTaskTree idx)
                ]
        ]


viewTaskNode : Int -> TreeTest.Task -> Tree.Node TreeTest.Item -> Element Msg
viewTaskNode idx task (Tree.Node nID nData nChildren) =
    column
        [ paddingEach { edges | left = 10 }
        , Border.color <| rgb255 0 0 0
        , Border.widthEach { edges | left = 4 }
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


viewTabs : LoadedModel -> Element Msg
viewTabs model =
    let
        make label role =
            UI.tab label (model.activeTab == role) (TabClicked role)

        roles =
            [ make "tree" EditTree
            , make "tasks" EditTasks
            ]
    in
    row [ paddingEach { edges | left = 8 }, spacing 6 ]
        roles


viewHeader : LoadedModel -> Element Msg
viewHeader model =
    UI.subToolbar
        [ text <| "editing " ++ model.study.name
        , el [ alignRight ]
            (case model.saveNotificationState of
                SaveFailed ->
                    UI.destructiveButton True "save failed!" Save

                SaveIdle ->
                    if model.study == model.oldStudy then
                        UI.button False "up to date" Save

                    else
                        UI.button True "save" Save

                SaveSucceeded ->
                    UI.button True "saved!" Save
            )
        ]


edges : { left : number, top : number, bottom : number, right : number }
edges =
    { left = 0, top = 0, bottom = 0, right = 0 }


viewNode : Bool -> LoadedModel -> Tree.Node TreeTest.Item -> Element Msg
viewNode isRoot model (Tree.Node id data children) =
    let
        isEmpty =
            List.length children == 0

        nonShelfActions =
            [ UI.button True "move" (PutOnShelf id)
            , if isEmpty then
                UI.button True "add children" (NewNode id)

              else
                none
            , UI.destructiveButton True "delete" (DeleteNode id)
            ]

        currentlyShelfingActions =
            [ UI.epheremalButton True "move before this" (TakeOffShelfBefore id)
            , UI.epheremalButton True "move after this" (TakeOffShelfAfter id)
            ]

        shelfActions =
            [ UI.button True "cancel move" PutBack ]

        actions =
            if isRoot then
                [ none ]

            else
                case model.onShelf of
                    Just item ->
                        if item == id then
                            shelfActions

                        else
                            currentlyShelfingActions

                    _ ->
                        nonShelfActions

        trailing =
            [ if not isEmpty then
                UI.button True "+" (NewNode id)

              else
                none
            ]

        childs =
            List.map (viewNode False model) children
    in
    column
        [ paddingEach { edges | left = 20 }
        , Border.color <| rgb255 0 0 0
        , Border.widthEach { edges | left = 4 }
        , Border.dotted
        , spacing 8
        , case model.onShelf of
            Just item ->
                if item == id then
                    alpha 0.5

                else
                    alpha 1.0

            Nothing ->
                alpha 1.0
        ]
        (row [ spacing 4 ]
            (Input.text UI.inputStyles
                { onChange = \str -> EditItem id str
                , text = data.text
                , placeholder = Nothing
                , label = Input.labelHidden "node name"
                }
                :: actions
            )
            :: (case model.onShelf of
                    Just item ->
                        if item == id then
                            []

                        else
                            childs

                    _ ->
                        childs
               )
            ++ (case model.onShelf of
                    Just item ->
                        if item == id then
                            []

                        else
                            trailing

                    _ ->
                        trailing
               )
        )
