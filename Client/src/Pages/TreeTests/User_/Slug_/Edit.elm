module Pages.TreeTests.User_.Slug_.Edit exposing (Model, Msg, page)

import Auth
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Gen.Params.TreeTests.User_.Slug_.Edit exposing (Params)
import Http
import Network
import Page
import Request
import Shared
import SharedUI
import TreeManipulation
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.protected.advanced
        (\user ->
            { init = init user req.params
            , update = update user req.params
            , view = view shared
            , subscriptions = subscriptions
            }
        )



-- INIT


type Model
    = Loaded LoadedModel
    | Loading
    | Failed Http.Error


init : Auth.User -> Params -> ( Model, Effect Msg )
init user params =
    ( Loading
    , Effect.fromCmd <|
        Network.treeTest (Network.UserSession user.token) params.user params.slug GotResult
    )



-- UPDATE


type Msg
    = GotResult (Result Http.Error Network.TreeTestStudyData)
    | Inner LoadedMsg


update : Auth.User -> Params -> Msg -> Model -> ( Model, Effect Msg )
update user params msg model =
    case ( model, msg ) of
        ( _, GotResult (Ok ok) ) ->
            ( Loaded (initialLoaded ok), Effect.none )

        ( _, GotResult (Err err) ) ->
            ( Failed err, Effect.none )

        ( Loaded innerModel, Inner inner ) ->
            let
                ( new, cmd ) =
                    updateLoaded user params inner innerModel
            in
            ( Loaded new, Effect.map Inner cmd )

        ( _, Inner _ ) ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    case model of
        Loaded loaded ->
            loadedView shared loaded
                |> View.map Inner

        Loading ->
            loadingView shared

        Failed err ->
            failedView shared err


loadingView : Shared.Model -> View Msg
loadingView shared =
    SharedUI.sharedFrame shared
        { title = "Loading..."
        , body = none
        , over = Nothing
        }


failedView : Shared.Model -> Http.Error -> View Msg
failedView shared _ =
    SharedUI.sharedFrame shared
        { title = "Failed :/"
        , body = none
        , over = Nothing
        }



-- MODEL LOADED


type ActiveTab
    = Tree
    | Tasks


type alias LoadedModel =
    { studyData : Network.StudyData
    , tree : Network.TreeNode Network.TreeStudyItem
    , tasks : List Network.TreeStudyTask
    , tab : ActiveTab
    , definingFor : Maybe Int
    }


initialLoaded : Network.TreeTestStudyData -> LoadedModel
initialLoaded data =
    LoadedModel data.studyData data.tree data.tasks Tree Nothing



-- UPDATE LOADED


type LoadedMsg
    = SetTab ActiveTab
    | EditItem String String
    | DeleteNode String
    | NewNodeUnder String
    | NewTask
    | EditTask Int String
    | StartDefiningAnswerForTask Int
    | DefineAnswerForTask Int String
    | CancelDefiningAnswerForTask
    | Save
    | SaveResult (Result Http.Error ())


uniqueTaskID : Int -> List Network.TreeStudyTask -> String
uniqueTaskID accumulator list =
    if not (List.any (\v -> v.id == String.fromInt accumulator) list) then
        accumulator |> String.fromInt

    else
        uniqueTaskID (accumulator + 1) list


updateLoaded : Auth.User -> Params -> LoadedMsg -> LoadedModel -> ( LoadedModel, Effect LoadedMsg )
updateLoaded user params msg model =
    case msg of
        SetTab tab ->
            ( { model | tab = tab }, Effect.none )

        EditItem id cont ->
            ( { model | tree = TreeManipulation.mapByID id (\k -> { k | text = cont }) model.tree }, Effect.none )

        NewNodeUnder underID ->
            ( { model
                | tree =
                    TreeManipulation.appendInNode
                        underID
                        (Network.TreeNode (TreeManipulation.uniqueIntID 0 model.tree) { text = "" } [])
                        model.tree
              }
            , Effect.none
            )

        DeleteNode id ->
            ( { model | tree = TreeManipulation.deleteByID id model.tree }, Effect.none )

        NewTask ->
            ( { model
                | tasks =
                    model.tasks
                        ++ [ Network.TreeStudyTask
                                (uniqueTaskID 0 model.tasks)
                                ""
                                ""
                           ]
              }
            , Effect.none
            )

        EditTask num text ->
            ( { model | tasks = mapOne (\t -> { t | text = text }) num model.tasks }, Effect.none )

        StartDefiningAnswerForTask num ->
            ( { model | definingFor = Just num }, Effect.none )

        DefineAnswerForTask num id ->
            ( { model | definingFor = Nothing, tasks = mapOne (\t -> { t | answer = id }) num model.tasks }, Effect.none )

        CancelDefiningAnswerForTask ->
            ( { model | definingFor = Nothing }, Effect.none )

        Save ->
            ( model, Effect.fromCmd <|
                Network.saveTreeStudy
                    (Network.UserSession user.token)
                    params.user
                    params.slug
                    model.tree
                    model.tasks
                    SaveResult
            )

        SaveResult _ ->
            ( model, Effect.none )



-- VIEW LOADED


tabButton : ActiveTab -> ActiveTab -> String -> Element LoadedMsg
tabButton currentTab forTab text =
    (if currentTab == forTab then
        UI.darkTextButton

     else
        UI.textButton
    )
        (Just (SetTab forTab))
        []
        text


loadedView : Shared.Model -> LoadedModel -> View LoadedMsg
loadedView shared model =
    SharedUI.sharedFrame shared
        { title = "Viewing " ++ model.studyData.title
        , body =
            column [ padding 10, spacing 20, width fill ]
                [ row [ spacing 20, width fill ]
                    [ tabButton model.tab Tree "Tree"
                    , tabButton model.tab Tasks "Tasks"
                    , UI.textButton (Just Save) [ alignRight ] "Save"
                    ]
                , case model.tab of
                    Tree ->
                        viewNode True model model.tree

                    Tasks ->
                        viewTasks model model.tasks
                ]
        , over = Maybe.map (selectingDialog model) model.definingFor
        }



-- NODE VIEW


viewNode : Bool -> LoadedModel -> Network.TreeNode Network.TreeStudyItem -> Element LoadedMsg
viewNode isRoot model (Network.TreeNode id data children) =
    let
        childNodes =
            List.map (viewNode False model) children

        addChild =
            UI.smallTextButton (Just (NewNodeUnder id)) [] "add"

        delete =
            UI.smallTextButton (Just (DeleteNode id)) [] "delete"

        actions =
            if isRoot then
                [ addChild ]

            else
                [ addChild, delete ]
    in
    column
        [ paddingEach
            { left =
                if isRoot then
                    0

                else
                    30
            , top = 0
            , right = 0
            , bottom = 0
            }
        , spacing 8
        ]
        (row [ spacing 10 ]
            (UI.textInput []
                { onChange = \str -> EditItem id str
                , text = data.text
                , placeholder = Just (Input.placeholder [] (text "label"))
                , label = Input.labelHidden "node name"
                }
                :: actions
            )
            :: childNodes
        )


mapOne : (a -> a) -> Int -> List a -> List a
mapOne mapper index list =
    let
        inner idx item =
            if idx == index then
                mapper item

            else
                item
    in
    List.indexedMap inner list


viewTask : LoadedModel -> Int -> Network.TreeStudyTask -> Element LoadedMsg
viewTask model num task =
    UI.grayBox [ padding 20, width fill ] <|
        column [ width fill ]
            [ row [ width fill, paddingEach { top = 0, left = 0, right = 0, bottom = 10 } ]
                [ el [ Font.size <| UI.intScale -1 ] (text <| "Task " ++ String.fromInt (num + 1))
                , UI.smallTextButton (Just (StartDefiningAnswerForTask num)) [ alignRight ] "Define Correct Answer"
                ]
            , UI.multilineInput [ width fill ]
                { onChange = EditTask num
                , text = task.text
                , placeholder = Just (Input.placeholder [] (text "Enter task text here"))
                , label = Input.labelHidden "task text"
                , spellcheck = True
                }
            , let
                label =
                    TreeManipulation.nodeByIDWithParents task.answer model.tree
                        |> Maybe.map (\( node, parents ) -> (parents ++ [ node ]) |> List.map (\(Network.TreeNode _ cont _) -> cont.text) |> String.join " / ")
                        |> Maybe.withDefault "Failed to find answer"
              in
              el [ paddingEach { top = 10, left = 0, right = 0, bottom = 0 } ] (text label)
            ]


viewTasks : LoadedModel -> List Network.TreeStudyTask -> Element LoadedMsg
viewTasks model tasks =
    column [ spacing 20, width (fill |> maximum 600) ]
        (List.indexedMap (viewTask model) tasks
            ++ [ UI.textButton (Just NewTask) [ width fill ] "New Task" ]
        )



-- SELECTING DIALOG


selectingViewNode : Int -> Bool -> LoadedModel -> Network.TreeNode Network.TreeStudyItem -> Element LoadedMsg
selectingViewNode num isRoot model (Network.TreeNode id data children) =
    let
        childNodes =
            List.map (selectingViewNode num False model) children
        action =
            if List.length children == 0 then
                Just (DefineAnswerForTask num id)
            else
                Nothing
        item =
            if List.length children == 0 then
                UI.tealTextButton
            else
                UI.textButton
    in
    column
        [ paddingEach
            { left =
                if isRoot then
                    0

                else
                    30
            , top = 0
            , right = 0
            , bottom = 0
            }
        , spacing 8
        ]
        (item action [] data.text
            :: childNodes
        )


selectingDialog : LoadedModel -> Int -> Element LoadedMsg
selectingDialog model num =
    column
        [ centerX
        , centerY
        , width (fill |> maximum 600)
        , Background.color <| rgb255 225 221 210
        , behindContent (UI.grayBox [ width fill, height fill ] none)
        , padding 20
        , spacing 20
        ]
        [ row [ width fill ]
            [ text "Select An Answer"
            , UI.textButton (Just CancelDefiningAnswerForTask) [ alignRight ] "Close"
            ]
        , selectingViewNode num True model model.tree
        ]
