module Pages.TreeTests.User_.Slug_.Edit exposing (Model, Msg, page)

import Array exposing (Array)
import Auth
import Browser.Navigation
import Dict
import Drag
import Effect exposing (Effect)
import Element exposing (..)
import Element.Background as Background
import Element.Font as Font
import Element.Input as Input
import Gen.Params.TreeTests.User_.Slug_.Edit exposing (Params)
import Http
import Json.Decode as D
import Json.Encode as E
import List.Extra
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
    | LoadingResults Network.TreeTestStudyData
    | LoadedResults ResultsModel


init : Auth.User -> Params -> ( Model, Effect Msg )
init user params =
    ( Loading
    , Effect.fromCmd <|
        Network.treeTest (Network.UserSession user.token) params.user params.slug GotResult
    )



-- UPDATE


type Msg
    = GotResult (Result Http.Error Network.TreeTestStudyData)
    | GotResultsResult (Result Http.Error Network.TreeStudyResults)
    | Inner LoadedMsg
    | InnerResults ResultsMsg


update : Auth.User -> Params -> Msg -> Model -> ( Model, Effect Msg )
update user params msg model =
    case ( model, msg ) of
        ( _, GotResult (Ok ok) ) ->
            if ok.studyData.published then
                ( LoadingResults ok, Effect.fromCmd <| Network.treeStudyResults (Network.UserSession user.token) params.user params.slug GotResultsResult )

            else
                ( Loaded (initialLoaded ok), Effect.none )

        ( LoadingResults results, GotResultsResult (Ok ok) ) ->
            initResults results ok
                |> Tuple.mapBoth LoadedResults (Effect.map InnerResults)

        ( _, GotResultsResult (Err err) ) ->
            ( Failed err, Effect.none )

        ( _, GotResult (Err err) ) ->
            ( Failed err, Effect.none )

        ( Loaded innerModel, Inner inner ) ->
            let
                ( new, cmd ) =
                    updateLoaded user params inner innerModel
            in
            ( Loaded new, Effect.map Inner cmd )

        ( LoadedResults innerModel, InnerResults inner ) ->
            updateResults inner innerModel
                |> Tuple.mapBoth LoadedResults (Effect.map InnerResults)

        _ ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loaded inner ->
            loadedSubscriptions inner |> Sub.map Inner

        _ ->
            Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    case model of
        Loaded loaded ->
            loadedView shared loaded
                |> View.map Inner

        LoadedResults loaded ->
            resultsView shared loaded
                |> View.map InnerResults

        Loading ->
            loadingView shared

        LoadingResults _ ->
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
    , dragging : Maybe (Network.TreeNode Network.TreeStudyItem)
    , dragTarget : Maybe CandidatePosition
    }


initialLoaded : Network.TreeTestStudyData -> LoadedModel
initialLoaded data =
    LoadedModel data.studyData data.tree data.tasks Tree Nothing Nothing Nothing



-- SUBSCRIPTIONS LOADED


loadedSubscriptions : LoadedModel -> Sub LoadedMsg
loadedSubscriptions _ =
    Drag.subDragEvents candidatePositionDecoder Drag



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
    | Publish
    | PublishResult (Result Http.Error ())
    | Drag (Drag.DragMsg (Network.TreeNode Network.TreeStudyItem) CandidatePosition)


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
            ( model
            , Effect.fromCmd <|
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

        Publish ->
            ( model
            , Effect.fromCmd <|
                Network.publishTreeStudy
                    (Network.UserSession user.token)
                    params.user
                    params.slug
                    PublishResult
            )

        PublishResult (Ok _) ->
            ( model, Effect.fromCmd <| Browser.Navigation.reload )

        PublishResult (Err _) ->
            ( model, Effect.none )

        Drag (Drag.Start ( (Network.TreeNode id _ _) as node, _ )) ->
            ( { model | dragging = Just node, tree = TreeManipulation.deleteByID id model.tree }, Effect.none )

        Drag (Drag.Move coords beacons) ->
            ( { model | dragTarget = Drag.closestBeacon coords beacons }, Effect.none )

        Drag Drag.Stop ->
            let
                nieuw =
                    { model | dragging = Nothing, dragTarget = Nothing }
            in
            case ( model.dragging, model.dragTarget ) of
                ( Just node, Just target ) ->
                    case target of
                        Before id ->
                            ( { nieuw | tree = TreeManipulation.appendBeforeNode node id nieuw.tree }, Effect.none )

                        After id ->
                            ( { nieuw | tree = TreeManipulation.appendAfterNode node id nieuw.tree }, Effect.none )

                        AppendedIn id ->
                            ( { nieuw | tree = TreeManipulation.appendInNode id node nieuw.tree }, Effect.none )

                _ ->
                    ( nieuw, Effect.none )

        Drag (Drag.Invalid _) ->
            ( model, Effect.none )



-- VIEW LOADED


tabButton : a -> a -> (a -> msg) -> String -> Element msg
tabButton currentTab forTab msg text =
    (if currentTab == forTab then
        UI.darkTextButton

     else
        UI.textButton
    )
        (Just (msg forTab))
        []
        text


loadedView : Shared.Model -> LoadedModel -> View LoadedMsg
loadedView shared model =
    SharedUI.sharedFrame shared
        { title = "Viewing " ++ model.studyData.title
        , body =
            column [ padding 10, spacing 20, width fill ]
                [ row [ spacing 20, width fill ]
                    [ tabButton model.tab Tree SetTab "Tree"
                    , tabButton model.tab Tasks SetTab "Tasks"
                    , UI.textButton (Just Publish) [ alignRight ] "Publish"
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



-- BEACON


type CandidatePosition
    = Before String
    | After String
    | AppendedIn String


candidatePositionDecoder : D.Decoder CandidatePosition
candidatePositionDecoder =
    D.map2
        Tuple.pair
        (D.field "position" D.string)
        (D.field "text" D.string)
        |> D.andThen jsonValueToPosition


jsonValueToPosition : ( String, String ) -> D.Decoder CandidatePosition
jsonValueToPosition ( position, text ) =
    case position of
        "before" ->
            D.succeed (Before text)

        "after" ->
            D.succeed (After text)

        "appended-in" ->
            D.succeed (AppendedIn text)

        _ ->
            D.fail ("Unknown position: " ++ position)


encodeCandidatePosition : CandidatePosition -> E.Value
encodeCandidatePosition position =
    let
        ( positionStr, textStr ) =
            case position of
                Before text ->
                    ( "before", text )

                After text ->
                    ( "after", text )

                AppendedIn text ->
                    ( "appended-in", text )
    in
    E.object
        [ ( "position", E.string positionStr )
        , ( "text", E.string textStr )
        ]


beacon : CandidatePosition -> List (Attribute msg) -> Element msg
beacon position =
    Drag.beacon (encodeCandidatePosition position)



-- NODE VIEW


viewPlacingInner : String -> Element LoadedMsg
viewPlacingInner label =
    row [ paddingEach { left = 30, top = 0, bottom = 0, right = 0 }, spacing 10 ]
        [ UI.grayBox [ height fill, width (px 16) ] none
        , UI.grayBox [ padding 10 ] (text label)
        ]


viewPlacing : Maybe (Network.TreeNode Network.TreeStudyItem) -> Element LoadedMsg
viewPlacing node =
    case node of
        Just (Network.TreeNode _ content _) ->
            viewPlacingInner content.text

        Nothing ->
            none


whenTrue : Bool -> Element msg -> Element msg
whenTrue cond node =
    if cond then
        node

    else
        none


viewNode : Bool -> LoadedModel -> Network.TreeNode Network.TreeStudyItem -> Element LoadedMsg
viewNode isRoot model ((Network.TreeNode id data children) as node) =
    let
        childNodes =
            List.map (viewNode False model) children

        addChild =
            UI.smallTextButton (Just (NewNodeUnder id)) [] "add"

        delete =
            UI.smallTextButton (Just (DeleteNode id)) [] "delete"

        editActions =
            if isRoot then
                [ addChild ]

            else
                [ addChild, delete ]

        whenNotRoot el =
            if isRoot then
                none

            else
                el
    in
    column
        [ spacing 8
        , Element.below <| whenNotRoot (beacon (After id) [ width (px 1) ])
        ]
        [ whenTrue (model.dragTarget == Just (Before id)) (viewPlacing model.dragging)
        , column
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
            (row
                [ spacing 10
                , Element.above <| whenNotRoot (beacon (Before id) [ width (px 1) ])
                ]
                ([ if not isRoot then
                    UI.grayBox
                        [ Drag.onDragStart Drag node
                        , height fill
                        , width (px 16)
                        ]
                        none

                   else
                    none
                 , UI.textInput [ Element.below <| beacon (AppendedIn id) [ width (px 1) ] ]
                    { onChange = \str -> EditItem id str
                    , text = data.text
                    , placeholder = Just (Input.placeholder [] (text "label"))
                    , label = Input.labelHidden "node name"
                    }
                 ]
                    ++ editActions
                )
                :: whenTrue (model.dragTarget == Just (AppendedIn id)) (viewPlacing model.dragging)
                :: childNodes
            )
        , whenTrue (model.dragTarget == Just (After id)) (viewPlacing model.dragging)
        ]


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



-- INIT RESULTS


type ResultsActiveTab
    = ResultsTree
    | ResultsTasks
    | Overview


type alias ResultsModel =
    { studyData : Network.StudyData
    , tree : Network.TreeNode Network.TreeStudyItem
    , tasks : List Network.TreeStudyTask
    , results : Network.TreeStudyResults
    , tab : ResultsActiveTab
    }


initResults : Network.TreeTestStudyData -> Network.TreeStudyResults -> ( ResultsModel, Effect ResultsMsg )
initResults study results =
    ( { studyData = study.studyData
      , tree = study.tree
      , tasks = study.tasks
      , results = results
      , tab = Overview
      }
    , Effect.none
    )



-- UPDATE RESULTS


type ResultsMsg
    = ResultsSetTab ResultsActiveTab


updateResults : ResultsMsg -> ResultsModel -> ( ResultsModel, Effect ResultsMsg )
updateResults msg model =
    case msg of
        ResultsSetTab tab ->
            ( { model | tab = tab }, Effect.none )



-- VIEW RESULTS


resultsTree : Bool -> Network.TreeNode Network.TreeStudyItem -> Element ResultsMsg
resultsTree isRoot (Network.TreeNode _ content children) =
    column
        [ paddingEach
            { left =
                if isRoot then
                    0

                else
                    30
            , top = 0
            , bottom = 0
            , right = 0
            }
        , spacing 10
        ]
        (UI.grayBox [ padding 10 ] (text content.text)
            :: List.map (resultsTree False) children
        )


resultsTask : ResultsModel -> Int -> ( Network.TreeStudyTask, List Network.TreeTestAnsweredQuestion ) -> Element ResultsMsg
resultsTask model num ( task, answers ) =
    let
        pathLabel item =
            TreeManipulation.nodeByIDWithParents item model.tree
                |> Maybe.map (\( node, parents ) -> (parents ++ [ node ]) |> List.map (\(Network.TreeNode _ cont _) -> cont.text) |> String.join " / ")
                |> Maybe.withDefault "Failed to find answer"

        innerCounter value dictionary =
            Dict.update value (Maybe.withDefault 0 >> (+) 1 >> Just) dictionary

        selectedAnswers =
            answers
                |> List.map .answer
                |> List.foldl innerCounter Dict.empty
                |> Dict.toList
                |> List.sortBy Tuple.second
                |> List.reverse
    in
    UI.grayBox [ padding 20, width (fill |> maximum 800) ] <|
        column [ width fill, spacing 10 ]
            ([ el [ Font.size <| UI.intScale -1 ] (text <| "Task " ++ String.fromInt (num + 1))
             , text task.text
             , let
                label =
                    pathLabel task.answer
               in
               el [ UI.fontSize -1 ] (text label)
             , UI.blackLine [ width fill ]
             , text "Success"
             , column [ paddingEach { left = 20, top = 0, bottom = 0, right = 0 }, spacing 10 ]
                [ text <| "Direct: " ++ (String.fromInt <| countDirectSuccess model.tree model.tasks answers)
                , text <| "Indirect: " ++ (String.fromInt <| countIndirectSuccess model.tree model.tasks answers)
                ]
             , text "Failure"
             , column [ paddingEach { left = 20, top = 0, bottom = 0, right = 0 }, spacing 10 ]
                [ text <| "Direct: " ++ (String.fromInt <| countDirectFailure model.tree model.tasks answers)
                , text <| "Indirect: " ++ (String.fromInt <| countIndirectFailure model.tree model.tasks answers)
                ]
             , UI.blackLine [ width fill ]
             , text "Chosen Answers"
             ]
                ++ List.map
                    (\( node, chosenCount ) ->
                        el [ UI.fontSize -1 ] (text <| pathLabel node ++ ": " ++ String.fromInt chosenCount)
                    )
                    selectedAnswers
            )


resultsTasks : ResultsModel -> List Network.TreeStudyTask -> Element ResultsMsg
resultsTasks model tasks =
    let
        tasksWithResponses =
            List.map2 Tuple.pair
                tasks
                (List.Extra.transpose (model.results.observations |> List.map .responses))
    in
    column [ spacing 10, width fill ]
        (List.indexedMap (resultsTask model) tasksWithResponses)


isDirect : Network.TreeNode Network.TreeStudyItem -> Network.TreeTestAnsweredQuestion -> Bool
isDirect tree answer =
    let
        inner pastSelections node =
            case pastSelections of
                [] ->
                    True

                head :: tail ->
                    TreeManipulation.nodeByID head.selectedID node
                        |> Maybe.map (inner tail)
                        |> Maybe.withDefault False
    in
    inner answer.pastSelections tree


percent : (a -> Bool) -> List a -> Float
percent f list =
    (List.filter f list
        |> List.length
        |> toFloat
    )
        / (List.length list |> toFloat)


count : (a -> Bool) -> List a -> Int
count f list =
    List.filter f list
        |> List.length


percentDirect : Network.TreeNode Network.TreeStudyItem -> List Network.TreeTestObservation -> Float
percentDirect tree list =
    list
        |> List.concatMap .responses
        |> percent (isDirect tree)


firstList : List a -> Maybe a
firstList list =
    case list of
        x :: _ ->
            Just x

        _ ->
            Nothing


isCorrect : List Network.TreeStudyTask -> Network.TreeTestAnsweredQuestion -> Bool
isCorrect tasks item =
    item.answer == (tasks |> List.filter (\task -> task.id == item.taskID) |> List.map .answer |> firstList |> Maybe.withDefault "")


timeTakenMillis : Network.TreeTestAnsweredQuestion -> Int
timeTakenMillis answer =
    answer.endedAt - answer.startedAt


millisToSecond : Int -> Int
millisToSecond millis =
    millis // 1000


median : Array Float -> Float
median items =
    let
        itemCount =
            Array.length items
    in
    if modBy itemCount 2 /= 0 then
        Array.get (itemCount // 2) items
            |> Maybe.withDefault 0.0

    else
        ( Array.get (itemCount // 2) items, Array.get ((itemCount // 2) - 1) items )
            |> Tuple.mapBoth (Maybe.withDefault 0.0) (Maybe.withDefault 0.0)
            |> (\( x, y ) -> (x + y) / 2.0)


percentCorrect : List Network.TreeStudyTask -> List Network.TreeTestObservation -> Float
percentCorrect tasks list =
    list
        |> List.concatMap .responses
        |> percent (isCorrect tasks)


floatToPercent : Float -> String
floatToPercent =
    (*) 100
        >> round
        >> String.fromInt
        >> (\k -> k ++ "%")


medianTime : List Network.TreeTestObservation -> Float
medianTime list =
    list
        |> List.concatMap .responses
        |> List.map (timeTakenMillis >> millisToSecond >> toFloat)
        |> Array.fromList
        |> median


minimumTime : List Network.TreeTestObservation -> Int
minimumTime list =
    list
        |> List.concatMap .responses
        |> List.map (timeTakenMillis >> millisToSecond)
        |> List.minimum
        |> Maybe.withDefault -1


maximumTime : List Network.TreeTestObservation -> Int
maximumTime list =
    list
        |> List.concatMap .responses
        |> List.map (timeTakenMillis >> millisToSecond)
        |> List.maximum
        |> Maybe.withDefault -1


percentDirectFailure : Network.TreeNode Network.TreeStudyItem -> List Network.TreeStudyTask -> List Network.TreeTestAnsweredQuestion -> Float
percentDirectFailure tree tasks answers =
    percent (isDirectFailure tree tasks) answers


isDirectFailure : Network.TreeNode Network.TreeStudyItem -> List Network.TreeStudyTask -> Network.TreeTestAnsweredQuestion -> Bool
isDirectFailure tree tasks x =
    isDirect tree x && not (isCorrect tasks x)


countDirectFailure : Network.TreeNode Network.TreeStudyItem -> List Network.TreeStudyTask -> List Network.TreeTestAnsweredQuestion -> Int
countDirectFailure tree tasks answers =
    count (isDirectFailure tree tasks) answers


countIndirectFailure : Network.TreeNode Network.TreeStudyItem -> List Network.TreeStudyTask -> List Network.TreeTestAnsweredQuestion -> Int
countIndirectFailure tree tasks answers =
    count (isIndirectFailure tree tasks) answers


countIndirectSuccess : Network.TreeNode Network.TreeStudyItem -> List Network.TreeStudyTask -> List Network.TreeTestAnsweredQuestion -> Int
countIndirectSuccess tree tasks answers =
    count (isIndirectSuccess tree tasks) answers


countDirectSuccess : Network.TreeNode Network.TreeStudyItem -> List Network.TreeStudyTask -> List Network.TreeTestAnsweredQuestion -> Int
countDirectSuccess tree tasks answers =
    count (isDirectSuccess tree tasks) answers


percentDirectSuccess : Network.TreeNode Network.TreeStudyItem -> List Network.TreeStudyTask -> List Network.TreeTestAnsweredQuestion -> Float
percentDirectSuccess tree tasks answers =
    percent (isDirectSuccess tree tasks) answers


isDirectSuccess : Network.TreeNode Network.TreeStudyItem -> List Network.TreeStudyTask -> Network.TreeTestAnsweredQuestion -> Bool
isDirectSuccess tree tasks x =
    isDirect tree x && isCorrect tasks x


percentIndirectSuccess : Network.TreeNode Network.TreeStudyItem -> List Network.TreeStudyTask -> List Network.TreeTestAnsweredQuestion -> Float
percentIndirectSuccess tree tasks answers =
    percent (isIndirectSuccess tree tasks) answers


isIndirectSuccess : Network.TreeNode Network.TreeStudyItem -> List Network.TreeStudyTask -> Network.TreeTestAnsweredQuestion -> Bool
isIndirectSuccess tree tasks x =
    not (isDirect tree x) && isCorrect tasks x


percentIndirectFailure : Network.TreeNode Network.TreeStudyItem -> List Network.TreeStudyTask -> List Network.TreeTestAnsweredQuestion -> Float
percentIndirectFailure tree tasks answers =
    percent (isIndirectFailure tree tasks) answers


isIndirectFailure : Network.TreeNode Network.TreeStudyItem -> List Network.TreeStudyTask -> Network.TreeTestAnsweredQuestion -> Bool
isIndirectFailure tree tasks x =
    not (isDirect tree x) && not (isCorrect tasks x)


answersByTask : List Network.TreeTestObservation -> List (List Network.TreeTestAnsweredQuestion)
answersByTask observations =
    observations
        |> List.map .responses
        |> List.Extra.transpose


bigPercentBox : String -> String -> Color -> Float -> Element msg
bigPercentBox title subtitle color value =
    UI.grayBox
        [ width fill, padding 20 ]
        (column [ spacing 10 ]
            [ el [ centerX ] <| text title
            , UI.solidBox color
                [ centerX
                , padding 20
                , UI.fontSize 2
                , Font.bold
                , Font.color <| rgb255 255 255 255
                ]
              <|
                text (floatToPercent value)
            , el [ centerX, UI.fontSize -1 ] <| text subtitle
            ]
        )


palette : { directF : Color, indirectF : Color, indirectS : Color, directS : Color }
palette =
    { directF = rgb255 233 61 88
    , indirectF = rgb255 239 151 60
    , indirectS = rgb255 184 117 220
    , directS = rgb255 61 212 37
    }


taskStats : ResultsModel -> Int -> List Network.TreeTestAnsweredQuestion -> Element ResultsMsg
taskStats model num answers =
    let
        directFailure =
            percentDirectFailure model.tree model.tasks answers

        directSuccess =
            percentDirectSuccess model.tree model.tasks answers

        indirectSuccess =
            percentIndirectSuccess model.tree model.tasks answers

        indirectFailure =
            percentIndirectFailure model.tree model.tasks answers

        item color itemPercent =
            UI.solidRoughBox color [ width (fillPortion (round (itemPercent * 1000))), height (px 20) ] none
    in
    column [ width fill ]
        [ text <| "Task " ++ (String.fromInt <| num + 1)
        , row [ width fill, paddingXY 0 10 ]
            [ item palette.directF directFailure
            , item palette.indirectF indirectFailure
            , item palette.indirectS indirectSuccess
            , item palette.directS directSuccess
            ]
        ]


resultsOverview : ResultsModel -> Element ResultsMsg
resultsOverview model =
    column
        [ width (fill |> maximum 800), spacing 30 ]
        [ row [ width fill, spacing 20 ]
            [ bigPercentBox
                "Success"
                "Average success rate across all tasks"
                (rgb255 0 147 86)
              <|
                percentCorrect model.tasks model.results.observations
            , bigPercentBox
                "Directness"
                "Average directness rate across all tasks"
                (rgb255 0 146 126)
              <|
                percentDirect model.tree model.results.observations
            ]
        , let
            medTime =
                String.fromFloat <| medianTime model.results.observations

            maxTime =
                String.fromInt <| maximumTime model.results.observations

            minTime =
                String.fromInt <| minimumTime model.results.observations
          in
          UI.grayBox [ padding 20, width fill ]
            (paragraph [ width fill ]
                [ text "The median time for completion was "
                , el [ Font.bold ] (text medTime)
                , text " seconds."
                , text "The minimum time was "
                , el [ Font.bold ] (text minTime)
                , text " seconds and the maximum time was "
                , el [ Font.bold ] (text maxTime)
                , text " seconds."
                ]
            )
        , UI.grayBox [ padding 20, width fill ]
            (column [ spacing 20, width fill ]
                ([ el [ UI.fontSize 2, centerX ] (text "Per-Task Statistics")
                 , row [ centerX, spacing 10 ]
                    (let
                        box color label =
                            row [ UI.fontSize -2, spacing 10 ] [ UI.solidBox color [ width (px 16), height (px 16) ] none, text label ]
                     in
                     [ box palette.directF "Direct Failure"
                     , box palette.indirectF "Indirect Failure"
                     , box palette.indirectS "Indirect Success"
                     , box palette.directS "Indirect Success"
                     ]
                    )
                 ]
                    ++ (answersByTask model.results.observations |> List.indexedMap (taskStats model))
                )
            )
        ]


resultsView : Shared.Model -> ResultsModel -> View ResultsMsg
resultsView shared model =
    SharedUI.sharedFrame shared
        { title = "Viewing " ++ model.studyData.title
        , body =
            column [ padding 10, spacing 20, width fill ]
                [ row [ spacing 20, width fill ]
                    [ tabButton model.tab Overview ResultsSetTab "Overview"
                    , tabButton model.tab ResultsTasks ResultsSetTab "Tasks"
                    , tabButton model.tab ResultsTree ResultsSetTab "Tree"
                    ]
                , case model.tab of
                    ResultsTree ->
                        resultsTree True model.tree

                    ResultsTasks ->
                        resultsTasks model model.tasks

                    Overview ->
                        resultsOverview model
                ]
        , over = Nothing
        }
