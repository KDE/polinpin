module Pages.TreeTest.Test_ exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Gen.Params.TreeTest.Test_ exposing (Params)
import Http
import Json.Decode as D
import Json.Encode as E
import Page
import Request
import Shared
import Task
import Time
import TraversalList exposing (TraversalList)
import Tree
import TreeTest
import UI
import Url exposing (Protocol(..))
import View exposing (View)
import HTTPExt


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.element
        { init = init req.params.test
        , update = update req.params.test
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type Model
    = Loaded LoadedModel
    | Loading
    | LoadingFailed Http.Error


type alias LoadedModel =
    { rootNode : Tree.Node TreeTest.Item
    , selectedNode : Tree.ID
    , questions : TraversalList AnsweredTask
    , taskStarted : Bool
    , currentlyObserving : Maybe IncompleteObservation
    , observations : List Observation
    , id : String
    , sendingState : SendingState
    }


type SendingState
    = Sending
    | Failed Http.Error
    | Sent


sendResults : String -> List Observation -> Cmd Msg
sendResults id obs =
    Http.post
        { url = "http://127.0.0.1:25727/completed/tree-test/" ++ id
        , body = Http.jsonBody (E.list serializeObservation obs)
        , expect = Http.expectWhatever ResultsSent
        }


type alias AnsweredTask =
    { task : TreeTest.Task
    , answer : Tree.ID
    }


type alias PastSelection =
    { node : Tree.ID
    , at : Time.Posix
    }


type alias Observation =
    { question : AnsweredTask
    , pastSelections : List PastSelection
    , startedAt : Time.Posix
    , endedAt : Time.Posix
    }


type alias IncompleteObservation =
    { question : AnsweredTask
    , pastSelections : List PastSelection
    , startedAt : Time.Posix
    }


serializeTime : Time.Posix -> E.Value
serializeTime time =
    Time.posixToMillis time |> E.int


serializeAnsweredTask : AnsweredTask -> E.Value
serializeAnsweredTask answered =
    let
        (Tree.ID id) =
            answered.answer
    in
    E.object
        [ ( "task", TreeTest.encodeTask answered.task )
        , ( "answer", E.string id )
        ]


serializePastSelection : PastSelection -> E.Value
serializePastSelection selection =
    let
        (Tree.ID id) =
            selection.node
    in
    E.object
        [ ( "node", E.string id )
        , ( "at", serializeTime selection.at )
        ]


serializeObservation : Observation -> E.Value
serializeObservation observation =
    E.object
        [ ( "question", serializeAnsweredTask observation.question )
        , ( "pastSelections", E.list serializePastSelection observation.pastSelections )
        , ( "startedAt", serializeTime observation.startedAt )
        , ( "endedAt", serializeTime observation.endedAt )
        ]


completeObservation : IncompleteObservation -> AnsweredTask -> Time.Posix -> Observation
completeObservation incomplete newQuestion endedAt =
    Observation
        newQuestion
        incomplete.pastSelections
        incomplete.startedAt
        endedAt


init : String -> ( Model, Cmd Msg )
init id =
    ( Loading, TreeTest.getStudy id GotTreeTest )



-- UPDATE


type Msg
    = NodeClicked Tree.ID
    | NextQuestion
    | StartTask
    | Timestamped Msg Time.Posix
    | GotTreeTest (Result Http.Error TreeTest.Study)
    | ResultsSent (Result Http.Error ())


maybeAppend : List a -> Maybe a -> List a
maybeAppend list maybe =
    case maybe of
        Just a ->
            list ++ [ a ]

        _ ->
            list


makeModel : String -> TreeTest.Study -> LoadedModel
makeModel id treeTest =
    LoadedModel
        treeTest.tree
        (Tree.ID "")
        (TraversalList.make (List.map (\it -> AnsweredTask it (Tree.ID "")) treeTest.tasks))
        False
        Nothing
        []
        id
        Sending


update : String -> Msg -> Model -> ( Model, Cmd Msg )
update id msg model =
    case ( msg, model ) of
        ( _, Loaded it ) ->
            let
                ( new, cmd ) =
                    updateLoaded msg it
            in
            ( Loaded new, cmd )

        ( GotTreeTest (Ok test), Loading ) ->
            ( Loaded (makeModel id test), Cmd.none )

        ( GotTreeTest (Err err), Loading ) ->
            ( LoadingFailed err, Cmd.none )

        ( _, Loading ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


updateLoaded : Msg -> LoadedModel -> ( LoadedModel, Cmd Msg )
updateLoaded msg model =
    let
        updateQ : Tree.ID -> AnsweredTask -> AnsweredTask
        updateQ answer question =
            { question | answer = answer }

        updatedList =
            TraversalList.updateCurrent (updateQ model.selectedNode) model.questions

        currAns : TraversalList AnsweredTask -> Tree.ID
        currAns list =
            case TraversalList.toMaybe (TraversalList.current list) of
                Just a ->
                    a.answer

                _ ->
                    Tree.ID ""
    in
    case msg of
        Timestamped StartTask timestamp ->
            let
                currentlyObserving =
                    TraversalList.current model.questions
                        |> TraversalList.toMaybe
                        |> Maybe.map (\v -> IncompleteObservation v [] timestamp)
            in
            ( { model
                | taskStarted = True
                , currentlyObserving = currentlyObserving
              }
            , Cmd.none
            )

        Timestamped NextQuestion timestamp ->
            let
                newList =
                    TraversalList.next updatedList

                mald : Time.Posix -> IncompleteObservation -> AnsweredTask -> Observation
                mald time incomplete question =
                    completeObservation incomplete question time

                finishedObservation =
                    Maybe.map2 (mald timestamp)
                        model.currentlyObserving
                        (TraversalList.toMaybe <| TraversalList.current updatedList)

                newObservations =
                    maybeAppend model.observations finishedObservation
            in
            ( { model
                | taskStarted = False
                , observations = newObservations
                , questions = newList
                , selectedNode = currAns newList
              }
            , case TraversalList.current <| newList of
                TraversalList.AfterList ->
                    sendResults model.id newObservations

                _ ->
                    Cmd.none
            )

        Timestamped (NodeClicked node) timestamp ->
            let
                newObserving =
                    model.currentlyObserving
                        |> Maybe.map (\a -> { a | pastSelections = a.pastSelections ++ [ PastSelection node timestamp ] })
            in
            ( { model | selectedNode = node, currentlyObserving = newObserving }, Cmd.none )

        Timestamped _ _ ->
            ( model, Cmd.none )

        GotTreeTest _ ->
            ( model, Cmd.none )

        NodeClicked node ->
            ( model, Task.perform (Timestamped (NodeClicked node)) Time.now )

        NextQuestion ->
            ( model, Task.perform (Timestamped NextQuestion) Time.now )

        StartTask ->
            ( model, Task.perform (Timestamped StartTask) Time.now )

        ResultsSent (Ok ()) ->
            ( { model | sendingState = Sent }, Cmd.none )

        ResultsSent (Err err) ->
            ( { model | sendingState = Failed err }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


par : String -> Element msg
par txt =
    paragraph [] [ text txt ]


preTask : LoadedModel -> Element Msg
preTask _ =
    textColumn [ spacing 10, padding 24 ]
        [ el [ Font.bold ] (text "Welcome")
        , par "This is a study for TODO."
        , par "You will be asked to find an item that helps you with a given task from list of links."
        , par "Click through it until you find an item that you think helps you complete the given task."
        , par "If you make a wrong turn you can go back by clicking one of the links above."
        , par "Remember, this isn't a test of your ability. There are no right or wrong answers."
        , UI.button True "Get Started" NextQuestion
        ]


postTask : LoadedModel -> Element Msg
postTask model =
    textColumn [ padding 24 ]
        [ par "You're finished!"
        , par "Thank you for participating in our study."
        , case model.sendingState of
            Sending ->
                par "Sending results..."

            Failed err ->
                par <| "Sending results failed! " ++ HTTPExt.errorToString err

            Sent ->
                par "Results sent!"
        ]


taskCount : { a | questions : TraversalList b } -> String
taskCount model =
    let
        idx =
            TraversalList.index model.questions + 1

        count =
            TraversalList.length model.questions
    in
    "Task " ++ String.fromInt idx ++ " of " ++ String.fromInt count


header : LoadedModel -> AnsweredTask -> Element msg
header model question =
    textColumn [ spacing 8 ]
        [ el [ Font.bold ] (text <| taskCount model)
        , text question.task.text
        ]


atTask : LoadedModel -> AnsweredTask -> Element Msg
atTask model question =
    column [ width fill, padding 16, spacing 24 ]
        [ header model question
        , if model.taskStarted then
            viewEl model

          else
            UI.button True "Start Task" StartTask
        ]


view : Shared.Model -> Model -> View Msg
view shared model =
    case model of
        Loading ->
            { title = "Loading Tree Test"
            , element =
                UI.with shared
                    [ paragraph [ padding 24 ] [ text "Loading TreeTest..." ] ]
            }

        LoadingFailed err ->
            { title = "Loading Failed"
            , element =
                UI.with shared
                    [ textColumn [ padding 24 ] [ par "Oh no, loading failed!", par <| HTTPExt.errorToString err ] ]
            }

        Loaded it ->
            viewLoaded shared it


viewLoaded : Shared.Model -> LoadedModel -> View Msg
viewLoaded shared model =
    { title = "Tree Test"
    , element =
        UI.with shared
            [ case TraversalList.current model.questions of
                TraversalList.AtItem question ->
                    atTask model question

                TraversalList.AfterList ->
                    postTask model

                TraversalList.BeforeList ->
                    preTask model
            ]
    }


edges : { top : number, right : number, bottom : number, left : number }
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


signifier : LoadedModel -> Tree.Node TreeTest.Item -> Element Msg
signifier model (Tree.Node id cont _) =
    el
        [ Border.color <| rgb255 0xD1 0xD5 0xD9
        , Border.widthEach { edges | left = 2 }
        , Border.dotted
        , paddingEach { edges | left = 6, top = 2, bottom = 2 }
        ]
    <|
        el
            [ Background.color
                (if model.selectedNode == id then
                    rgb255 0 146 126

                 else
                    rgb255 0xD1 0xD5 0xD9
                )
            , Font.color
                (if model.selectedNode == id then
                    rgb255 255 255 255

                 else
                    rgb255 0 0 0
                )
            , Border.rounded 20
            , paddingXY 10 6
            , pointer
            , onClick (NodeClicked id)
            ]
            (text cont.text)


viewNode : LoadedModel -> Bool -> Tree.Node TreeTest.Item -> Element Msg
viewNode model isRoot ((Tree.Node id _ children) as node) =
    column
        [ paddingEach
            { edges
                | left =
                    if isRoot then
                        0

                    else
                        20
            }
        ]
        [ row
            [ spacing 20 ]
            [ signifier model node
            , if id == model.selectedNode && List.length children == 0 then
                UI.button True "I'd find it here" NextQuestion

              else
                none
            ]
        , column []
            (if model.selectedNode == id then
                List.map (viewNode model False) children
            else
                children
                |> List.filter (Tree.containsID model.selectedNode)
                |> List.map (viewNode model False))
        ]


viewEl : LoadedModel -> Element Msg
viewEl model =
    column [ spacing 10 ]
        [ viewNode model True model.rootNode
        ]
