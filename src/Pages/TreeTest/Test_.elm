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
import UI
import Url exposing (Protocol(..))
import View exposing (View)


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
    { rootNode : Node
    , selectedNode : List String
    , questions : TraversalList Question
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


type alias TreeTest =
    { node : Node
    , questions : List Question
    }


deserializeTreeTest : D.Decoder TreeTest
deserializeTreeTest =
    D.map2 TreeTest
        (D.field "node" deserializeNode)
        (D.field "question" (D.list deserializeQuestion))


type Node
    = Node
        { label : String
        , id : String
        , children : List Node
        }


type alias Mald =
    { label : String, id : String, children : List Node }


getRootNodeForTest : String -> Cmd Msg
getRootNodeForTest id =
    Http.get
        { url = "http://127.0.0.1:25727/tree-test/" ++ id
        , expect = Http.expectJson GotTreeTest deserializeTreeTest
        }


sendResults : String -> List Observation -> Cmd Msg
sendResults id obs =
    Http.post
        { url = "http://127.0.0.1:25727/completed/tree-test/" ++ id
        , body = Http.jsonBody (E.list serializeObservation obs)
        , expect = Http.expectWhatever ResultsSent
        }


deserializeNode : D.Decoder Node
deserializeNode =
    D.map Node <|
        D.map3 Mald
            (D.field "label" D.string)
            (D.field "id" D.string)
            (D.field "children"
                (D.oneOf
                    [ D.null []
                    , D.list (D.lazy (\_ -> deserializeNode))
                    ]
                )
            )


type alias Question =
    { text : String
    , correctAnswer : List String
    , answer : List String
    }


type alias PastSelection =
    { node : List String
    , at : Time.Posix
    }


type alias Observation =
    { question : Question
    , pastSelections : List PastSelection
    , startedAt : Time.Posix
    , endedAt : Time.Posix
    }


type alias IncompleteObservation =
    { question : Question
    , pastSelections : List PastSelection
    , startedAt : Time.Posix
    }


serializeTime : Time.Posix -> E.Value
serializeTime time =
    Time.posixToMillis time |> E.int


serializeQuestion : Question -> E.Value
serializeQuestion question =
    E.object
        [ ( "text", E.string question.text )
        , ( "correctAnswer", E.list E.string question.correctAnswer )
        , ( "answer", E.list E.string question.answer )
        ]


deserializeQuestion : D.Decoder Question
deserializeQuestion =
    D.map3 Question
        (D.field "text" D.string)
        (D.field "correctAnswer" (D.list D.string))
        (D.maybe (D.field "answer" (D.list D.string))
            |> D.map (Maybe.withDefault [])
        )


serializePastSelection : PastSelection -> E.Value
serializePastSelection selection =
    E.object
        [ ( "node", E.list E.string selection.node )
        , ( "at", serializeTime selection.at )
        ]


serializeObservation : Observation -> E.Value
serializeObservation observation =
    E.object
        [ ( "question", serializeQuestion observation.question )
        , ( "pastSelections", E.list serializePastSelection observation.pastSelections )
        , ( "startedAt", serializeTime observation.startedAt )
        , ( "endedAt", serializeTime observation.endedAt )
        ]


completeObservation : IncompleteObservation -> Question -> Time.Posix -> Observation
completeObservation incomplete newQuestion endedAt =
    Observation
        newQuestion
        incomplete.pastSelections
        incomplete.startedAt
        endedAt


nLabel : Node -> String
nLabel node =
    case node of
        Node { label } ->
            label


nChildren : Node -> List Node
nChildren node =
    case node of
        Node { children } ->
            children


nID : Node -> String
nID node =
    case node of
        Node { id } ->
            id


init : String -> ( Model, Cmd Msg )
init id =
    ( Loading, getRootNodeForTest id )



-- UPDATE


type Msg
    = NodeClicked (List String)
    | NextQuestion
    | StartTask
    | Timestamped Msg Time.Posix
    | GotTreeTest (Result Http.Error TreeTest)
    | ResultsSent (Result Http.Error ())


updateQ : List String -> Question -> Question
updateQ mayhaps q =
    { q | answer = mayhaps }


maybeAppend : List a -> Maybe a -> List a
maybeAppend list maybe =
    case maybe of
        Just a ->
            list ++ [ a ]

        _ ->
            list


makeModel : String -> TreeTest -> LoadedModel
makeModel id treeTest =
    LoadedModel
        treeTest.node
        []
        (TraversalList.make treeTest.questions)
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

        ( GotTreeTest (Err _), Loading ) ->
            ( model, Cmd.none )

        ( _, Loading ) ->
            ( model, Cmd.none )

        ( _, _ ) ->
            ( model, Cmd.none )


updateLoaded : Msg -> LoadedModel -> ( LoadedModel, Cmd Msg )
updateLoaded msg model =
    let
        updatedList =
            TraversalList.updateCurrent (updateQ model.selectedNode) model.questions

        currAns list =
            case TraversalList.toMaybe (TraversalList.current list) of
                Just a ->
                    a.answer

                _ ->
                    []
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

                mald : Time.Posix -> IncompleteObservation -> Question -> Observation
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

        GotTreeTest (Ok value) ->
            let
                _ =
                    Debug.log "mald" value
            in
            ( model, Cmd.none )

        GotTreeTest (Err (Http.BadBody body)) ->
            let
                _ =
                    Debug.log body 1
            in
            ( model, Cmd.none )

        GotTreeTest (Err _) ->
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

            Failed _ ->
                par "Sending results failed!"

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


header : { a | questions : TraversalList b } -> { c | text : String } -> Element msg
header model question =
    textColumn [ spacing 8 ]
        [ el [ Font.bold ] (text <| taskCount model)
        , text question.text
        ]


atTask : LoadedModel -> Question -> Element Msg
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

        LoadingFailed _ ->
            { title = "Loading Failed"
            , element =
                UI.with shared
                    [ paragraph [ padding 24 ] [ text "Oh no, loading failed!" ] ]
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


isPrefix : List a -> List a -> Bool
isPrefix a b =
    List.take (List.length a) b == a


signifier : LoadedModel -> b -> Node -> List String -> Element Msg
signifier model _ node myPath =
    el
        [ Background.color
            (if model.selectedNode == myPath then
                rgb255 0 0 0

             else
                rgb255 255 255 255
            )
        , Font.color
            (if model.selectedNode == myPath then
                rgb255 255 255 255

             else
                rgb255 0 0 0
            )
        , Border.color <| rgb255 100 100 100
        , Border.widthEach { edges | left = 2 }
        , padding 6
        , pointer
        , onClick (NodeClicked <| myPath)
        ]
        (text (node |> nLabel))


viewNode : LoadedModel -> List String -> Node -> Element Msg
viewNode model parents node =
    let
        myPath =
            parents ++ [ nID node ]
    in
    column
        [ paddingEach
            { edges
                | left =
                    if parents == [] then
                        0

                    else
                        20
            }
        ]
        [ row
            [ spacing 20 ]
            [ signifier model parents node myPath
            , if myPath == model.selectedNode && List.length (node |> nChildren) == 0 then
                UI.button True "I'd find it here" NextQuestion

              else
                none
            ]
        , column []
            (if isPrefix myPath model.selectedNode then
                node |> nChildren |> List.map (viewNode model <| myPath)

             else
                []
            )
        ]


viewEl : LoadedModel -> Element Msg
viewEl model =
    column [ spacing 10 ]
        [ viewNode model [] model.rootNode
        ]
