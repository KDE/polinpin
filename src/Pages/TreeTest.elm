module Pages.TreeTest exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Gen.Params.TreeTest exposing (Params)
import Page
import Request
import Shared
import TraversalList exposing (TraversalList)
import UI
import View exposing (View)
import Time
import Task

page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { rootNode : Node
    , selectedNode : List String
    , questions : TraversalList Question
    , taskStarted : Bool
    , currentlyObserving : Maybe IncompleteObservation
    , observations : List Observation
    }


type Node
    = Node
        { label : String
        , id : String
        , children : List Node
        }


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

completeObservation incomplete newQuestion endedAt =
    Observation
        newQuestion
        incomplete.pastSelections
        incomplete.startedAt
        endedAt

mkNode : String -> String -> List Node -> Node
mkNode id label children =
    Node { id = id, label = label, children = children }


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


defaultRootNode : Node
defaultRootNode =
    mkNode "homepage"
        "Homepage"
        [ mkNode "shop" "Shop" []
        , mkNode "settings" "Settings" []
        , mkNode "account"
            "My Account"
            [ mkNode "upgrade" "Upgrade my plan" []
            , mkNode "profile" "Profile" []
            , mkNode "balance" "Account balance" []
            ]
        ]


defaultQuestions : TraversalList Question
defaultQuestions =
    TraversalList.make
        [ Question "Buy a jar." ["homepage", "shop"] []
        , Question "Check to see how to make the website gay." ["homepage", "settings"] []
        , Question "Update your name on the website." ["homepage", "account", "profile"] []
        ]


init : ( Model, Cmd Msg )
init =
    ( Model
        defaultRootNode
        []
        defaultQuestions
        False
        Nothing
        []
    , Cmd.none )



-- UPDATE


type Msg
    = NodeClicked (List String)
    | NextQuestion
    | StartTask
    | Timestamped Msg Time.Posix


updateQ : List String -> Question -> Question
updateQ mayhaps q =
    { q | answer = mayhaps }

maybeAppend : List a -> Maybe a -> List a
maybeAppend list maybe =
    case maybe of
        Just a ->
            list ++ [a]

        _ ->
            list

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
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
              }, Cmd.none
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
                        (model.currentlyObserving)
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
            , Cmd.none
            )

        Timestamped (NodeClicked node) timestamp ->
            let
                newObserving =
                    model.currentlyObserving
                    |> Maybe.map (\a -> { a | pastSelections = a.pastSelections ++ [PastSelection node timestamp] })
            in
            ( { model | selectedNode = node, currentlyObserving = newObserving }, Cmd.none )

        Timestamped _ _ ->
            ( model, Cmd.none )

        NodeClicked node ->
            ( model, Task.perform (Timestamped (NodeClicked node)) Time.now)

        NextQuestion ->
            ( model, Task.perform (Timestamped NextQuestion) Time.now )

        StartTask ->
            ( model, Task.perform (Timestamped StartTask) Time.now )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


par : String -> Element msg
par txt =
    paragraph [] [ text txt ]


preTask : Model -> Element Msg
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


postTask : Model -> Element Msg
postTask _ =
    textColumn [ padding 24 ]
        [ par "You're finished!"
        , par "Thank you for participating in our study."
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


atTask : Model -> Question -> Element Msg
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

signifier model parents node myPath =
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

viewNode : Model -> List String -> Node -> Element Msg
viewNode model parents node =
    let
        myPath =
            parents ++ [ nID node ]
    in
    column
        [ paddingEach
            { edges
            | left = if parents == [] then
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
                (node |> nChildren |> List.map (viewNode model <| myPath))
            else
                [])
        ]


viewEl : Model -> Element Msg
viewEl model =
    column [ spacing 10 ]
        [ viewNode model [] model.rootNode
        ]
