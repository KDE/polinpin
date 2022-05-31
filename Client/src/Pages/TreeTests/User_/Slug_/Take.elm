module Pages.TreeTests.User_.Slug_.Take exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Gen.Params.TreeTests.User_.Slug_.Take exposing (Params)
import Http
import Network
import Page
import Request
import Shared
import SharedUI
import Task
import Time
import TraversalList exposing (TraversalList)
import TreeManipulation
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.advanced
        { init = init req.params
        , update = update req.params
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type Model
    = Loading
    | Loaded LoadedModel
    | Failed Http.Error


init : Params -> ( Model, Effect Msg )
init params =
    ( Loading, Effect.fromCmd <| Network.treeTestWithoutAuth Nothing params.user params.slug ResultGot )



-- UPDATE


type Msg
    = ResultGot (Result Http.Error Network.TreeTestStudyData)
    | InnerMsg LoadedMsg


wrapLoaded : ( LoadedModel, Effect LoadedMsg ) -> ( Model, Effect Msg )
wrapLoaded =
    Tuple.mapFirst Loaded >> Tuple.mapSecond (Effect.map InnerMsg)


update : Params -> Msg -> Model -> ( Model, Effect Msg )
update params msg model =
    case msg of
        ResultGot (Ok data) ->
            initLoaded data |> wrapLoaded

        ResultGot (Err why) ->
            ( Failed why, Effect.none )

        InnerMsg imsg ->
            case model of
                Loaded it ->
                    updateLoaded params imsg it |> wrapLoaded

                _ ->
                    ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    case model of
        Loading ->
            viewLoading shared

        Failed err ->
            viewFailed shared err

        Loaded imodel ->
            viewLoaded shared imodel |> View.map InnerMsg


viewLoading : Shared.Model -> View Msg
viewLoading shared =
    SharedUI.sharedFrame shared <|
        View.placeholder "Loading"


viewFailed : Shared.Model -> Http.Error -> View Msg
viewFailed shared err =
    SharedUI.sharedFrame shared <|
        View.placeholder "Failed to load study... :/"



-- INIT LOADED


type alias LoadedModel =
    { studyData : Network.StudyData
    , tree : Network.TreeNode Network.TreeStudyItem
    , tasks : TraversalList ( Network.TreeStudyTask, Network.TreeTestAnsweredQuestion )
    , save : Network.RequestStatus ()
    }


toPair : Network.TreeStudyTask -> ( Network.TreeStudyTask, Network.TreeTestAnsweredQuestion )
toPair task =
    ( task, Network.TreeTestAnsweredQuestion task.id "" [] -1 -1 )


updateStartTime : Time.Posix -> Network.TreeTestAnsweredQuestion -> Network.TreeTestAnsweredQuestion
updateStartTime posix answer =
    { answer | startedAt = Time.posixToMillis posix }


updateEndTime : Time.Posix -> Network.TreeTestAnsweredQuestion -> Network.TreeTestAnsweredQuestion
updateEndTime posix answer =
    { answer | endedAt = Time.posixToMillis posix }


addPastSelection : Time.Posix -> Network.TreeTestAnsweredQuestion -> Network.TreeTestAnsweredQuestion
addPastSelection posix answer =
    if answer.answer == "" then
        answer

    else
        { answer
            | pastSelections =
                answer.pastSelections
                    ++ [ Network.TreeTestPastSelection answer.answer (Time.posixToMillis posix)
                       ]
        }


initLoaded : Network.TreeTestStudyData -> ( LoadedModel, Effect LoadedMsg )
initLoaded data =
    ( { studyData = data.studyData
      , tree = data.tree
      , tasks = data.tasks |> List.map toPair |> TraversalList.make
      , save = Network.NoRequest
      }
    , Effect.none
    )



-- UPDATE LOADED


type LoadedMsg
    = TimestampedStartTask Time.Posix
    | TimestampedNextQuestion Time.Posix
    | TimestampedNodeClicked String Time.Posix
    | StartTask
    | NextQuestion
    | NodeClicked String
    | ResponseSubmitResult (Result Http.Error ())
    | RetrySubmit


updateLoaded : Params -> LoadedMsg -> LoadedModel -> ( LoadedModel, Effect LoadedMsg )
updateLoaded params msg model =
    case msg of
        StartTask ->
            ( model, Effect.fromCmd <| Task.perform TimestampedStartTask Time.now )

        NextQuestion ->
            ( model, Effect.fromCmd <| Task.perform TimestampedNextQuestion Time.now )

        NodeClicked node ->
            ( model, Effect.fromCmd <| Task.perform (TimestampedNodeClicked node) Time.now )

        TimestampedNextQuestion when ->
            let
                tasks =
                    model.tasks
                        |> TraversalList.mapCurrent (Tuple.mapSecond (updateEndTime when))
                        |> TraversalList.next
                        |> TraversalList.mapCurrent (Tuple.mapSecond (updateStartTime when))

                justTasks =
                    tasks
                        |> TraversalList.toList
                        |> List.map Tuple.second

                ( nmodel, effect ) =
                    if TraversalList.current tasks == TraversalList.AfterList then
                        ( { model | save = Network.PendingRequest }
                        , Network.treeStudySubmitObservation
                            Nothing
                            params.user
                            params.slug
                            (Network.TreeTestObservation justTasks)
                            ResponseSubmitResult
                            |> Effect.fromCmd
                        )

                    else
                        ( model, Effect.none )
            in
            ( { nmodel | tasks = tasks }, effect )

        TimestampedStartTask when ->
            let
                tasks =
                    model.tasks
                        |> TraversalList.next
                        |> TraversalList.mapCurrent (Tuple.mapSecond (updateStartTime when))
            in
            ( { model | tasks = tasks }, Effect.none )

        TimestampedNodeClicked node when ->
            let
                tasks =
                    model.tasks
                        |> TraversalList.mapCurrent (Tuple.mapSecond (addPastSelection when))
                        |> TraversalList.mapCurrent (Tuple.mapSecond (\k -> { k | answer = node }))
            in
            ( { model | tasks = tasks }, Effect.none )

        ResponseSubmitResult res ->
            ( { model | save = Network.ResultReceived res }, Effect.none )

        RetrySubmit ->
            let
                justTasks =
                    model.tasks
                        |> TraversalList.toList
                        |> List.map Tuple.second
            in
            ( model
            , Network.treeStudySubmitObservation
                Nothing
                params.user
                params.slug
                (Network.TreeTestObservation justTasks)
                ResponseSubmitResult
                |> Effect.fromCmd
            )



-- VIEW LOADED


par : String -> Element msg
par txt =
    paragraph [] [ text txt ]


preTask : Element LoadedMsg
preTask =
    textColumn [ spacing 10, padding 24, width fill ]
        [ el [ Font.bold, Font.size <| UI.intScale 2 ] (text "Welcome")
        , par "This is a study for KDE, to help us make our software easier to use."
        , par "You will be asked to find an item that helps you with a given task from a list of items."
        , par "Click through it until you find an item that you think helps you complete the given task."
        , par "If you make a wrong turn you can go back by clicking one of the items above."
        , par "Remember, this isn't a test of your ability. There are no right or wrong answers."
        , UI.textButton (Just NextQuestion) [ alignLeft ] "Get Started"
        ]


postTask : LoadedModel -> Element LoadedMsg
postTask model =
    textColumn [ spacing 10, padding 24, width fill ]
        [ el [ Font.bold, Font.size <| UI.intScale 2 ] (text "All Done!")
        , par "You're finished!."
        , par "Thank you for participating in our study."
        , case model.save of
            Network.PendingRequest ->
                par "Submitting..."

            Network.NoRequest ->
                par "Getting ready to submit..."

            Network.ResultReceived (Ok _) ->
                par "Submitted!"

            Network.ResultReceived (Err _) ->
                column [ spacing 10 ] [ par "Failed to save!", UI.textButton (Just RetrySubmit) [] "Try again" ]
        ]


viewLoaded : Shared.Model -> LoadedModel -> View LoadedMsg
viewLoaded shared model =
    SharedUI.sharedFrame shared <|
        { title = "Taking " ++ model.studyData.title
        , body =
            case TraversalList.current model.tasks of
                TraversalList.BeforeList ->
                    preTask

                TraversalList.AtItem ( task, answer ) ->
                    viewTask model task answer

                TraversalList.AfterList ->
                    postTask model
        , over = Nothing
        }


viewNode : LoadedModel -> Bool -> Network.TreeTestAnsweredQuestion -> Network.TreeNode Network.TreeStudyItem -> Element LoadedMsg
viewNode model isRoot answer (Network.TreeNode id data children) =
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
        , spacing 10
        ]
        (wrappedRow [ spacing 20 ]
            [ UI.textButton (Just (NodeClicked id)) [] data.text
            , if answer.answer == id && List.length children == 0 then
                UI.smallTealTextButton (Just NextQuestion) [] "I'd find it here"

              else
                none
            ]
            :: (if answer.answer == id then
                    List.map (viewNode model False answer) children

                else
                    children
                        |> List.filter (TreeManipulation.containsNodeWithID answer.answer)
                        |> List.map (viewNode model False answer)
               )
        )


viewTask : LoadedModel -> Network.TreeStudyTask -> Network.TreeTestAnsweredQuestion -> Element LoadedMsg
viewTask model task answer =
    column
        [ padding 24, spacing 20 ]
        [ text task.text
        , viewNode model True answer model.tree
        ]
