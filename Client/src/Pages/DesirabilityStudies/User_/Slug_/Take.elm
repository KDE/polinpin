module Pages.DesirabilityStudies.User_.Slug_.Take exposing (Model, Msg, page)

import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Gen.Params.DesirabilityStudies.User_.Slug_.Take exposing (Params)
import Http
import List.Extra
import Network
import Page
import Request
import Shared
import SharedUI
import TraversalList exposing (TraversalList)
import UI exposing (par)
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
    ( Loading, Effect.fromCmd <| Network.desirabilityStudyWithoutAuth Nothing params.user params.slug LoadResult )



-- UPDATE


type Msg
    = InnerLoaded LoadedMsg
    | LoadResult (Result Http.Error Network.DesirabilityStudyData)


update : Params -> Msg -> Model -> ( Model, Effect Msg )
update params msg model =
    case ( model, msg ) of
        ( Loaded innerModel, InnerLoaded innerMsg ) ->
            updateLoaded params innerMsg innerModel
                |> Tuple.mapBoth Loaded (Effect.map InnerLoaded)

        ( _, LoadResult (Ok ok) ) ->
            initLoaded ok
                |> Tuple.mapBoth Loaded (Effect.map InnerLoaded)

        ( _, LoadResult (Err why) ) ->
            ( Failed why, Effect.none )

        _ ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loaded it ->
            subscriptionsLoaded it |> Sub.map InnerLoaded

        _ ->
            Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    case model of
        Loaded it ->
            viewLoaded shared it
                |> View.map InnerLoaded

        Failed _ ->
            SharedUI.sharedFrame shared (View.placeholder "Failed to load")

        Loading ->
            SharedUI.sharedFrame shared (View.placeholder "Loading")



-- INIT LOADED


type alias LoadedModel =
    { studyData : Network.StudyData
    , wordBank : List Network.DesirabilityStudyWord
    , items : TraversalList ( Network.DesirabilityStudyItem, Network.DesirabilityStudyWordResponse )
    , numberOfWordsToSelect : Int
    , submit : Network.RequestStatus ()
    }


swap : ( a, b ) -> ( b, a )
swap ( a, b ) =
    ( b, a )


initLoaded : Network.DesirabilityStudyData -> ( LoadedModel, Effect LoadedMsg )
initLoaded data =
    ( LoadedModel
        data.studyData
        data.wordBank
        (data.items
            |> List.map (Tuple.pair (Network.DesirabilityStudyWordResponse [] -1) >> swap)
            |> TraversalList.make
        )
        data.numberOfWordsToSelect
        Network.NoRequest
    , Effect.none
    )



-- UPDATE LOADED


type LoadedMsg
    = NextQuestion
    | AddWord String
    | RemoveWord String
    | SetRating Int
    | Submitted (Result Http.Error ())
    | RetrySubmit


updateLoaded : Params -> LoadedMsg -> LoadedModel -> ( LoadedModel, Effect LoadedMsg )
updateLoaded params msg model =
    case msg of
        NextQuestion ->
            let
                newItems =
                    TraversalList.next model.items

                atEnd =
                    TraversalList.current newItems == TraversalList.AfterList

                ( nmodel, effect ) =
                    if atEnd then
                        ( { model | submit = Network.PendingRequest }
                        , Effect.fromCmd <|
                            Network.desirabilityStudySubmitObservation
                                Nothing
                                params.user
                                params.slug
                                (newItems |> TraversalList.toList |> List.map Tuple.second)
                                Submitted
                        )

                    else
                        ( model, Effect.none )
            in
            ( { nmodel | items = newItems }, effect )

        AddWord word ->
            let
                newItems =
                    model.items
                        |> TraversalList.mapCurrent
                            (Tuple.mapSecond (\k -> { k | words = word :: k.words }))

                currentCount =
                    model.items
                        |> TraversalList.current
                        |> TraversalList.toMaybe
                        |> Maybe.map Tuple.second
                        |> Maybe.map .words
                        |> Maybe.map List.length
                        |> Maybe.withDefault -1
            in
            ( { model
                | items =
                    if currentCount == model.numberOfWordsToSelect then
                        model.items

                    else
                        newItems
              }
            , Effect.none
            )

        RemoveWord word ->
            let
                newItems =
                    model.items
                        |> TraversalList.mapCurrent
                            (Tuple.mapSecond (\k -> { k | words = List.Extra.remove word k.words }))
            in
            ( { model | items = newItems }, Effect.none )

        SetRating num ->
            let
                newItems =
                    model.items
                        |> TraversalList.mapCurrent
                            (Tuple.mapSecond (\k -> { k | rating = num }))
            in
            ( { model | items = newItems }, Effect.none )

        Submitted res ->
            ( { model | submit = Network.ResultReceived res }, Effect.none )

        RetrySubmit ->
            ( { model | submit = Network.PendingRequest }
            , Effect.fromCmd <|
                Network.desirabilityStudySubmitObservation
                    Nothing
                    params.user
                    params.slug
                    (model.items |> TraversalList.toList |> List.map Tuple.second)
                    Submitted
            )



-- SUBSCRIPTIONS LOADED


subscriptionsLoaded : LoadedModel -> Sub LoadedMsg
subscriptionsLoaded _ =
    Sub.none



-- VIEW LOADED


viewLoaded : Shared.Model -> LoadedModel -> View LoadedMsg
viewLoaded shared model =
    SharedUI.sharedFrame shared
        { title = "Taking " ++ model.studyData.title
        , body =
            case TraversalList.current model.items of
                TraversalList.AtItem item ->
                    viewItem model item

                TraversalList.BeforeList ->
                    preTask model

                TraversalList.AfterList ->
                    postTask model
        , over = Nothing
        }


preTask : LoadedModel -> Element LoadedMsg
preTask model =
    textColumn [ spacing 10, padding 24, width fill ]
        [ el [ Font.bold, Font.size <| UI.intScale 2 ] (text "Welcome")
        , par "This is a study for KDE, to help us make our software more appealing."
        , par "You will be presented an image, and a list of words."
        , par "Click words that describe how you see the image to move them to the right list."
        , par "Click words in the right list to move them back to the word bank."
        , par <| "You will need " ++ String.fromInt model.numberOfWordsToSelect ++ " words in the right list."
        , par "Afterwards, you will be asked to rate the image's overall appeal on a scale of 1-7."
        , UI.textButton (Just NextQuestion) [ alignLeft ] "Get Started"
        ]


viewItem : LoadedModel -> ( Network.DesirabilityStudyItem, Network.DesirabilityStudyWordResponse ) -> Element LoadedMsg
viewItem model ( item, response ) =
    column [ centerX, width (fill |> maximum 700), spacing 20 ]
        [ UI.grayBox [ centerX, width fill ]
            (column [ padding 20, spacing 20, width fill ]
                [ image [ width fill, centerX ]
                    { src = Network.imagePath item.imageFileID
                    , description = ""
                    }
                , el [ centerX ] (text item.description)
                ]
            )
        , if List.length response.words == model.numberOfWordsToSelect && response.rating /= -1 then
            UI.textButton (Just NextQuestion) [ width fill ] "Next Image"

          else
            UI.textButton Nothing [ width fill, alpha 0.5 ] "Next Image"
        , UI.grayBox [ width fill, padding 20 ] <|
            row [ centerX, spacing 10 ]
                (List.map
                    (\num ->
                        (if response.rating == num then
                            UI.tealTextButton

                         else
                            UI.textButton
                        )
                            (Just (SetRating num))
                            []
                            (String.fromInt num)
                    )
                    (List.range 1 7)
                )
        , row [ width fill, spacing 20 ]
            [ UI.grayBox [ width (fillPortion 1), padding 20, alignTop ] <|
                column [ width fill, spacing 10 ]
                    (model.wordBank
                        |> List.filter (\k -> not (List.member k.word response.words))
                        |> List.map .word
                        |> (List.map <| \k -> UI.textButton (Just (AddWord k)) [ width fill ] k)
                    )
            , UI.grayBox [ width (fillPortion 1), padding 20, alignTop ] <|
                column [ width fill, spacing 10 ]
                    (response.words
                        |> (List.map <| \k -> UI.textButton (Just (RemoveWord k)) [ width fill ] k)
                    )
            ]
        ]

postTask : LoadedModel -> Element LoadedMsg
postTask model =
    textColumn [ spacing 10, padding 24, width fill ]
        [ el [ Font.bold, Font.size <| UI.intScale 2 ] (text "All Done!")
        , par "You're finished!."
        , par "Thank you for participating in our study."
        , case model.submit of
            Network.PendingRequest ->
                par "Submitting..."

            Network.NoRequest ->
                par "Getting ready to submit..."

            Network.ResultReceived (Ok _) ->
                par "Submitted!"

            Network.ResultReceived (Err _) ->
                column [ spacing 10 ] [ par "Failed to save!", UI.textButton (Just RetrySubmit) [] "Try again" ]
        ]
