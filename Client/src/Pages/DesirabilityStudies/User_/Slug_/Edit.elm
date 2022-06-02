module Pages.DesirabilityStudies.User_.Slug_.Edit exposing (Model, Msg, page)

import Auth
import Browser.Navigation
import Effect exposing (Effect)
import Element exposing (..)
import Element.Font as Font
import Element.Input as Input
import File exposing (File)
import File.Select as Select
import Gen.Params.DesirabilityStudies.User_.Slug_.Edit exposing (Params)
import Http
import List.Extra
import Network
import Page
import Request
import Round
import Set
import Shared
import SharedUI
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared req =
    Page.protected.advanced
        (\user ->
            { init = init user req.params
            , update = update user req.params shared
            , view = view shared
            , subscriptions = subscriptions
            }
        )



-- INIT


type Model
    = Loaded LoadedModel
    | Loading
    | Failed Http.Error
    | LoadingResults Network.DesirabilityStudyData
    | LoadedResults ResultsModel


init : Auth.User -> Params -> ( Model, Effect Msg )
init user params =
    ( Loading
    , Effect.fromCmd <|
        Network.desirabilityStudy
            (Network.UserSession user.token)
            params.user
            params.slug
            GotLoadedResult
    )



-- UPDATE


type Msg
    = InnerLoadedMsg LoadedMsg
    | InnerResultsMsg ResultsMsg
    | GotLoadedResult (Result Http.Error Network.DesirabilityStudyData)
    | GotResultsResult (Result Http.Error Network.DesirabilityStudyResults)


update : Auth.User -> Params -> Shared.Model -> Msg -> Model -> ( Model, Effect Msg )
update user params shared msg model =
    case ( model, msg ) of
        ( Loaded innerModel, InnerLoadedMsg innerMsg ) ->
            updateLoaded user params shared innerMsg innerModel
                |> Tuple.mapBoth Loaded (Effect.map InnerLoadedMsg)

        ( LoadedResults innerModel, InnerResultsMsg innerMsg ) ->
            updateResults user params shared innerMsg innerModel
                |> Tuple.mapBoth LoadedResults (Effect.map InnerResultsMsg)

        ( _, GotLoadedResult (Ok ok) ) ->
            if ok.studyData.published then
                ( LoadingResults ok
                , Effect.fromCmd <|
                    Network.desirabilityStudyResults
                        (Network.UserSession user.token)
                        params.user
                        params.slug
                        GotResultsResult
                )

            else
                initLoaded ok
                    |> Tuple.mapBoth Loaded (Effect.map InnerLoadedMsg)

        ( _, GotLoadedResult (Err why) ) ->
            ( Failed why, Effect.none )

        ( LoadingResults data, GotResultsResult (Ok ok) ) ->
            initResults data ok
                |> Tuple.mapBoth LoadedResults (Effect.map InnerResultsMsg)

        ( _, GotResultsResult (Err why) ) ->
            ( Failed why, Effect.none )

        _ ->
            ( model, Effect.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    case model of
        Loaded inner ->
            subscriptionsLoaded inner
                |> Sub.map InnerLoadedMsg

        LoadedResults inner ->
            subscriptionsResults inner
                |> Sub.map InnerResultsMsg

        _ ->
            Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    case model of
        Loading ->
            SharedUI.sharedFrame shared <|
                View.placeholder "Loading..."

        Loaded inner ->
            viewLoaded shared inner
                |> View.map InnerLoadedMsg

        LoadedResults inner ->
            viewResults shared inner
                |> View.map InnerResultsMsg

        LoadingResults _ ->
            SharedUI.sharedFrame shared <|
                View.placeholder "Loading results..."

        Failed _ ->
            SharedUI.sharedFrame shared <|
                View.placeholder "Failed to load :/"



-- INIT LOADED


type alias LoadedModel =
    { studyData : Network.StudyData
    , wordBank : List Network.DesirabilityStudyWord
    , wordTags : List Network.DesirabilityStudyWordTag
    , items : List Network.DesirabilityStudyItem
    , numberOfWordsToSelect : Int
    , tab : LoadedTab
    }


initLoaded : Network.DesirabilityStudyData -> ( LoadedModel, Effect LoadedMsg )
initLoaded data =
    ( LoadedModel data.studyData data.wordBank data.wordTags data.items data.numberOfWordsToSelect WordBank, Effect.none )



-- UPDATE LOADED


type LoadedMsg
    = LoadedTabClicked LoadedTab
    | UpdateWord Int String
    | AddTag Int String
    | RemoveTag Int String
    | UpdateTagName Int String
    | UpdateTagDescription Int String
    | UpdateItemDescription Int String
    | NewItem
    | GotFile File
    | FileUploadResult (Result Http.Error String)
    | Save
    | SaveResult (Result Http.Error ())
    | Publish
    | PublishResult (Result Http.Error ())
    | NewTag
    | NewWord


type LoadedTab
    = WordBank
    | WordTags
    | Items


updateLoaded : Auth.User -> Params -> Shared.Model -> LoadedMsg -> LoadedModel -> ( LoadedModel, Effect LoadedMsg )
updateLoaded user params shared msg model =
    case msg of
        LoadedTabClicked tab ->
            ( { model | tab = tab }, Effect.none )

        UpdateWord num text ->
            ( { model | wordBank = List.Extra.updateAt num (\k -> { k | word = text }) model.wordBank }, Effect.none )

        AddTag num text ->
            ( { model | wordBank = List.Extra.updateAt num (\k -> { k | tags = text :: k.tags }) model.wordBank }, Effect.none )

        RemoveTag num text ->
            ( { model | wordBank = List.Extra.updateAt num (\k -> { k | tags = List.Extra.remove text k.tags }) model.wordBank }, Effect.none )

        UpdateTagName num text ->
            let
                oldName =
                    List.Extra.getAt num model.wordTags |> Maybe.map .tag |> Maybe.withDefault ""

                newWordBank =
                    model.wordBank
                        |> List.map
                            (\k ->
                                { k
                                    | tags =
                                        k.tags
                                            |> List.Extra.setIf ((==) oldName) text
                                }
                            )

                newTags =
                    List.Extra.updateAt num (\k -> { k | tag = text }) model.wordTags
            in
            ( { model | wordBank = newWordBank, wordTags = newTags }, Effect.none )

        UpdateTagDescription num text ->
            ( { model | wordTags = List.Extra.updateAt num (\k -> { k | description = text }) model.wordTags }, Effect.none )

        UpdateItemDescription num text ->
            ( { model | items = List.Extra.updateAt num (\k -> { k | description = text }) model.items }, Effect.none )

        NewItem ->
            ( model, Effect.fromCmd <| Select.file [ "image/png", "image/jpeg" ] GotFile )

        GotFile fileToUpload ->
            ( model, Effect.fromCmd <| Network.uploadFile (Network.UserSession user.token) fileToUpload FileUploadResult )

        FileUploadResult (Ok fileID) ->
            ( { model | items = List.append model.items [ Network.DesirabilityStudyItem fileID "" ] }, Effect.none )

        NewTag ->
            ( { model | wordTags = List.append model.wordTags [ Network.DesirabilityStudyWordTag "" "" ] }, Effect.none )

        NewWord ->
            ( { model | wordBank = List.append model.wordBank [ Network.DesirabilityStudyWord "" [] ] }, Effect.none )

        FileUploadResult (Err why) ->
            ( model, Effect.none )

        Save ->
            ( model
            , Effect.fromCmd <|
                Network.saveDesirabilityStudy
                    (Network.UserSession user.token)
                    params.user
                    params.slug
                    model.wordBank
                    model.wordTags
                    model.items
                    model.numberOfWordsToSelect
                    SaveResult
            )

        SaveResult _ ->
            ( model, Effect.none )

        Publish ->
            ( model
            , Effect.fromCmd <|
                Network.publishStudy
                    (Network.UserSession user.token)
                    params.user
                    params.slug
                    PublishResult
            )

        PublishResult (Ok _) ->
            ( model, Effect.fromCmd <| Browser.Navigation.reload )

        PublishResult (Err _) ->
            ( model, Effect.none )



-- SUBSCRIPTIONS LOADED


subscriptionsLoaded : LoadedModel -> Sub LoadedMsg
subscriptionsLoaded model =
    Sub.none



-- VIEW LOADED


viewLoaded : Shared.Model -> LoadedModel -> View LoadedMsg
viewLoaded shared model =
    SharedUI.sharedFrame shared
        { title = "Viewing " ++ model.studyData.title
        , body = bodyLoaded shared model
        , over = Nothing
        }


bodyLoaded : Shared.Model -> LoadedModel -> Element LoadedMsg
bodyLoaded _ model =
    column [ padding 10, spacing 20, width fill ]
        [ row [ spacing 20, width fill ]
            [ SharedUI.tabButton model.tab WordBank LoadedTabClicked "Word Bank"
            , SharedUI.tabButton model.tab WordTags LoadedTabClicked "Word Tags"
            , SharedUI.tabButton model.tab Items LoadedTabClicked "Items"
            , UI.textButton (Just Publish) [ alignRight ] "Publish"
            , UI.textButton (Just Save) [ alignRight ] "Save"
            ]
        , case model.tab of
            WordBank ->
                viewWordBank model

            WordTags ->
                viewWordTags model

            Items ->
                viewItems model
        ]


viewWordBank : LoadedModel -> Element LoadedMsg
viewWordBank ({ wordBank } as model) =
    column [ spacing 20, width (fill |> maximum 500) ]
        (List.indexedMap (viewWord model) wordBank
            ++ [ UI.textButton (Just NewWord) [ width fill ] "New Word"
               ]
        )


viewWord : LoadedModel -> Int -> Network.DesirabilityStudyWord -> Element LoadedMsg
viewWord model num word =
    UI.grayBox [ width fill, padding 20 ] <|
        column [ width fill, spacing 10 ]
            [ UI.textInput [ width fill ]
                { onChange = UpdateWord num
                , text = word.word
                , placeholder = Just (Input.placeholder [] (text "Word that describes a user's feeling"))
                , label = Input.labelHidden "word"
                }
            , wrappedRow [ width fill, spacing 10 ]
                (let
                    wordTag : Network.DesirabilityStudyWordTag -> Element LoadedMsg
                    wordTag tag =
                        if List.member tag.tag word.tags then
                            UI.smallTealTextButton (Just (RemoveTag num tag.tag)) [] tag.tag

                        else
                            UI.smallTextButton (Just (AddTag num tag.tag)) [] tag.tag
                 in
                 List.map wordTag model.wordTags
                )
            ]


viewWordTags : LoadedModel -> Element LoadedMsg
viewWordTags ({ wordTags } as model) =
    column [ spacing 20, width (fill |> maximum 500) ]
        (List.indexedMap (viewWordTag model) wordTags
            ++ [ UI.textButton (Just NewTag) [ width fill ] "New Word"
               ]
        )


viewWordTag : LoadedModel -> Int -> Network.DesirabilityStudyWordTag -> Element LoadedMsg
viewWordTag model num tag =
    UI.grayBox [ width fill, padding 20 ] <|
        column [ spacing 10 ]
            [ UI.textInput [ width fill ]
                { onChange = UpdateTagName num
                , text = tag.tag
                , placeholder = Just (Input.placeholder [] (text "Tag"))
                , label = Input.labelHidden "Tag name"
                }
            , UI.multilineInput [ width fill ]
                { onChange = UpdateTagDescription num
                , text = tag.description
                , placeholder = Just (Input.placeholder [] (text "Describe the tag..."))
                , label = Input.labelHidden "Tag description"
                , spellcheck = True
                }
            ]


viewItems : LoadedModel -> Element LoadedMsg
viewItems ({ items } as model) =
    column [ spacing 20, width (fill |> maximum 500) ]
        (List.indexedMap (viewItem model) items
            ++ [ UI.textButton (Just NewItem) [ width fill ] "New Item" ]
        )


viewItem : LoadedModel -> Int -> Network.DesirabilityStudyItem -> Element LoadedMsg
viewItem model num item =
    UI.grayBox [ width fill, padding 20 ] <|
        column [ spacing 10 ]
            [ image [ width fill ]
                { src = Network.imagePath item.imageFileID
                , description = ""
                }
            , UI.multilineInput [ width fill ]
                { onChange = UpdateItemDescription num
                , text = item.description
                , placeholder = Just (Input.placeholder [] (text "Describe the image..."))
                , label = Input.labelHidden "Image description"
                , spellcheck = True
                }
            ]



-- INIT RESULTS


type alias ResultsModel =
    { studyData : Network.StudyData
    , wordBank : List Network.DesirabilityStudyWord
    , wordTags : List Network.DesirabilityStudyWordTag
    , items : List Network.DesirabilityStudyItem
    , numberOfWordsToSelect : Int
    , observations : List Network.DesirabilityStudyObservationData
    , tab : ResultsTab
    }


initResults : Network.DesirabilityStudyData -> Network.DesirabilityStudyResults -> ( ResultsModel, Effect ResultsMsg )
initResults data results =
    ( ResultsModel data.studyData data.wordBank data.wordTags data.items data.numberOfWordsToSelect results.observations ResultsItems, Effect.none )



-- UPDATE RESULTS


type ResultsMsg
    = ResultsSetTab ResultsTab


type ResultsTab
    = ResultsItems
    | ResultsWordBank
    | ResultsWordTags


updateResults : Auth.User -> Params -> Shared.Model -> ResultsMsg -> ResultsModel -> ( ResultsModel, Effect ResultsMsg )
updateResults user params shared msg model =
    case msg of
        ResultsSetTab tab ->
            ( { model | tab = tab }, Effect.none )



-- SUBSCRIPTIONS RESULTS


subscriptionsResults : ResultsModel -> Sub ResultsMsg
subscriptionsResults model =
    Sub.none



-- VIEW RESULTS


viewResults : Shared.Model -> ResultsModel -> View ResultsMsg
viewResults shared model =
    SharedUI.sharedFrame shared
        { title = "Viewing Results For " ++ model.studyData.title
        , body =
            column [ padding 10, spacing 20, width fill ]
                [ row [ spacing 20, width fill ]
                    [ SharedUI.tabButton model.tab ResultsItems ResultsSetTab "Items"
                    , SharedUI.tabButton model.tab ResultsWordBank ResultsSetTab "Word Bank"
                    , SharedUI.tabButton model.tab ResultsWordTags ResultsSetTab "Tags"
                    ]
                , case model.tab of
                    ResultsItems ->
                        viewResultsItems model

                    ResultsWordBank ->
                        viewResultsWordBank model

                    ResultsWordTags ->
                        viewResultsTags model
                ]
        , over = Nothing
        }


viewResultsItems : ResultsModel -> Element ResultsMsg
viewResultsItems { items, observations } =
    let
        data =
            observationsByItem observations
                |> wordPercents
                |> List.map2 Tuple.pair items
                |> List.map2 Tuple.pair (observationsByItem observations |> List.map averageRating)
    in
    column [ spacing 20, width (fill |> maximum 600) ]
        (List.map viewResultsItem data)


viewResultsItem : ( Float, ( Network.DesirabilityStudyItem, List ( String, Float ) ) ) -> Element ResultsMsg
viewResultsItem ( average, ( item, data ) ) =
    UI.grayBox [ width fill, padding 20 ] <|
        column [ width fill, spacing 20 ]
            ([ row [ spacing 20, width fill ]
                [ image [ width (fill |> maximum 100) ]
                    { src = Network.imagePath item.imageFileID
                    , description = ""
                    }
                , el [ alignLeft ] (text item.description)
                , el [ alignRight ] (text <| "Average Rating: " ++ Round.round 1 average)
                ]
             , UI.blackLine [ width fill ]
             ]
                ++ List.map viewResultsItemPercent data
            )


viewResultsItemPercent : ( String, Float ) -> Element ResultsMsg
viewResultsItemPercent ( word, percent ) =
    row [ width fill ]
        [ text word
        , el [ alignRight ] (text <| (percent * 100.0 |> round |> String.fromInt) ++ "%")
        ]


viewResultsWordBank : ResultsModel -> Element ResultsMsg
viewResultsWordBank { wordBank } =
    column [ spacing 20, width (fill |> maximum 600) ]
        (List.map viewResultsWordBankItem wordBank)


viewResultsWordBankItem : Network.DesirabilityStudyWord -> Element ResultsMsg
viewResultsWordBankItem { word, tags } =
    UI.grayBox [ padding 20, width fill ] <|
        column [ width fill, spacing 20 ]
            [ el [ Font.bold ] (text word)
            , wrappedRow [ spacing 10, UI.fontSize -1 ] (List.map text tags)
            ]


viewResultsTags : ResultsModel -> Element ResultsMsg
viewResultsTags { wordTags } =
    column [ spacing 20, width (fill |> maximum 600) ]
        (List.map viewResultsTag wordTags)


viewResultsTag : Network.DesirabilityStudyWordTag -> Element ResultsMsg
viewResultsTag { tag, description } =
    UI.grayBox [ padding 20, width fill ] <|
        column [ width fill, spacing 20 ]
            [ el [ Font.bold ] (text tag)
            , UI.par description
            ]



-- STATISTICS


observationsByItem : List Network.DesirabilityStudyObservationData -> List (List Network.DesirabilityStudyWordResponse)
observationsByItem observations =
    observations
        |> List.map .responses
        |> List.Extra.transpose


doubleUp : a -> ( a, a )
doubleUp k =
    ( k, k )


mapSecond : (b -> y) -> List ( a, b ) -> List ( a, y )
mapSecond =
    List.map << Tuple.mapSecond


wordPercentsForItem : List Network.DesirabilityStudyWordResponse -> List ( String, Float )
wordPercentsForItem responses =
    let
        uniqueWords =
            responses
                |> List.map .words
                |> List.concat
                |> Set.fromList
                |> Set.toList

        wordCount word =
            responses
                |> List.map .words
                |> List.map (List.member word)
                |> List.filter ((==) True)
                |> List.length
    in
    uniqueWords
        |> List.map doubleUp
        |> mapSecond wordCount
        |> mapSecond (\k -> toFloat k / toFloat (List.length responses))
        |> List.sortBy Tuple.second
        |> List.reverse


averageRating : List Network.DesirabilityStudyWordResponse -> Float
averageRating item =
    item
        |> List.map .rating
        |> List.sum
        |> (\x -> (x |> toFloat) / (List.length item |> toFloat))


wordPercents : List (List Network.DesirabilityStudyWordResponse) -> List (List ( String, Float ))
wordPercents responses =
    responses |> List.map wordPercentsForItem
