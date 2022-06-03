module Network exposing (DesirabilityStudyData, DesirabilityStudyItem, DesirabilityStudyObservationData, DesirabilityStudyResults, DesirabilityStudyWord, DesirabilityStudyWordResponse, DesirabilityStudyWordTag, OAuth2Service, OAuth2ServiceConfig, RequestStatus(..), StudyData, StudyKind(..), TreeNode(..), TreeStudyItem, TreeStudyResults, TreeStudyTask, TreeTestAnsweredQuestion, TreeTestObservation, TreeTestPastSelection, TreeTestStudyData, UserInformation, UserSession, desirabilityStudy, desirabilityStudyResults, desirabilityStudySubmitObservation, desirabilityStudyWithoutAuth, imagePath, login, me, myStudies, newDesirabilityStudy, newTreeTest, oauth2Services, publishStudy, register, saveDesirabilityStudy, saveTreeStudy, treeStudyResults, treeStudySubmitObservation, treeTest, treeTestWithoutAuth, uploadFile)

import File exposing (File)
import Http
import Json.Decode as D
import Json.Encode as E


serverUrl : String
serverUrl =
    "https://api.polinpin.blackquill.cc"


endpoint : String -> String
endpoint name =
    serverUrl ++ "/" ++ name


type alias UserSession =
    { token : String
    }


userSessionDecoder : D.Decoder UserSession
userSessionDecoder =
    D.map UserSession
        (D.field "token" D.string)


login : String -> String -> (Result Http.Error UserSession -> msg) -> Cmd msg
login username password msg =
    Http.post
        { url = endpoint "auth/login"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "username", E.string username )
                    , ( "password", E.string password )
                    ]
                )
        , expect = Http.expectJson msg userSessionDecoder
        }


register : String -> String -> String -> (Result Http.Error UserSession -> msg) -> Cmd msg
register name username password msg =
    Http.post
        { url = endpoint "auth/register"
        , body =
            Http.jsonBody
                (E.object
                    [ ( "name", E.string name )
                    , ( "username", E.string username )
                    , ( "password", E.string password )
                    ]
                )
        , expect = Http.expectJson msg userSessionDecoder
        }


postHeaders : { url : String, body : Http.Body, expect : Http.Expect msg, headers : List Http.Header } -> Cmd msg
postHeaders { url, body, expect, headers } =
    Http.request
        { method = "POST"
        , headers = headers
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


patchHeaders : { url : String, body : Http.Body, expect : Http.Expect msg, headers : List Http.Header } -> Cmd msg
patchHeaders { url, body, expect, headers } =
    Http.request
        { method = "PATCH"
        , headers = headers
        , url = url
        , body = body
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


getHeaders : { url : String, expect : Http.Expect msg, headers : List Http.Header } -> Cmd msg
getHeaders { url, expect, headers } =
    Http.request
        { method = "GET"
        , headers = headers
        , url = url
        , body = Http.emptyBody
        , expect = expect
        , timeout = Nothing
        , tracker = Nothing
        }


type alias UserInformation =
    { name : String
    , username : String
    }


userInformationDecoder : D.Decoder UserInformation
userInformationDecoder =
    D.map2 UserInformation
        (D.field "name" D.string)
        (D.field "username" D.string)


sessionHeaders : UserSession -> List Http.Header
sessionHeaders session =
    [ Http.header "Authorization" session.token ]


me : UserSession -> (Result Http.Error UserInformation -> msg) -> Cmd msg
me session msg =
    getHeaders
        { headers = sessionHeaders session
        , url = endpoint "auth/me"
        , expect = Http.expectJson msg userInformationDecoder
        }


type alias StudyData =
    { title : String
    , slug : String
    , published : Bool
    , password : Maybe String
    , kind : StudyKind
    }


studyDataDecoder : D.Decoder StudyData
studyDataDecoder =
    D.map5 StudyData
        (D.field "title" D.string)
        (D.field "slug" D.string)
        (D.field "published" D.bool)
        (D.maybe <| D.field "password" D.string)
        (D.field "kind" kindDecoder)


kindDecoder : D.Decoder StudyKind
kindDecoder =
    D.string
        |> D.andThen
            (\x ->
                case x of
                    "treeTest" ->
                        D.succeed TreeTest

                    "desirabilityTest" ->
                        D.succeed DesirabilityTest

                    _ ->
                        D.fail "Unknown case"
            )


myStudies : UserSession -> (Result Http.Error (List StudyData) -> msg) -> Cmd msg
myStudies session msg =
    getHeaders
        { headers = sessionHeaders session
        , url = endpoint "studies/my"
        , expect = Http.expectJson msg (D.field "studies" (D.list studyDataDecoder))
        }


type StudyKind
    = TreeTest
    | DesirabilityTest


maybeToList : Maybe a -> List a
maybeToList =
    Maybe.map List.singleton >> Maybe.withDefault []


newTreeTest : UserSession -> String -> String -> (Result Http.Error String -> msg) -> Cmd msg
newTreeTest session username title msg =
    postHeaders
        { headers = sessionHeaders session
        , url = endpoint "tree-tests/" ++ username
        , expect = Http.expectString msg
        , body =
            Http.jsonBody
                (E.object
                    [ ( "title", E.string title ) ]
                )
        }


newDesirabilityStudy : UserSession -> String -> String -> (Result Http.Error String -> msg) -> Cmd msg
newDesirabilityStudy session username title msg =
    postHeaders
        { headers = sessionHeaders session
        , url = endpoint "desirability-studies/" ++ username
        , expect = Http.expectString msg
        , body =
            Http.jsonBody
                (E.object
                    [ ( "title", E.string title ) ]
                )
        }


treeTest : UserSession -> String -> String -> (Result Http.Error TreeTestStudyData -> msg) -> Cmd msg
treeTest session username slug msg =
    getHeaders
        { headers = sessionHeaders session
        , url = endpoint "tree-tests/" ++ username ++ "/" ++ slug
        , expect = Http.expectJson msg treeTestStudyDataDecoder
        }


desirabilityStudy : UserSession -> String -> String -> (Result Http.Error DesirabilityStudyData -> msg) -> Cmd msg
desirabilityStudy session username slug msg =
    getHeaders
        { headers = sessionHeaders session
        , url = endpoint "desirability-studies/" ++ username ++ "/" ++ slug
        , expect = Http.expectJson msg desirabilityStudyDataDecoder
        }


desirabilityStudyWithoutAuth : Maybe String -> String -> String -> (Result Http.Error DesirabilityStudyData -> msg) -> Cmd msg
desirabilityStudyWithoutAuth password username slug msg =
    getHeaders
        { headers = password |> Maybe.map (\p -> Http.header "StudyPassword" p) |> maybeToList
        , url = endpoint "desirability-studies/" ++ username ++ "/" ++ slug
        , expect = Http.expectJson msg desirabilityStudyDataDecoder
        }


type alias DesirabilityStudyData =
    { studyData : StudyData
    , wordBank : List DesirabilityStudyWord
    , wordTags : List DesirabilityStudyWordTag
    , items : List DesirabilityStudyItem
    , numberOfWordsToSelect : Int
    }


desirabilityStudyDataDecoder : D.Decoder DesirabilityStudyData
desirabilityStudyDataDecoder =
    D.map5 DesirabilityStudyData
        (D.field "studyData" studyDataDecoder)
        (D.field "wordBank" (D.list desirabilityStudyWordDecoder))
        (D.field "wordTags" (D.list desirabilityStudyWordTagDecoder))
        (D.field "items" (D.list desirabilityStudyItemDecoder))
        (D.field "numberOfWordsToSelect" D.int)


type alias DesirabilityStudyItem =
    { imageFileID : String
    , description : String
    }


desirabilityStudyItemDecoder : D.Decoder DesirabilityStudyItem
desirabilityStudyItemDecoder =
    D.map2 DesirabilityStudyItem
        (D.field "imageFileID" D.string)
        (D.field "description" D.string)


type alias DesirabilityStudyWordTag =
    { tag : String
    , description : String
    }


desirabilityStudyWordTagDecoder : D.Decoder DesirabilityStudyWordTag
desirabilityStudyWordTagDecoder =
    D.map2 DesirabilityStudyWordTag
        (D.field "tag" D.string)
        (D.field "description" D.string)


type alias DesirabilityStudyWord =
    { word : String
    , tags : List String
    }


desirabilityStudyWordDecoder : D.Decoder DesirabilityStudyWord
desirabilityStudyWordDecoder =
    D.map2 DesirabilityStudyWord
        (D.field "word" D.string)
        (D.field "tags" (D.list D.string))


treeTestWithoutAuth : Maybe String -> String -> String -> (Result Http.Error TreeTestStudyData -> msg) -> Cmd msg
treeTestWithoutAuth password username slug msg =
    getHeaders
        { headers = password |> Maybe.map (\p -> Http.header "StudyPassword" p) |> maybeToList
        , url = endpoint "tree-tests/" ++ username ++ "/" ++ slug
        , expect = Http.expectJson msg treeTestStudyDataDecoder
        }


type RequestStatus a
    = NoRequest
    | PendingRequest
    | ResultReceived (Result Http.Error a)


type alias TreeTestStudyData =
    { studyData : StudyData
    , tree : TreeNode TreeStudyItem
    , tasks : List TreeStudyTask
    }


treeTestStudyDataDecoder : D.Decoder TreeTestStudyData
treeTestStudyDataDecoder =
    D.map3 TreeTestStudyData
        (D.field "studyData" studyDataDecoder)
        (D.field "tree" (treeNodeDecoder treeStudyItemDecoder))
        (D.field "tasks" (D.list treeStudyTaskDecoder))


type TreeNode a
    = TreeNode String a (List (TreeNode a))


treeNodeDecoder : D.Decoder a -> D.Decoder (TreeNode a)
treeNodeDecoder contentDecoder =
    D.map3 TreeNode
        (D.field "id" D.string)
        (D.field "content" contentDecoder)
        (D.field "children" (D.list (D.lazy (\_ -> treeNodeDecoder contentDecoder))))


encodeTreeNode : (a -> E.Value) -> TreeNode a -> E.Value
encodeTreeNode contentEncoder (TreeNode id content children) =
    E.object
        [ ( "id", E.string id )
        , ( "content", contentEncoder content )
        , ( "children", E.list (encodeTreeNode contentEncoder) children )
        ]


type alias TreeStudyItem =
    { text : String
    }


treeStudyItemDecoder : D.Decoder TreeStudyItem
treeStudyItemDecoder =
    D.map TreeStudyItem
        (D.field "text" D.string)


encodeTreeStudyItem : TreeStudyItem -> E.Value
encodeTreeStudyItem item =
    E.object
        [ ( "text", E.string item.text )
        ]


type alias TreeStudyTask =
    { id : String
    , text : String
    , answer : String
    }


treeStudyTaskDecoder : D.Decoder TreeStudyTask
treeStudyTaskDecoder =
    D.map3 TreeStudyTask
        (D.field "id" D.string)
        (D.field "text" D.string)
        (D.field "answer" D.string)


encodeTreeStudyTask : TreeStudyTask -> E.Value
encodeTreeStudyTask task =
    E.object
        [ ( "id", E.string task.id )
        , ( "text", E.string task.text )
        , ( "answer", E.string task.answer )
        ]


saveTreeStudy : UserSession -> String -> String -> TreeNode TreeStudyItem -> List TreeStudyTask -> (Result Http.Error () -> msg) -> Cmd msg
saveTreeStudy session username slug tree tasks msg =
    patchHeaders
        { headers = sessionHeaders session
        , url = endpoint "tree-tests/" ++ username ++ "/" ++ slug
        , expect = Http.expectWhatever msg
        , body =
            Http.jsonBody
                (E.object
                    [ ( "tree", encodeTreeNode encodeTreeStudyItem tree )
                    , ( "tasks", E.list encodeTreeStudyTask tasks )
                    ]
                )
        }


saveDesirabilityStudy :
    UserSession
    -> String
    -> String
    -> List DesirabilityStudyWord
    -> List DesirabilityStudyWordTag
    -> List DesirabilityStudyItem
    -> Int
    -> (Result Http.Error () -> msg)
    -> Cmd msg
saveDesirabilityStudy session username slug words tags items selectCount msg =
    patchHeaders
        { headers = sessionHeaders session
        , url = endpoint "desirability-studies/" ++ username ++ "/" ++ slug
        , expect = Http.expectWhatever msg
        , body =
            Http.jsonBody
                (E.object
                    [ ( "wordBank", E.list encodeDesirabilityStudyWord words )
                    , ( "wordTags", E.list encodeDesirabilityStudyWordTag tags )
                    , ( "items", E.list encodeDesirabilityStudyItem items )
                    , ( "numberOfWordsToSelect", E.int selectCount )
                    ]
                )
        }


encodeDesirabilityStudyWord : DesirabilityStudyWord -> E.Value
encodeDesirabilityStudyWord word =
    E.object
        [ ( "word", E.string word.word )
        , ( "tags", E.list E.string word.tags )
        ]


encodeDesirabilityStudyWordTag : DesirabilityStudyWordTag -> E.Value
encodeDesirabilityStudyWordTag tag =
    E.object
        [ ( "tag", E.string tag.tag )
        , ( "description", E.string tag.description )
        ]


encodeDesirabilityStudyItem : DesirabilityStudyItem -> E.Value
encodeDesirabilityStudyItem item =
    E.object
        [ ( "imageFileID", E.string item.imageFileID )
        , ( "description", E.string item.description )
        ]


publishStudy : UserSession -> String -> String -> (Result Http.Error () -> msg) -> Cmd msg
publishStudy session username slug msg =
    postHeaders
        { headers = sessionHeaders session
        , url = endpoint "studies/" ++ username ++ "/" ++ slug ++ "/publish"
        , expect = Http.expectWhatever msg
        , body = Http.emptyBody
        }


type alias TreeStudyResults =
    { observations : List TreeTestObservation
    }


treeStudyResultsDecoder : D.Decoder TreeStudyResults
treeStudyResultsDecoder =
    D.map TreeStudyResults
        (D.field "observations" (D.list treeTestObservationDecoder))


type alias TreeTestObservation =
    { responses : List TreeTestAnsweredQuestion
    }


treeTestObservationDecoder : D.Decoder TreeTestObservation
treeTestObservationDecoder =
    D.map TreeTestObservation
        (D.field "responses" (D.list treeTestAnsweredQuestionDecoder))


encodeTreeTestObservation : TreeTestObservation -> E.Value
encodeTreeTestObservation observation =
    E.object
        [ ( "responses", E.list encodeTreeTestAnsweredQuestion observation.responses )
        ]


type alias TreeTestAnsweredQuestion =
    { taskID : String
    , answer : String
    , pastSelections : List TreeTestPastSelection
    , startedAt : Int
    , endedAt : Int
    }


encodeTreeTestAnsweredQuestion : TreeTestAnsweredQuestion -> E.Value
encodeTreeTestAnsweredQuestion answered =
    E.object
        [ ( "taskID", E.string answered.taskID )
        , ( "answer", E.string answered.answer )
        , ( "pastSelections", E.list encodeTreeTestPastSelection answered.pastSelections )
        , ( "startedAt", E.int answered.startedAt )
        , ( "endedAt", E.int answered.endedAt )
        ]


treeTestAnsweredQuestionDecoder : D.Decoder TreeTestAnsweredQuestion
treeTestAnsweredQuestionDecoder =
    D.map5 TreeTestAnsweredQuestion
        (D.field "taskID" D.string)
        (D.field "answer" D.string)
        (D.field "pastSelections" (D.list treeTestPastSelectionDecoder))
        (D.field "startedAt" D.int)
        (D.field "endedAt" D.int)


type alias TreeTestPastSelection =
    { selectedID : String
    , selectedAt : Int
    }


treeTestPastSelectionDecoder : D.Decoder TreeTestPastSelection
treeTestPastSelectionDecoder =
    D.map2 TreeTestPastSelection
        (D.field "selectedID" D.string)
        (D.field "selectedAt" D.int)


encodeTreeTestPastSelection : TreeTestPastSelection -> E.Value
encodeTreeTestPastSelection selection =
    E.object
        [ ( "selectedID", E.string selection.selectedID )
        , ( "selectedAt", E.int selection.selectedAt )
        ]


treeStudyResults : UserSession -> String -> String -> (Result Http.Error TreeStudyResults -> msg) -> Cmd msg
treeStudyResults session username slug msg =
    getHeaders
        { headers = sessionHeaders session
        , url = endpoint "tree-tests/" ++ username ++ "/" ++ slug ++ "/results"
        , expect = Http.expectJson msg treeStudyResultsDecoder
        }


desirabilityStudyResults : UserSession -> String -> String -> (Result Http.Error DesirabilityStudyResults -> msg) -> Cmd msg
desirabilityStudyResults session username slug msg =
    getHeaders
        { headers = sessionHeaders session
        , url = endpoint "desirability-studies/" ++ username ++ "/" ++ slug ++ "/results"
        , expect = Http.expectJson msg desirabilityStudyResultsDecoder
        }


type alias DesirabilityStudyResults =
    { observations : List DesirabilityStudyObservationData
    }


desirabilityStudyResultsDecoder : D.Decoder DesirabilityStudyResults
desirabilityStudyResultsDecoder =
    D.map DesirabilityStudyResults
        (D.field "observations" (D.list desirabilityStudyObservationDataDecoder))


type alias DesirabilityStudyObservationData =
    { responses : List DesirabilityStudyWordResponse
    }


desirabilityStudyObservationDataDecoder : D.Decoder DesirabilityStudyObservationData
desirabilityStudyObservationDataDecoder =
    D.map DesirabilityStudyObservationData
        (D.field "responses" (D.list desirabilityStudyWordResponseDecoder))


treeStudySubmitObservation : Maybe String -> String -> String -> TreeTestObservation -> (Result Http.Error () -> msg) -> Cmd msg
treeStudySubmitObservation password username slug observations msg =
    postHeaders
        { headers = password |> Maybe.map (\p -> Http.header "StudyPassword" p) |> maybeToList
        , url = endpoint "tree-tests/" ++ username ++ "/" ++ slug ++ "/completed"
        , expect = Http.expectWhatever msg
        , body =
            Http.jsonBody <|
                E.object
                    [ ( "result", encodeTreeTestObservation observations )
                    ]
        }


desirabilityStudySubmitObservation :
    Maybe String
    -> String
    -> String
    -> List DesirabilityStudyWordResponse
    -> (Result Http.Error () -> msg)
    -> Cmd msg
desirabilityStudySubmitObservation password username slug observations msg =
    postHeaders
        { headers = password |> Maybe.map (\p -> Http.header "StudyPassword" p) |> maybeToList
        , url = endpoint "desirability-studies/" ++ username ++ "/" ++ slug ++ "/completed"
        , expect = Http.expectWhatever msg
        , body =
            Http.jsonBody <|
                E.object
                    [ ( "responses", E.list encodeDesirabilityStudyWordResponse observations )
                    ]
        }


imagePath : String -> String
imagePath path =
    endpoint "uploads/" ++ path


uploadFile : UserSession -> File -> (Result Http.Error String -> msg) -> Cmd msg
uploadFile session file msg =
    postHeaders
        { headers = sessionHeaders session
        , url = endpoint "files/upload"
        , expect = Http.expectString msg
        , body = Http.fileBody file
        }


type alias DesirabilityStudyWordResponse =
    { words : List String
    , rating : Int
    }


encodeDesirabilityStudyWordResponse : DesirabilityStudyWordResponse -> E.Value
encodeDesirabilityStudyWordResponse response =
    E.object
        [ ( "words", E.list E.string response.words )
        , ( "rating", E.int response.rating )
        ]


desirabilityStudyWordResponseDecoder : D.Decoder DesirabilityStudyWordResponse
desirabilityStudyWordResponseDecoder =
    D.map2 DesirabilityStudyWordResponse
        (D.field "words" (D.list D.string))
        (D.field "rating" D.int)


type alias OAuth2Service =
    { name : String
    , url : String
    }


oauth2ServiceDecoder : D.Decoder OAuth2Service
oauth2ServiceDecoder =
    D.map2 OAuth2Service
        (D.field "name" D.string)
        (D.field "url" D.string)


type alias OAuth2ServiceConfig =
    { services : List OAuth2Service
    , normalLoginPossible : Bool
    }


oauth2Services : (Result Http.Error OAuth2ServiceConfig -> msg) -> Cmd msg
oauth2Services msg =
    getHeaders
        { url = endpoint "oauth2/services"
        , expect =
            Http.expectJson msg
                (D.map2 OAuth2ServiceConfig
                    (D.field "services" (D.list oauth2ServiceDecoder))
                    (D.field "normalLoginEnabled" D.bool)
                )
        , headers = []
        }
