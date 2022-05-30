module Network exposing (..)

import Http
import Json.Decode as D
import Json.Encode as E


serverUrl : String
serverUrl =
    "http://127.0.0.1:8080"


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

                    _ ->
                        D.fail "Unknown case"
            )


myTreeTests : UserSession -> (Result Http.Error (List StudyData) -> msg) -> Cmd msg
myTreeTests session msg =
    getHeaders
        { headers = sessionHeaders session
        , url = endpoint "tree-tests/my"
        , expect = Http.expectJson msg (D.field "studies" (D.list studyDataDecoder))
        }


type StudyKind
    = TreeTest


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


treeTest : UserSession -> String -> String -> (Result Http.Error TreeTestStudyData -> msg) -> Cmd msg
treeTest session username slug msg =
    getHeaders
        { headers = sessionHeaders session
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
