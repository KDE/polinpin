module TreeTest exposing (..)

import Json.Decode as D
import Json.Encode as E
import Tree
import Http

type alias Study =
    { tree : Tree.Node Item
    , name : String
    , tasks : List Task
    }


type alias Item =
    { text : String
    }


type alias Task =
    { text : String
    , correctAnswer : Tree.ID
    }


encodeStudy : Study -> E.Value
encodeStudy study =
    E.object
        [ ( "name", E.string study.name )
        , ( "tasks", E.list encodeTask study.tasks )
        , ( "tree", Tree.encodeNode encodeItem study.tree )
        ]


studyDecoder : D.Decoder Study
studyDecoder =
    D.map3 Study
        (D.field "tree" (Tree.nodeDecoder itemDecoder))
        (D.field "name" D.string)
        (D.field "tasks" (D.list taskDecoder))


itemDecoder : D.Decoder Item
itemDecoder =
    D.map Item
        (D.field "text" D.string)


taskDecoder : D.Decoder Task
taskDecoder =
    D.map2 Task
        (D.field "text" D.string)
        (D.field "correctAnswer" D.string |> D.map Tree.ID)


encodeTask : Task -> E.Value
encodeTask { text, correctAnswer } =
    let
        (Tree.ID nid) =
            correctAnswer
    in
    E.object
        [ ( "text", E.string text )
        , ( "correctAnswer", E.string nid )
        ]


encodeItem : Item -> E.Value
encodeItem { text } =
    E.object
        [ ( "text", E.string text )
        ]

getStudy : String -> (Result Http.Error Study -> msg) -> Cmd msg
getStudy id f =
    Http.get
        { url = "http://127.0.0.1:25727/tree-test/" ++ id
        , expect = Http.expectJson f studyDecoder
        }

setStudy : String -> Study -> (Result Http.Error () -> msg) -> Cmd msg
setStudy id study f =
    Http.post
        { url = "http://127.0.0.1:25727/editor/tree-test/" ++ id
        , body = Http.jsonBody <| encodeStudy study
        , expect = Http.expectWhatever f
        }
