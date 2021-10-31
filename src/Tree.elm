module Tree exposing (ID(..), Node(..), any, appendID, containsID, delete, encodeNode, makeLeaf, map, mapID, nodeByID, nodeByIDWithParents, nodeDecoder)

import Json.Decode as D
import Json.Encode as E


type ID
    = ID String


type Node a
    = Node ID a (List (Node a))


any : (a -> Bool) -> Node a -> Bool
any f (Node _ content children) =
    f content || List.any (any f) children


nodeByIDWithParents : ID -> Node a -> Maybe ( Node a, List (Node a) )
nodeByIDWithParents targetID ((Node id _ children) as node) =
    let
        stepper : List (Node a) -> Node a -> Maybe ( Node a, List (Node a) ) -> Maybe ( Node a, List (Node a) )
        stepper parents ((Node stepID _ stepChildren) as stepNode) stepMaybe =
            case stepMaybe of
                Just ( cNode, cParents ) ->
                    Just ( cNode, cParents )

                Nothing ->
                    if stepID == targetID then
                        Just ( stepNode, parents )

                    else
                        List.foldl (stepper <| parents ++ [ stepNode ]) Nothing stepChildren
    in
    if id == targetID then
        Just ( node, [] )

    else
        List.foldl (stepper [ node ]) Nothing children


nodeByID : ID -> Node a -> Maybe (Node a)
nodeByID targetID node =
    nodeByIDWithParents targetID node
        |> Maybe.map (\( it, _ ) -> it)


containsID : ID -> Node a -> Bool
containsID compID (Node id _ children) =
    (compID == id) || List.any (containsID compID) children


encodeNode : (a -> E.Value) -> Node a -> E.Value
encodeNode f (Node (ID id) content children) =
    E.object
        [ ( "id", E.string id )
        , ( "content", f content )
        , ( "children", E.list (encodeNode f) children )
        ]


nodeDecoder : D.Decoder a -> D.Decoder (Node a)
nodeDecoder contentDecoder =
    D.map3 Node
        (D.field "id" D.string |> D.map ID)
        (D.field "content" contentDecoder)
        (D.field "children"
            (D.oneOf
                [ D.null []
                , D.list (D.lazy (\_ -> nodeDecoder contentDecoder))
                ]
            )
        )


makeLeaf : ID -> a -> Node a
makeLeaf id cont =
    Node id cont []


delete : ID -> Node a -> Node a
delete id (Node nid ndata nchildren) =
    let
        filter (Node compID _ _) =
            not (compID == id)
    in
    Node nid ndata (List.map (delete id) (List.filter filter nchildren))


map : (a -> b) -> Node a -> Node b
map f (Node id data children) =
    Node id (f data) (List.map (map f) children)


mapID : ID -> (a -> a) -> Node a -> Node a
mapID targetID f (Node id data children) =
    let
        mapOne nID nData =
            if nID == targetID then
                f nData

            else
                nData
    in
    Node id (mapOne id data) (List.map (mapID targetID f) children)


appendID : ID -> Node a -> Node a -> Node a
appendID id newNode appendUnder =
    let
        f =
            \(Node fID fData fChildren) ->
                if fID == id then
                    Node fID fData (fChildren ++ [ newNode ])

                else
                    Node fID fData fChildren

        (Node nID nData nChildren) =
            f appendUnder
    in
    Node nID nData (List.map (appendID id newNode) nChildren)
