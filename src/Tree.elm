module Tree exposing (ID(..), Node(..), any, appendID, containsID, count, delete, encodeNode, makeLeaf, map, mapID, nodeByID, nodeByIDWithParents, nodeDecoder, uniqueIntID, moveBefore, moveAfter, appendBeforeNode, appendAfterNode)

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


uniqueIntID : Int -> Node a -> Int
uniqueIntID akku node =
    if containsID (ID (String.fromInt akku)) node then
        uniqueIntID (akku + 1) node

    else
        akku


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

moveBefore : ID -> ID -> Node a -> Node a
moveBefore item before node =
    case nodeByID item node of
        Just itemBeingMoved ->
            let
                modified =
                    delete item node
            in
            appendBeforeNode itemBeingMoved before modified

        Nothing ->
            node

moveAfter : ID -> ID -> Node a -> Node a
moveAfter item after node =
    case nodeByID item node of
        Just itemBeingMoved ->
            let
                modified =
                    delete item node
            in
            appendAfterNode itemBeingMoved after modified

        Nothing ->
            node

count : Node a -> Int
count (Node _ _ children) =
    1 + List.length children + List.sum (List.map count children)


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

appendBeforeNode : Node a -> ID -> Node a -> Node a
appendBeforeNode newNode appendBefore appendUnder =
    let
        before =
            \((Node fID _ _) as fnode) ->
                if fID == appendBefore then
                    [newNode, fnode]
                else
                    [fnode]

        f =
            \(Node fID fData fChildren) ->
                Node fID fData (List.concatMap before fChildren)

        (Node nID nData nChildren) =
            f appendUnder
    in
    Node nID nData (List.map (appendBeforeNode newNode appendBefore) nChildren)

appendAfterNode : Node a -> ID -> Node a -> Node a
appendAfterNode newNode appendAfter appendUnder =
    let
        after =
            \((Node fID _ _) as fnode) ->
                if fID == appendAfter then
                    [fnode, newNode]
                else
                    [fnode]

        f =
            \(Node fID fData fChildren) ->
                Node fID fData (List.concatMap after fChildren)

        (Node nID nData nChildren) =
            f appendUnder
    in
    Node nID nData (List.map (appendAfterNode newNode appendAfter) nChildren)

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
