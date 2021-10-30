module Tree exposing (ID(..), Node(..), map, mapID, appendID, makeLeaf, delete)


type ID
    = ID String


type Node a
    = Node ID a (List (Node a))

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
