module TreeManipulation exposing (..)

import Html.Attributes exposing (target)
import Network exposing (TreeNode(..))


any : (a -> Bool) -> TreeNode a -> Bool
any f (TreeNode _ content children) =
    f content || List.any (any f) children


containsNodeWithID : String -> TreeNode a -> Bool
containsNodeWithID comparingID (TreeNode id _ children) =
    (comparingID == id) || List.any (containsNodeWithID comparingID) children


appendInNode : String -> TreeNode a -> TreeNode a -> TreeNode a
appendInNode parentID newNode tree =
    let
        f ((TreeNode fid fdata fchildren) as node) =
            if fid == parentID then
                TreeNode fid fdata (fchildren ++ [ newNode ])

            else
                node

        (TreeNode nid ndata nchildren) =
            f tree
    in
    TreeNode nid ndata (List.map (appendInNode parentID newNode) nchildren)


uniqueIntID : Int -> TreeNode a -> String
uniqueIntID accumulator node =
    if containsNodeWithID (String.fromInt accumulator) node then
        uniqueIntID (accumulator + 1) node

    else
        accumulator |> String.fromInt


mapByID : String -> (a -> a) -> TreeNode a -> TreeNode a
mapByID targetID mapper (TreeNode id data children) =
    let
        mapOne nodeID nodeData =
            if nodeID == targetID then
                mapper nodeData

            else
                nodeData
    in
    TreeNode id (mapOne id data) (List.map (mapByID targetID mapper) children)


deleteByID : String -> TreeNode a -> TreeNode a
deleteByID targetID (TreeNode id data children) =
    let
        filter (TreeNode compareID _ _) =
            not (compareID == targetID)
    in
    TreeNode id data (List.map (deleteByID targetID) (List.filter filter children))


nodeByIDWithParents : String -> TreeNode a -> Maybe ( TreeNode a, List (TreeNode a) )
nodeByIDWithParents targetID ((TreeNode id _ children) as node) =
    let
        stepper : List (TreeNode a) -> TreeNode a -> Maybe ( TreeNode a, List (TreeNode a) ) -> Maybe ( TreeNode a, List (TreeNode a) )
        stepper parents ((TreeNode stepID _ stepChildren) as stepTreeNode) stepMaybe =
            case stepMaybe of
                Just ( cTreeNode, cParents ) ->
                    Just ( cTreeNode, cParents )

                Nothing ->
                    if stepID == targetID then
                        Just ( stepTreeNode, parents )

                    else
                        List.foldl (stepper <| parents ++ [ stepTreeNode ]) Nothing stepChildren
    in
    if id == targetID then
        Just ( node, [] )

    else
        List.foldl (stepper [ node ]) Nothing children


nodeByID : String -> TreeNode a -> Maybe (TreeNode a)
nodeByID targetID node =
    nodeByIDWithParents targetID node
        |> Maybe.map (\( it, _ ) -> it)
