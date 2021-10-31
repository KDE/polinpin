module TraversalList exposing (Pos(..), TraversalList, current, index, length, make, map, next, previous, toList, toMaybe, updateCurrent)

import Html exposing (a)


type Pos a
    = BeforeList
    | AtItem a
    | AfterList


toMaybe : Pos a -> Maybe a
toMaybe pos =
    case pos of
        BeforeList ->
            Nothing

        AfterList ->
            Nothing

        AtItem a ->
            Just a


type TraversalList a
    = TraversalList
        { before : List a
        , after : List a
        , current : Pos a
        }


index : TraversalList a -> Int
index tlist =
    let
        list =
            rec tlist
    in
    List.length list.before


length : TraversalList a -> Int
length tlist =
    let
        list =
            rec tlist

        size =
            case list.current of
                AtItem _ ->
                    1

                _ ->
                    0
    in
    List.length list.before + size + List.length list.after


rec : TraversalList a -> { before : List a, after : List a, current : Pos a }
rec a =
    case a of
        TraversalList record ->
            record


make : List a -> TraversalList a
make list =
    TraversalList { before = [], after = list, current = BeforeList }


current : TraversalList a -> Pos a
current list =
    (rec list).current


previous : TraversalList a -> TraversalList a
previous tlist =
    let
        list =
            rec tlist
    in
    case list.current of
        BeforeList ->
            TraversalList list

        AtItem item ->
            case list.before of
                h :: t ->
                    TraversalList
                        { list
                            | before = t
                            , after = item :: list.after
                            , current = AtItem h
                        }

                [] ->
                    TraversalList { list | after = item :: list.after, current = BeforeList }

        AfterList ->
            case list.before of
                h :: t ->
                    TraversalList
                        { list
                            | before = t
                            , current = AtItem h
                        }

                [] ->
                    TraversalList list


map : (a -> a) -> TraversalList a -> TraversalList a
map mapper tlist =
    let
        list =
            rec tlist
    in
    TraversalList
        { before = List.map mapper list.before
        , after = List.map mapper list.after
        , current =
            case list.current of
                AtItem a ->
                    AtItem <| mapper a

                _ ->
                    list.current
        }


toList : TraversalList a -> List a
toList tlist =
    let
        list =
            rec tlist
    in
    case list.current of
        AtItem a ->
            list.before ++ a :: list.after

        _ ->
            list.before ++ list.after


updateCurrent : (a -> a) -> TraversalList a -> TraversalList a
updateCurrent mapper tlist =
    let
        list =
            rec tlist
    in
    case list.current of
        AtItem a ->
            TraversalList
                { before = list.before
                , after = list.after
                , current = AtItem <| mapper a
                }

        _ ->
            tlist


next : TraversalList a -> TraversalList a
next tlist =
    let
        list =
            rec tlist
    in
    case list.current of
        BeforeList ->
            case list.after of
                h :: t ->
                    TraversalList
                        { list
                            | after = t
                            , current = AtItem h
                        }

                [] ->
                    TraversalList list

        AtItem item ->
            case list.after of
                h :: t ->
                    TraversalList
                        { list
                            | after = t
                            , before = item :: list.before
                            , current = AtItem h
                        }

                [] ->
                    TraversalList { list | before = item :: list.before, current = AfterList }

        AfterList ->
            TraversalList list
