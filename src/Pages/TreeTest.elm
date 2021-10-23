module Pages.TreeTest exposing (Model, Msg, page)

import Element exposing (..)
import Element.Background as Background
import Element.Border as Border
import Element.Events exposing (..)
import Element.Font as Font
import Gen.Params.TreeTest exposing (Params)
import Page
import Request
import Shared
import TraversalList exposing (TraversalList)
import UI
import View exposing (View)


page : Shared.Model -> Request.With Params -> Page.With Model Msg
page shared _ =
    Page.element
        { init = init
        , update = update
        , view = view shared
        , subscriptions = subscriptions
        }



-- INIT


type alias Model =
    { rootNode : Node
    , selectedNode : Maybe String
    , questions : TraversalList Question
    }


type Node
    = Node
        { label : String
        , id : String
        , children : List Node
        }


type alias Question =
    { text : String
    , answer : Maybe String
    }


mkNode id label children =
    Node { id = id, label = label, children = children }


nLabel node =
    case node of
        Node { label } ->
            label


nChildren node =
    case node of
        Node { children } ->
            children


nID : Node -> String
nID node =
    case node of
        Node { id } ->
            id


defaultRootNode =
    mkNode "homepage"
        "Homepage"
        [ mkNode "homepage-shop" "Shop" []
        , mkNode "homepage-settings" "Settings" []
        , mkNode "homepage-profile" "Profile" []
        ]


defaultQuestions : TraversalList Question
defaultQuestions =
    TraversalList.make
        [ { text = "hi", answer = Nothing }
        ]


init : ( Model, Cmd Msg )
init =
    ( Model defaultRootNode Nothing defaultQuestions, Cmd.none )



-- UPDATE


type Msg
    = NodeClicked String
    | NextQuestion
    | PreviousQuestion


updateQ : Maybe String -> Question -> Question
updateQ mayhaps q =
    { q | answer = mayhaps }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updatedList =
            TraversalList.updateCurrent (updateQ model.selectedNode) model.questions
    in
    case msg of
        NodeClicked node ->
            ( { model | selectedNode = Just node }, Cmd.none )

        NextQuestion ->
            let
                newList =
                    TraversalList.next updatedList
            in
            ( { model | questions = newList, selectedNode = Maybe.andThen (\q -> q.answer) (TraversalList.toMaybe (TraversalList.current newList)) }, Cmd.none )

        PreviousQuestion ->
            let
                newList =
                    TraversalList.previous updatedList
            in
            ( { model | questions = newList, selectedNode = Maybe.andThen (\q -> q.answer) (TraversalList.toMaybe (TraversalList.current newList)) }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Shared.Model -> Model -> View Msg
view shared model =
    { title = "Tree Test"
    , element =
        UI.with shared
            [ el [ centerX, padding 4, Border.color (rgb255 0 0 0), Border.width 1 ] <| viewEl model
            , row [ centerX, spacing 8 ]
                [ UI.button "mald" NextQuestion
                , UI.button "unmald" PreviousQuestion
                ]
            , el [ centerX ] <|
                case TraversalList.current model.questions of
                    TraversalList.AtItem question ->
                        text question.text

                    TraversalList.AfterList ->
                        text "You're done!"

                    TraversalList.BeforeList ->
                        text "Press mald to get started."
            ]
    }


edges : { top : number, right : number, bottom : number, left : number }
edges =
    { top = 0
    , right = 0
    , bottom = 0
    , left = 0
    }


viewNode : Model -> Node -> Element Msg
viewNode model node =
    column
        [ paddingEach { edges | left = 6 }
        ]
        [ row
            [ Background.color
                (if model.selectedNode == Just (nID node) then
                    rgb255 0 0 0

                 else
                    rgb255 255 255 255
                )
            , Font.color
                (if model.selectedNode == Just (nID node) then
                    rgb255 255 255 255

                 else
                    rgb255 0 0 0
                )
            , padding 6
            , pointer
            , onClick (NodeClicked <| nID node)
            ]
            [ text (node |> nLabel) ]
        , column [] (node |> nChildren |> List.map (viewNode model))
        ]


viewEl : Model -> Element Msg
viewEl model =
    row [] [ viewNode model model.rootNode ]
