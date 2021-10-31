port module Storage exposing (..)

import Json.Decode as D
import Json.Encode as E


port save : E.Value -> Cmd msg


saveStorage : Storage -> Cmd msg
saveStorage storage =
    save (encoder storage)


type alias Storage =
    { user : Maybe User
    }


type alias User =
    { name : String
    , token : String
    }


initial : Storage
initial =
    { user = Nothing
    }


encoder : Storage -> E.Value
encoder storage =
    E.object
        (case storage.user of
            Nothing ->
                []

            Just user ->
                [ ( "user", userEncoder user ) ]
        )


userDecoder : D.Decoder User
userDecoder =
    D.map2 User
        (D.field "name" D.string)
        (D.field "token" D.string)


userEncoder : User -> E.Value
userEncoder user =
    E.object
        [ ( "name", E.string user.name )
        , ( "token", E.string user.token )
        ]


decoder : D.Decoder Storage
decoder =
    D.map Storage
        (D.maybe (D.field "user" userDecoder))


fromJson : D.Value -> Storage
fromJson value =
    value
        |> D.decodeValue decoder
        |> Result.withDefault initial


toJson : Storage -> E.Value
toJson =
    encoder
