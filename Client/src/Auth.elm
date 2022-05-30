module Auth exposing
    ( User
    , beforeProtectedInit
    )

import ElmSpa.Page as ElmSpa
import Gen.Route exposing (Route)
import Request exposing (Request)
import Shared


type alias User =
    Shared.User


beforeProtectedInit : Shared.Model -> Request -> ElmSpa.Protected User Route
beforeProtectedInit shared _ =
    case shared.user of
        Just it ->
            ElmSpa.Provide it

        Nothing ->
            ElmSpa.RedirectTo Gen.Route.Auth__Login
