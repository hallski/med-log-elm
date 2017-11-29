module HttpHelpers exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)

requestWithCredentials : String -> String -> Decoder a -> Http.Request a
requestWithCredentials method url decoder =
    Http.request
    { method = method
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = True
    }


getWithCredentials : String -> Decoder a -> Http.Request a
getWithCredentials = requestWithCredentials "GET"

deleteWithCredentials : String -> Decoder a -> Http.Request a
deleteWithCredentials = requestWithCredentials "DELETE"
