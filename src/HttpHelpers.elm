module HttpHelpers exposing (..)

import Http
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode exposing (Value)


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


postWithCredentials : String -> Encode.Value -> Decoder a -> Http.Request a
postWithCredentials url body decoder =
    Http.request
        { method = "post"
        , headers = []
        , url = url
        , body = Http.jsonBody body
        , expect = Http.expectJson decoder
        , timeout = Nothing
        , withCredentials = True
        }


urlWithQuery : String -> List (String, String) -> String
urlWithQuery url params =
    let
        enc = Http.encodeUri

        paramString =
            params
                |> String.join "&" << List.map (\(k, v) -> (enc k) ++ "=" ++ (enc v))
    in
        url ++ "?" ++ paramString
