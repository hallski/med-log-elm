module MedLog exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Json.Decode as Decode exposing (Decoder, field, succeed)
import Http


backendUrl : String
backendUrl = "http://localhost:9090"


-- Model

type alias Model =
    { user : Maybe String
    , entries : List Entry
    }

type alias Entry =
    { id: String
    , hoursOfSleep : Float
    , tag : String
    , restingPulse : Int
    , timeStamp : Int
    }

init : ( Model, Cmd Msg )
init =
    ( Model (Just "foo") [], getUser )


-- Update

type Msg
    = NewEntry
    | NewUser (Result Http.Error String)
    | Login
    | Logout
    | LogoutUserDone (Result Http.Error String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewEntry ->
            ( model, Cmd.none )
        NewUser (Ok username) ->
            ( { model | user = Just username }, Cmd.none )
        NewUser (Err error) ->
            case error of
                Http.BadPayload msg _ ->
                    let
                        _ = Debug.log "Error: " msg
                    in
                        ( { model | user = Nothing }, Cmd.none )
                _ ->
                    ( { model | user = Nothing }, Cmd.none )
        Login ->
            ( { model | user = Just "something" }, Cmd.none )
        Logout ->
            ( { model | user = Nothing }, logoutUser )
        LogoutUserDone (Ok username) ->
            ( { model | user = Just username }, Cmd.none )
        LogoutUserDone (Err error) ->
            case error of
                Http.BadPayload msg _ ->
                    let
                        _ = Debug.log "Error: " msg
                    in
                        ( { model | user = Nothing }, Cmd.none )
                _ ->
                    ( { model | user = Nothing }, Cmd.none )

-- Commands
getUser : Cmd Msg
getUser =
    userDecoder
    |> getWithCredentials (backendUrl ++ "/user")
    |> Http.send NewUser

logoutUser : Cmd Msg
logoutUser =
    userDecoder
    |> deleteWithCredentials (backendUrl ++ "/user")
    |> Http.send LogoutUserDone

userDecoder : Decoder String
userDecoder =
    Decode.field "user" Decode.string

getWithCredentials : String -> Decoder a -> Http.Request a
getWithCredentials url decoder =
    Http.request
    { method = "GET"
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = True
    }

deleteWithCredentials : String -> Decoder a -> Http.Request a
deleteWithCredentials url decoder =
    Http.request
    { method = "DELETE"
    , headers = []
    , url = url
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = True
    }


-- View

view : Model -> Html Msg
view model =
    div [ class "container-fluid" ]
        [ nav [ class "navbar navbar-dark bg-dark" ]
              [ homeLinkButton
              , loginButton model
              ]
        ]

homeLinkButton : Html Msg
homeLinkButton =
    a [ class "navbar-brand", href "/" ] [ text "MedLog" ]

loginButton : Model -> Html Msg
loginButton model =
    case model.user of
        Just _ ->
            button [ class "btn btn-outline-secondary my-2 my-sm-0" , onClick Logout ]
                   [ text "Logout" ]
        Nothing ->
            a [ class "btn btn-success my-2 my-sm-0" , onClick Login ]
              [ text "Login" ]

-- Main

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
