module MedLog exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Json.Decode as Decode exposing (Decoder, field, succeed)
import Http
import Navigation exposing (Location)
import UrlParser as UP


backendUrl : String
backendUrl = "http://localhost:9090"


-- Model

type Route
    = RootRoute
    | AddEntryRoute
    | NotFoundRoute

type alias Model =
    { user : Maybe String
    , entries : List Entry
    , route : Route
    }

type alias Entry =
    { id: String
    , hoursOfSleep : Float
    , tag : String
    , restingPulse : Int
    , timeStamp : Int
    }

init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute = parseLocation location
    in
        ( Model Nothing [] currentRoute, getUser )


-- Routing

matchers : UP.Parser (Route -> a) a
matchers =
    UP.oneOf
        [ UP.map RootRoute UP.top
        , UP.map AddEntryRoute (UP.s "addEntry")
        ]

parseLocation : Location -> Route
parseLocation location =
    case (UP.parseHash matchers location) of
        Just route ->
            route
        Nothing ->
            NotFoundRoute

-- Update

type Msg
    = NewEntry
    | NewUser (Result Http.Error String)
    | Login
    | Logout
    | LogoutUserDone (Result Http.Error String)
    | OnLocationChange (Location)

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
        OnLocationChange location ->
            let
                newRoute = parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

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
    let
        page = case model.user of
            Just _ ->
                viewLoggedIn model
            Nothing ->
                viewWelcome
    in

    div [ class "container-fluid" ]
        [ nav [ class "navbar navbar-dark bg-dark" ]
              [ homeLinkButton
              , loginButton model
              ]
        , page
        ]

viewWelcome : Html Msg
viewWelcome =
    div [ class "container" ]
        [ div [ class "jumbotron" ]
              [ p [ class "display-4" ] [ text "Welcome" ]
              , p [] [ text "This is the MedLog demo application." ]
              , p [] [ text "Please sign in with your google account to proceed" ]
              ]
        ]

viewLoggedIn : Model -> Html Msg
viewLoggedIn model =
    text "You are logged in!"

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
            a [ class "btn btn-success my-2 my-sm-0" , href (backendUrl ++ "/login") ]
              [ text "Login" ]

-- Main

main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
