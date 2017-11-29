module MedLog exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)

import Json.Decode as Decode exposing (Decoder, field, succeed)
import Http
import Navigation exposing (Location)
import UrlParser as UP

import HttpHelpers exposing (..)

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

isLoggedIn : Model -> Bool
isLoggedIn model =
    case model.user of
        Just _ -> True
        Nothing -> False


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
    = NewEntries (Result Http.Error (List Entry))
    | NewUser (Result Http.Error String)
    | Logout
    | LogoutUserDone (Result Http.Error String)
    | OnLocationChange (Location)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewEntries (Ok entries) ->
            ( { model | entries = entries }, Cmd.none )
        NewEntries (Err error) ->
            case error of
                Http.BadPayload msg _ ->
                    let
                        _ = Debug.log "Error: " msg
                    in
                        ( { model | user = Nothing }, Cmd.none )
                _ ->
                    ( { model | user = Nothing }, Cmd.none )

        NewUser (Ok username) ->
            ( { model | user = Just username }, getEntries )
        NewUser (Err error) ->
            case error of
                Http.BadPayload msg _ ->
                    let
                        _ = Debug.log "Error: " msg
                    in
                        ( { model | user = Nothing }, Cmd.none )
                _ ->
                    ( { model | user = Nothing }, Cmd.none )
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
                fetchCommand = if isLoggedIn model then getEntries else Cmd.none
            in
                ( { model | route = newRoute }, fetchCommand )

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

getEntries : Cmd Msg
getEntries =
    resultsDecoder
        |> getWithCredentials (backendUrl ++ "/entries")
        |> Http.send NewEntries

userDecoder : Decoder String
userDecoder =
    Decode.field "user" Decode.string

resultsDecoder : Decoder (List Entry)
resultsDecoder =
    Decode.field "results" (Decode.list entryDecoder)

entryDecoder : Decoder Entry
entryDecoder =
    Decode.map5 Entry
        (field "id" Decode.string)
        (field "hoursOfSleep" Decode.float)
        (field "tag" Decode.string)
        (field "restingPulse" Decode.int)
        (field "timestamp" Decode.int)


-- View

view : Model -> Html Msg
view model =
    let
        page = if isLoggedIn model then
                    viewLoggedIn model
                else
                    viewWelcome
    in
        viewTemplate model page

viewTemplate : Model -> Html Msg -> Html Msg
viewTemplate model page =
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
    let
        headers = [ "Hours of sleep"
                  , "Resting pulse"
                  , "Tag"
                  , "Timestamp"
                  ]
        tableHeader = \str -> th [] [ text str ]
        h = thead [] [ tr [] (List.map tableHeader headers) ]
        rows = h :: List.map viewEntryRow model.entries
    in
        table [ class "table" ] rows

viewEntryRow : Entry -> Html Msg
viewEntryRow entry =
    tr []
       [ td [] [ text (toString entry.hoursOfSleep) ]
       , td [] [ text (toString entry.restingPulse) ]
       , td [] [ text entry.tag ]
       , td [] [ text (toString entry.timeStamp) ]
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
