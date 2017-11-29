module MedLog exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode
import Http
import Navigation exposing (Location)
import UrlParser as UP
import Time
import HttpHelpers exposing (..)
import Task

backendUrl : String
backendUrl = "http://localhost:9090"


-- Model

type Route
    = RootRoute
    | AddEntryRoute
    | NewEntryRoute
    | NotFoundRoute

type alias Model =
    { user : Maybe String
    , entries : List Entry
    , route : Route
    , newEntry : NewEntry
    }

type alias NewEntry =
    { hoursOfSleep : Float
    , tag : String
    , restingPulse : Int
    , timestamp: Int
    }

defaultNewEntry : NewEntry
defaultNewEntry =
    NewEntry 8 "" 80 0

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
        ( Model Nothing [] currentRoute defaultNewEntry, getUser )

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
    | OnNewEntry
    | Logout
    | LogoutUserDone (Result Http.Error String)
    | OnLocationChange (Location)
    | NewEntrySave
    | NewEntryTimestamp (Time.Time)
    | NewEntrySaveDone (Result Http.Error String)
    | NewEntryCancel
    | NewEntryFormChange (NewEntryFormMsg)

type NewEntryFormMsg
    = NewEntryHoursOfSleepChange (String)
    | NewEntryRestingPulseChange (String)
    | NewEntryTagChange (String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewEntryFormChange formMsg ->
            ( { model | newEntry = updateNewEntry formMsg model.newEntry }, Cmd.none)
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
        OnNewEntry ->
            ( { model | route = NewEntryRoute, newEntry = defaultNewEntry }, getTimestamp )
        NewEntryTimestamp time ->
            let
                oldEntry = model.newEntry
                timestamp = round (Time.inSeconds time)
                newEntry = { oldEntry | timestamp = timestamp }
            in
                ( { model | newEntry = newEntry }, Cmd.none )
        NewEntrySave ->
            let
                newEntry = model.newEntry
            in
                ( { model | newEntry = defaultNewEntry }, saveNewEntry newEntry)
        NewEntrySaveDone (Ok result) ->
            ( { model | route = RootRoute }, getEntries )
        NewEntrySaveDone (Err error) ->
            case error of
                Http.BadStatus msg ->
                    let
                        _ = Debug.log "Error: " msg
                    in
                        ( { model | user = Nothing }, Cmd.none )

                Http.BadPayload msg _ ->
                    let
                        _ = Debug.log "Error: " msg
                    in
                        ( { model | user = Nothing }, Cmd.none )
                _ ->
                    ( { model | user = Nothing }, Cmd.none )
        NewEntryCancel ->
            ( { model | route = RootRoute, newEntry = defaultNewEntry }, Cmd.none )

updateNewEntry msg newEntry =
    case msg of
        NewEntryHoursOfSleepChange value ->
            { newEntry | hoursOfSleep = parseFloat value  }
        NewEntryRestingPulseChange value ->
            { newEntry | restingPulse = parseInt value }
        NewEntryTagChange value ->
            { newEntry | tag = value }


parseFloat : String -> Float
parseFloat = Result.withDefault 0.0 << String.toFloat

parseInt : String -> Int
parseInt = Result.withDefault 0 << String.toInt

-- Commands
getTimestamp : Cmd Msg
getTimestamp =
    Task.perform NewEntryTimestamp Time.now

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

saveNewEntry : NewEntry -> Cmd Msg
saveNewEntry entry =
    let
        body =
            Encode.object
                [ ("hoursOfSleep", Encode.float entry.hoursOfSleep)
                , ("restingPulse", Encode.int entry.restingPulse)
                , ("tag", Encode.string entry.tag)
                , ("timestamp", Encode.int entry.timestamp)
                ]
    in
        saveNewEntryDecoder
            |> postWithCredentials (backendUrl ++ "/entries") body
            |> Http.send NewEntrySaveDone

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

saveNewEntryDecoder : Decoder String
saveNewEntryDecoder =
    Decode.field "id" Decode.string

-- View

view : Model -> Html Msg
view model =
    let
        page = if isLoggedIn model then
                    if model.route == RootRoute then
                        viewLoggedIn model
                    else
                        viewNewEntry model.newEntry
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
    if List.length model.entries == 0 then
        viewNoEntries
    else
        viewShowEntries model.entries

viewNoEntries : Html Msg
viewNoEntries =
    div [ class "jumbotron" ]
        [ div [ class "display-5 text-center" ] [ text "You have not made any log entries yet!" ]
        , div [ class "text-center" ]
              [ button [ type_ "button", class "btn btn-primary", onClick OnNewEntry ]
                       [ text "Add your first log-entry!" ]
              ]
        ]

viewShowEntries : List Entry -> Html Msg
viewShowEntries entries =
    div []
        [ nav [ class "navbar nav-fill justify-content-between"]
              [ -- PageSelector
                ul [ class "nav nav-pills" ]
                   [ li [ class "nav-item" ]
                        [ button [ type_ "button", class "btn btn-outline-secondary", onClick OnNewEntry ]
                                 [ text "Add New" ]
                        ]
                   ]
              ]
        , div [] [ viewEntryTable entries ]
        ]

viewEntryTable : List Entry -> Html Msg
viewEntryTable entries =
    let
        headers = [ "Hours of sleep"
                  , "Resting pulse"
                  , "Tag"
                  , "Timestamp"
                  ]
        tableHeader = \str -> th [] [ text str ]
        h = thead [] [ tr [] (List.map tableHeader headers) ]
        rows = h :: List.map viewEntryRow entries
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
    if isLoggedIn model then
        button [ class "btn btn-outline-secondary my-2 my-sm-0" , onClick Logout ]
               [ text "Logout" ]
    else
        a [ class "btn btn-success my-2 my-sm-0" , href (backendUrl ++ "/login") ]
          [ text "Login" ]

viewNewEntry : NewEntry -> Html Msg
viewNewEntry entry =
    div [ class "container" ]
        [ div [ class "display-4" ] [ text "Input your Info" ]
            , viewSliderInput
                "Hours of sleep" "hoursOfSleep" entry.hoursOfSleep
                0 12 0.5 (NewEntryFormChange << NewEntryHoursOfSleepChange)
            , viewSliderInput
                "Resting pulse" "restingPulse" (toFloat entry.restingPulse)
                40 110 1 (NewEntryFormChange << NewEntryRestingPulseChange)
            , viewTextInput
                "Tag" "tag" entry.tag (NewEntryFormChange << NewEntryTagChange)
            , span [ class "float-left" ]
                   [ a [ class "btn btn-secondary", onClick NewEntryCancel ]
                       [ text "Cancel" ]
                   ]
            , span [ class "float-right" ]
                   [ button [ class "btn btn-primary", onClick NewEntrySave ]
                            [ text "Save" ]
                   ]
            ]


viewTextInput : String -> String -> String -> (String -> msg) -> Html msg
viewTextInput label n v onInputMsg =
    div [ class "input-group" ]
        [ span [ class "input-group-addon col-3 text-md-center" ] [ text label ]
        , input
            [ type_ "text"
            , class "form-control col-9"
            , name n
            , value v
            , onInput onInputMsg
            ]
            []
        ]

viewSliderInput : String -> String -> Float -> Float -> Float -> Float -> (String -> msg) -> Html msg
viewSliderInput label n v minValue maxValue stepValue onInputMsg =
    div [ class "input-group" ]
        [ span [ class "input-group-addon col-3 text-md-center" ] [ text label ]
        , div [ class "form-control col-9" ]
              [ span [ style [ ("marginRight", "10px") ]] [ text (toString v) ]
              , input
                [ style [ ("width", "100%") ]
                , type_ "range"
                , Html.Attributes.min (toString minValue)
                , Html.Attributes.max (toString maxValue)
                , step (toString stepValue)
                , name n
                , value (toString v)
                , onInput onInputMsg
                ]
                []
              ]
        ]
-- Main

main : Program Never Model Msg
main =
    Navigation.program OnLocationChange
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
