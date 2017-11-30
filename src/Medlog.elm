module MedLog exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode
import Http
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
    , entries : Entries
    , route : Route
    , newEntry : Entry
    }

type alias Entries =
    { entries: List Entry
    , pageNo: Int
    , pageSize: Int
    , pageCount: Int
    }

defaultEntries : Entries
defaultEntries =
    Entries [] 1 10 0

type alias Entry =
    { id: String
    , hoursOfSleep : Float
    , tag : String
    , restingPulse : Int
    , timeStamp : Int
    }

defaultEntry =
    Entry "0" 8 "" 80 0

init : ( Model, Cmd Msg )
init =
    let
        model = Model Nothing defaultEntries RootRoute defaultEntry
    in
        ( model, getUser )

isLoggedIn : Model -> Bool
isLoggedIn model =
    case model.user of
        Just _ -> True
        Nothing -> False


-- Update

type Msg
    = NewEntries (Result Http.Error Entries)
    | NewUser (Result Http.Error String)
    | OnNewEntry
    | OnGoHome
    | OnSetPage Int
    | Logout
    | LogoutUserDone (Result Http.Error String)
    | NewEntrySave
    | NewEntryTimestamp Time.Time
    | NewEntrySaveDone (Result Http.Error String)
    | NewEntryCancel
    | NewEntryFormChange NewEntryFormMsg

type NewEntryFormMsg
    = NewEntryHoursOfSleepChange (String)
    | NewEntryRestingPulseChange (String)
    | NewEntryTagChange (String)

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NewEntryFormChange formMsg ->
            ( { model | newEntry = updateNewEntry formMsg model.newEntry },
              Cmd.none)
        NewEntries (Ok entries) ->
            ( { model | entries = entries }, Cmd.none )
        NewEntries (Err error) ->
            ( handleHttpError error model, Cmd.none )
        NewUser (Ok username) ->
            ( { model | user = Just username }, getEntries model.entries )
        NewUser (Err error) ->
            ( handleHttpError error model, Cmd.none )
        Logout ->
            ( { model | user = Nothing }, logoutUser )
        LogoutUserDone (Ok username) ->
            ( { model | user = Just username }, Cmd.none )
        LogoutUserDone (Err error) ->
            ( handleHttpError error model, Cmd.none)
        OnGoHome ->
            ( { model | route = RootRoute }, Cmd.none )
        OnSetPage page ->
            let
                oldEntries = model.entries
                entries = { oldEntries | pageNo = page }
            in
                ( { model | entries = entries }, getEntries entries )
        OnNewEntry ->
            ( { model | route = NewEntryRoute, newEntry = defaultEntry }, Cmd.none )
        NewEntryTimestamp time ->
            let
                oldEntry = model.newEntry
                timeStamp = round (Time.inSeconds time)
                newEntry = { oldEntry | timeStamp = timeStamp }
            in
                ( { model | newEntry = newEntry }, saveNewEntry newEntry )
        NewEntrySave ->
            ( model, getTimestamp )
        NewEntrySaveDone (Ok result) ->
            ( { model | route = RootRoute }, getEntries model.entries )
        NewEntrySaveDone (Err error) ->
            ( handleHttpError error model, Cmd.none )
        NewEntryCancel ->
            ( { model | route = RootRoute }, Cmd.none )

updateNewEntry : NewEntryFormMsg -> Entry -> Entry
updateNewEntry msg newEntry =
    case msg of
        NewEntryHoursOfSleepChange value ->
            { newEntry | hoursOfSleep = parseFloat value  }
        NewEntryRestingPulseChange value ->
            { newEntry | restingPulse = parseInt value }
        NewEntryTagChange value ->
            { newEntry | tag = value }

handleHttpError : Http.Error -> Model -> Model
handleHttpError error model =
    case error of
        Http.BadStatus msg ->
            let
                _ = Debug.log "Error: " msg
            in
                { model | user = Nothing }

        Http.BadPayload msg _ ->
            let
                _ = Debug.log "Error: " msg
            in
                { model | user = Nothing }
        _ ->
            { model | user = Nothing }

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

getEntries : Entries -> Cmd Msg
getEntries entries =
    let
        params = [ ("pageSize", toString entries.pageSize)
                 , ("pageNo", toString entries.pageNo)
                 ]
        url = urlWithQuery (backendUrl ++ "/entries") params
    in

    resultsDecoder
        |> getWithCredentials url
        |> Http.send NewEntries

saveNewEntry : Entry -> Cmd Msg
saveNewEntry entry =
    let
        body =
            Encode.object
                [ ("hoursOfSleep", Encode.float entry.hoursOfSleep)
                , ("restingPulse", Encode.int entry.restingPulse)
                , ("tag", Encode.string entry.tag)
                , ("timestamp", Encode.int entry.timeStamp)
                ]
    in
        saveNewEntryDecoder
            |> postWithCredentials (backendUrl ++ "/entries") body
            |> Http.send NewEntrySaveDone


-- Decoders
userDecoder : Decoder String
userDecoder =
    Decode.field "user" Decode.string

resultsDecoder : Decoder (Entries)
resultsDecoder =
    Decode.map4 Entries
        (field "results" (Decode.list entryDecoder))
        (field "pageNo" Decode.int)
        (field "pageSize" Decode.int)
        (field "pageCount" Decode.int)

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
    if List.length model.entries.entries == 0 then
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

viewShowEntries : Entries -> Html Msg
viewShowEntries entries =
    div []
        [ nav [ class "navbar nav-fill justify-content-between"]
              [ viewPageSelector entries
              , ul [ class "nav nav-pills" ]
                   [ li [ class "nav-item" ]
                        [ button [ type_ "button", class "btn btn-outline-secondary", onClick OnNewEntry ]
                                 [ text "Add New" ]
                        ]
                   ]
              ]
        , div [] [ viewEntryTable entries.entries ]
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
    a [ class "navbar-brand", onClick OnGoHome ] [ text "MedLog" ]

loginButton : Model -> Html Msg
loginButton model =
    if isLoggedIn model then
        button [ class "btn btn-outline-secondary my-2 my-sm-0"
               , onClick Logout ]
               [ text "Logout" ]
    else
        a [ class "btn btn-success my-2 my-sm-0"
          , href (backendUrl ++ "/login") ]
          [ text "Login" ]

viewNewEntry : Entry -> Html Msg
viewNewEntry entry =
    let
        hoursOfSleepChangeMsg = NewEntryFormChange << NewEntryHoursOfSleepChange
        restingPulseChangeMsg = NewEntryFormChange << NewEntryRestingPulseChange
        tagChangeMsg = NewEntryFormChange << NewEntryTagChange
    in
        div [ class "container" ]
            [ div [ class "display-4" ] [ text "Input your Info" ]
                , viewSliderInput
                    "Hours of sleep" "hoursOfSleep" entry.hoursOfSleep
                    0 12 0.5 hoursOfSleepChangeMsg
                , viewSliderInput
                    "Resting pulse" "restingPulse" (toFloat entry.restingPulse)
                    40 110 1 restingPulseChangeMsg
                , viewTextInput
                    "Tag" "tag" entry.tag tagChangeMsg
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

viewPageLink : Int -> Int -> Html Msg
viewPageLink index pageNo =
    let
        class_ = "page-item" ++ if index == pageNo then " active" else ""
    in
        li [ class class_ ]
           [ a [ class "page-link", onClick (OnSetPage pageNo) ]
               [ text (toString pageNo) ]
           ]

viewPageSelector : Entries -> Html Msg
viewPageSelector e =
    let
        r = List.range 1 e.pageCount
        g = List.map (viewPageLink e.pageNo) r
    in
        ul [ class "nav pagination" ] g

-- Main

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
