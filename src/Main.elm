module MedLog exposing (..)

import HttpHelpers exposing (..)
import Model exposing (..)
import EntryForm
import PageListEntries exposing (..)

import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http

import Time
import Task


backendUrl : String
backendUrl = "http://localhost:9090"


init : ( Model, Cmd Msg )
init =
    let
        model = Model Nothing defaultEntries False defaultEntry
    in
        ( model, getUser )


isLoggedIn : Model -> Bool
isLoggedIn model =
    case model.user of
        Just _ ->
            True
        Nothing ->
            False


-- Update

type Msg
    = GetEntriesResult (Result Http.Error Entries)
    | GetUserResult (Result Http.Error String)
    | OnNewEntry
    | NavigateHome
    | OnSetPage Int
    | Logout
    | LogoutResult (Result Http.Error String)

    | EntryFormMsg EntryForm.FormMsg
    | EntryFormCancel
    | EntryFormSave
    | NewEntryTimestamp Time.Time
    | EntryFormSaveResult (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EntryFormMsg formMsg ->
            ( { model | newEntry = EntryForm.update formMsg model.newEntry }, Cmd.none)

        EntryFormCancel ->
            ( { model | showNewEntryForm = False }, Cmd.none ) -- IMPLEMENT

        EntryFormSave ->
            ( model, getTimestamp NewEntryTimestamp ) -- IMPLEMENT

        NewEntryTimestamp time ->
            let
                timeStamp = round <| Time.inSeconds time
                newEntry = model.newEntry
                entry = { newEntry | timeStamp = timeStamp }
            in
                ( { model | newEntry = entry }
                , saveNewEntry EntryFormSaveResult entry
                )

        EntryFormSaveResult (Ok id) ->
            ( { model | showNewEntryForm = False }, getEntries GetEntriesResult model.entries )

        EntryFormSaveResult (Err error) ->
            ( handleHttpError error model, Cmd.none )

        Logout ->
            ( { model | user = Nothing }, logoutUser )

        GetEntriesResult (Ok entries) ->
            ( { model | entries = entries }, Cmd.none )

        GetEntriesResult (Err error) ->
            ( handleHttpError error model, Cmd.none )

        GetUserResult (Ok username) ->
            ( { model | user = Just username }, getEntries GetEntriesResult model.entries )

        GetUserResult (Err error) ->
            ( handleHttpError error model, Cmd.none )

        LogoutResult (Ok username) ->
            ( { model | user = Just username }, Cmd.none )

        LogoutResult (Err error) ->
            ( handleHttpError error model, Cmd.none)

        NavigateHome ->
            ( { model | showNewEntryForm = False }, Cmd.none )

        OnSetPage page ->
            let
                oldEntries = model.entries
                entries = { oldEntries | pageNo = page }
            in
                ( { model | entries = entries }, getEntries GetEntriesResult entries )

        OnNewEntry ->
            ( { model | showNewEntryForm = True, newEntry = defaultEntry }, Cmd.none )


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


-- Commands

getUser : Cmd Msg
getUser =
    userDecoder
        |> getWithCredentials (backendUrl ++ "/user")
        |> Http.send GetUserResult


logoutUser : Cmd Msg
logoutUser =
    userDecoder
        |> deleteWithCredentials (backendUrl ++ "/user")
        |> Http.send LogoutResult


getEntries : (Result Http.Error Entries -> Msg) -> Entries -> Cmd Msg
getEntries msg entries =
    let
        params = [ ("pageSize", toString entries.pageSize)
                 , ("pageNo", toString entries.pageNo)
                 ]
        url = urlWithQuery (backendUrl ++ "/entries") params
    in

    resultsDecoder
        |> getWithCredentials url
        |> Http.send msg


getTimestamp : (Time.Time -> Msg) -> Cmd Msg
getTimestamp msg =
    Task.perform msg Time.now


saveNewEntry : (Result Http.Error String -> Msg) -> Entry -> Cmd Msg
saveNewEntry msg entry =
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
            |> Http.send msg


saveNewEntryDecoder : Decoder String
saveNewEntryDecoder =
    Decode.field "id" Decode.string

-- Decoders

userDecoder : Decoder String
userDecoder =
    Decode.field "user" Decode.string


resultsDecoder : Decoder (Entries)
resultsDecoder =
    Decode.map4 Entries
        (field "results" <| Decode.list entryDecoder)
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


-- View

view : Model -> Html Msg
view model =
    let
        page = if isLoggedIn model then
                   if model.showNewEntryForm then
                       EntryForm.viewAddNewEntry EntryFormSave EntryFormCancel EntryFormMsg model.newEntry
                   else
                       viewListEntries OnNewEntry OnSetPage model.entries
               else
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
              [ p [ class "display-4" ]
                  [ text "Welcome" ]
              , p []
                  [ text "This is the MedLog demo application." ]
              , p []
                  [ text "Please sign in with your google account to proceed" ]
              ]
        ]


homeLinkButton : Html Msg
homeLinkButton =
    a [ class "navbar-brand", onClick NavigateHome, href "" ]
      [ text "MedLog" ]


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


-- Main

main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
