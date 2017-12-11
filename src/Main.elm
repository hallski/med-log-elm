module MedLog exposing (..)

import EntriesTable exposing (..)
import EntryForm
import EntryModel exposing (..)
import HttpHelpers exposing (..)
import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Http
import Task
import Time


backendUrl : String
backendUrl =
    "http://localhost:9090"



-- Model


type alias User =
    String


type State
    = NotLoggedIn
    | ShowingEntries
    | ShowingEntryForm Entry


type alias Model =
    { state : State
    , entries : Entries
    }


init : ( Model, Cmd Msg )
init =
    let
        model =
            Model NotLoggedIn defaultEntries
    in
        ( model, getUser )



-- Update


type Msg
    = GetEntriesResult (Result Http.Error Entries)
    | GetUserResult (Result Http.Error String)
    | OnNewEntry
    | NavigateHome
    | OnSetPage Int
    | Logout
    | LogoutResult (Result Http.Error String)
      -- New Entry Form
    | EntryFormMsg EntryForm.FormMsg
    | EntryFormCancel
    | EntryFormSave
    | NewEntryTimestamp Time.Time
    | EntryFormSaveResult (Result Http.Error String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Logout ->
            model ! [ logoutUser ]

        GetEntriesResult (Ok entries) ->
            ( { model | state = ShowingEntries, entries = entries }, Cmd.none )

        GetEntriesResult (Err error) ->
            ( handleHttpError error model, Cmd.none )

        GetUserResult (Ok username) ->
            ( { model | state = ShowingEntries }, getEntries GetEntriesResult model.entries )

        GetUserResult (Err error) ->
            ( handleHttpError error model, Cmd.none )

        LogoutResult (Ok username) ->
            { model | state = NotLoggedIn } ! []

        LogoutResult (Err error) ->
            ( handleHttpError error model, Cmd.none )

        NavigateHome ->
            case model.state of
                ShowingEntryForm entry ->
                    { model | state = ShowingEntries } ! [ getEntries GetEntriesResult model.entries ]

                _ ->
                    model ! []

        OnSetPage page ->
            let
                oldEntries =
                    model.entries

                entries =
                    { oldEntries | pageNo = page }
            in
                ( { model | entries = entries }, getEntries GetEntriesResult entries )

        OnNewEntry ->
            ( { model | state = ShowingEntryForm defaultEntry }, Cmd.none )

        -- New Entry Form
        EntryFormMsg formMsg ->
            case model.state of
                ShowingEntryForm entry ->
                    { model | state = ShowingEntryForm <| EntryForm.update formMsg entry } ! []

                _ ->
                    model ! []

        EntryFormCancel ->
            ( { model | state = ShowingEntries }, Cmd.none )

        EntryFormSave ->
            ( model, getTimestamp NewEntryTimestamp )

        NewEntryTimestamp time ->
            let
                timeStamp =
                    round <| Time.inSeconds time

                newEntry =
                    case model.state of
                        ShowingEntryForm entry ->
                            entry

                        _ ->
                            defaultEntry

                entry =
                    { newEntry | timeStamp = timeStamp }
            in
                ( { model | state = ShowingEntryForm entry }
                , saveNewEntry EntryFormSaveResult entry
                )

        EntryFormSaveResult (Ok id) ->
            ( { model | state = ShowingEntries }, getEntries GetEntriesResult model.entries )

        EntryFormSaveResult (Err error) ->
            ( handleHttpError error model, Cmd.none )


handleHttpError : Http.Error -> Model -> Model
handleHttpError error model =
    case error of
        Http.BadStatus msg ->
            let
                _ =
                    Debug.log "Error: " msg
            in
                { model | state = NotLoggedIn }

        Http.BadPayload msg _ ->
            let
                _ =
                    Debug.log "Error: " msg
            in
                { model | state = NotLoggedIn }

        _ ->
            { model | state = NotLoggedIn }



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
        params =
            [ ( "pageSize", toString entries.pageSize )
            , ( "pageNo", toString entries.pageNo )
            ]

        url =
            urlWithQuery (backendUrl ++ "/entries") params
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
                [ ( "hoursOfSleep", Encode.float entry.hoursOfSleep )
                , ( "restingPulse", Encode.int entry.restingPulse )
                , ( "tag", Encode.string entry.tag )
                , ( "timestamp", Encode.int entry.timeStamp )
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


resultsDecoder : Decoder Entries
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
        page =
            case model.state of
                NotLoggedIn ->
                    viewWelcome

                ShowingEntries ->
                    viewEntriesTable OnNewEntry OnSetPage model.entries

                ShowingEntryForm entry ->
                    EntryForm.viewAddNewEntry EntryFormSave EntryFormCancel EntryFormMsg entry
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
    case model.state of
        NotLoggedIn ->
            a
                [ class "btn btn-success my-2 my-sm-0"
                , href (backendUrl ++ "/login")
                ]
                [ text "Login" ]

        _ ->
            button
                [ class "btn btn-outline-secondary my-2 my-sm-0"
                , onClick Logout
                ]
                [ text "Logout" ]



-- Main


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }
