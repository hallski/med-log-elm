module PageAddNewEntry exposing (updateNewEntry, viewAddNewEntry)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)

import Model exposing (Entry, backendUrl)
import Msg exposing (..)

import Json.Decode as Decode exposing (Decoder, field, succeed)
import Json.Encode as Encode
import Task
import Time

import HttpHelpers exposing (postWithCredentials)
import Http


-- Update
updateNewEntry : AddNewEntryMsg -> Entry -> ( Entry, SubUpdateResult )
updateNewEntry msg newEntry =
    case msg of
        AddNewEntryHoursOfSleepChange value ->
            ( { newEntry | hoursOfSleep = parseFloat value  }, Command Cmd.none )

        AddNewEntryRestingPulseChange value ->
            ( { newEntry | restingPulse = parseInt value }, Command Cmd.none )

        AddNewEntryTagChange value ->
            ( { newEntry | tag = value }, Command Cmd.none )

        AddNewEntryTimestamp time ->
            let
                timeStamp = round (Time.inSeconds time)
                entry = { newEntry | timeStamp = timeStamp }
            in
                ( entry, Command (saveNewEntry entry) )

        AddNewEntrySave ->
            ( newEntry, Command getTimestamp )

        AddNewEntrySaveDone (Ok result) ->
            ( newEntry, Message (NewEntryDone True) )

        AddNewEntrySaveDone (Err error) ->
            printHttpError error newEntry


parseFloat : String -> Float
parseFloat = Result.withDefault 0.0 << String.toFloat


parseInt : String -> Int
parseInt = Result.withDefault 0 << String.toInt


printHttpError : Http.Error -> Entry -> (Entry, SubUpdateResult)
printHttpError error entry =
    case error of
        Http.BadStatus msg ->
            let
                _ = Debug.log "Error: " msg
            in
                (entry, Command Cmd.none)

        Http.BadPayload msg _ ->
            let
                _ = Debug.log "Error: " msg
            in
                (entry, Command Cmd.none)
        _ ->
            (entry, Command Cmd.none)


-- Commands

getTimestamp : Cmd Msg
getTimestamp =
    Task.perform (SubAddEntry << AddNewEntryTimestamp) Time.now


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
            |> Http.send (SubAddEntry << AddNewEntrySaveDone)


saveNewEntryDecoder : Decoder String
saveNewEntryDecoder =
    Decode.field "id" Decode.string


-- Views

viewTextInput : String -> String -> String -> (String -> msg) -> Html msg
viewTextInput label n v onInputMsg =
    div [ class "input-group" ]
        [ span [ class "input-group-addon col-3 text-md-center" ]
               [ text label ]
        , input [ type_ "text"
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
        [ span [ class "input-group-addon col-3 text-md-center" ]
               [ text label ]
        , div [ class "form-control col-9" ]
              [ span [ style [ ("marginRight", "10px") ] ]
                     [ text (toString v) ]
              , input [ style [ ("width", "100%") ]
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


viewAddNewEntry : Entry -> Html Msg
viewAddNewEntry entry =
    let
        hoursOfSleepChangeMsg = SubAddEntry << AddNewEntryHoursOfSleepChange
        restingPulseChangeMsg = SubAddEntry << AddNewEntryRestingPulseChange
        tagChangeMsg          = SubAddEntry << AddNewEntryTagChange
        saveMsg               = SubAddEntry AddNewEntrySave
        cancelMsg             = NewEntryDone False
    in
        div [ class "container" ]
            [ div [ class "display-4" ]
                  [ text "Input your Info" ]
            , viewSliderInput
                "Hours of sleep" "hoursOfSleep" entry.hoursOfSleep
                0 12 0.5 hoursOfSleepChangeMsg
            , viewSliderInput
                "Resting pulse" "restingPulse" (toFloat entry.restingPulse)
                40 110 1 restingPulseChangeMsg
            , viewTextInput
                "Tag" "tag" entry.tag tagChangeMsg
            , span [ class "float-left" ]
                   [ a [ class "btn btn-secondary"
                       , onClick cancelMsg
                       ]
                       [ text "Cancel" ]
                   ]
            , span [ class "float-right" ]
                   [ button [ class "btn btn-primary"
                            , onClick saveMsg
                            ]
                            [ text "Save" ]
                   ]
            ]

