module Msg exposing (..)

import Model exposing (Entries)
import Http exposing (Error)
import Time

type Msg
    = GetEntriesResult (Result Http.Error Entries)
    | GetUserResult (Result Http.Error String)
    | OnNewEntry
    | NavigateHome
    | OnSetPage Int
    | Logout
    | LogoutResult (Result Http.Error String)
    | NewEntryDone Bool -- Whether a new entry was added or not
    | SubAddEntry AddNewEntryMsg

type AddNewEntryMsg
    = AddNewEntryHoursOfSleepChange (String)
    | AddNewEntryRestingPulseChange (String)
    | AddNewEntryTagChange (String)
    | AddNewEntrySave
    | AddNewEntryTimestamp Time.Time
    | AddNewEntrySaveDone (Result Http.Error String)

type SubUpdateResult
    = Message Msg
    | Command (Cmd Msg)
