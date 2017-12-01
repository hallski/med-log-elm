module Msg exposing (..)

import Model exposing (Entries)
import Http exposing (Error)
import Time

type Msg
    = GetEntriesResult (Result Http.Error Entries)
    | GetUserResult (Result Http.Error String)
    | OnNewEntry
    | OnGoHome
    | OnSetPage Int
    | Logout
    | LogoutUserDone (Result Http.Error String)
    | NewEntryDone Bool -- Whether a new entry was added or not
    | SubAddEntry AddNewEntryMsg

type AddNewEntryMsg
    = NewEntryHoursOfSleepChange (String)
    | NewEntryRestingPulseChange (String)
    | NewEntryTagChange (String)
    | NewEntrySave
    | NewEntryTimestamp Time.Time
    | NewEntrySaveDone (Result Http.Error String)

type SubUpdateResult
    = Message Msg
    | Command (Cmd Msg)
