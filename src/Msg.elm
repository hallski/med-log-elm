module Msg exposing (..)

import Model exposing (Entries)
import Http exposing (Error)
import Time

type Msg
    = NewEntries (Result Http.Error Entries)
    | NewUser (Result Http.Error String)
    | OnNewEntry
    | OnGoHome
    | OnSetPage Int
    | Logout
    | LogoutUserDone (Result Http.Error String)
    | NewEntryDone Bool -- Whether a new entry was added or not
    | NewEntryFormChange NewEntryFormMsg

type NewEntryFormMsg
    = NewEntryHoursOfSleepChange (String)
    | NewEntryRestingPulseChange (String)
    | NewEntryTagChange (String)
    | NewEntrySave
    | NewEntryTimestamp Time.Time
    | NewEntrySaveDone (Result Http.Error String)

type SubUpdateResult
    = Message Msg
    | Command (Cmd Msg)
