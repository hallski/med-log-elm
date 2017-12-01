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
    | NewEntrySave
    | NewEntryTimestamp Time.Time
    | NewEntrySaveDone (Result Http.Error String)
    | NewEntryCancel
    | NewEntryFormChange NewEntryFormMsg

type NewEntryFormMsg
    = NewEntryHoursOfSleepChange (String)
    | NewEntryRestingPulseChange (String)
    | NewEntryTagChange (String)
