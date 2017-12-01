module Model exposing (..)


-- For now, only use against server running on localhost.
backendUrl : String
backendUrl = "http://localhost:9090"


type Route
    = RootRoute
    | AddEntryRoute
    | NewEntryRoute


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


defaultEntry : Entry
defaultEntry =
    Entry "0" 8 "" 80 0
