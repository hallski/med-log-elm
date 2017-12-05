module EntriesTable exposing (viewEntriesTable)

import EntryModel exposing (Entry, Entries)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- Views
viewEntriesTable : msg -> (Int -> msg) -> Entries -> Html msg
viewEntriesTable newEntryMsg pageSelMsg entries =
    if List.length entries.entries == 0 then
        viewNoEntries newEntryMsg
    else
        viewShowEntries newEntryMsg pageSelMsg entries


viewPageSelector : (Int -> msg) -> Entries -> Html msg
viewPageSelector pageSelMsg entries =
    let
        pageLinks = List.range 1 entries.pageCount
                        |> List.map (viewPageLink pageSelMsg entries.pageNo)
    in
        ul [ class "nav pagination" ] pageLinks


viewPageLink : (Int -> msg) -> Int -> Int -> Html msg
viewPageLink pageSelMsg index pageNo =
    let
        class_ = "page-item" ++ if index == pageNo then " active" else ""
    in
        li [ class class_ ]
           [ a [ class "page-link", onClick (pageSelMsg pageNo) ]
               [ text (toString pageNo) ]
           ]


viewNoEntries : msg -> Html msg
viewNoEntries newEntryMsg =
    div [ class "jumbotron" ]
        [ div [ class "display-5 text-center" ] [ text "You have not made any log entries yet!" ]
        , div [ class "text-center" ]
              [ button [ type_ "button", class "btn btn-primary", onClick newEntryMsg ]
                       [ text "Add your first log-entry!" ]
              ]
        ]


viewShowEntries : msg -> (Int -> msg) -> Entries -> Html msg
viewShowEntries newEntryMsg pageSelMsg entries =
    div []
        [ nav [ class "navbar nav-fill justify-content-between"]
              [ viewPageSelector pageSelMsg entries
              , ul [ class "nav nav-pills" ]
                   [ li [ class "nav-item" ]
                        [ button [ type_ "button"
                                 , class "btn btn-outline-secondary"
                                 , onClick newEntryMsg
                                 ]
                                 [ text "Add New" ]
                        ]
                   ]
              ]
        , div [] [ viewEntryTable entries.entries ]
        ]


viewEntryTable : List Entry -> Html msg
viewEntryTable entries =
    let
        headers = [ "Hours of sleep"
                  , "Resting pulse"
                  , "Tag"
                  , "Timestamp"
                  ]
        tableHeader = \str -> th [] [ text str ]

        rows = thead [] [ tr [] (List.map tableHeader headers) ]
                   :: (List.map viewEntryRow entries)
    in
        table [ class "table" ] rows


viewEntryRow : Entry -> Html msg
viewEntryRow entry =
    tr []
       [ td [] [ text <| toString entry.hoursOfSleep ]
       , td [] [ text <| toString entry.restingPulse ]
       , td [] [ text entry.tag ]
       , td [] [ text <| toString entry.timeStamp ]
       ]
