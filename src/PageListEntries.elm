module PageListEntries exposing (viewListEntries)

import Model exposing (Entry, Entries)
import Msg exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)


-- Views
viewListEntries : Entries -> Html Msg
viewListEntries entries =
    if List.length entries.entries == 0 then
        viewNoEntries
    else
        viewShowEntries entries

viewPageSelector : Entries -> Html Msg
viewPageSelector e =
    let
        r = List.range 1 e.pageCount
        g = List.map (viewPageLink e.pageNo) r
    in
        ul [ class "nav pagination" ] g

viewPageLink : Int -> Int -> Html Msg
viewPageLink index pageNo =
    let
        class_ = "page-item" ++ if index == pageNo then " active" else ""
    in
        li [ class class_ ]
           [ a [ class "page-link", onClick (OnSetPage pageNo) ]
               [ text (toString pageNo) ]
           ]

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
