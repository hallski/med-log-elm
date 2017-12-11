module EntryForm exposing (FormMsg, update, viewAddNewEntry)

import EntryModel exposing (Entry)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)


-- Update


type FormMsg
    = SetHoursOfSleep String
    | SetRestingPulse String
    | SetTag String


update : FormMsg -> Entry -> Entry
update msg entry =
    case msg of
        SetHoursOfSleep value ->
            { entry | hoursOfSleep = parseFloat value }

        SetRestingPulse value ->
            { entry | restingPulse = parseInt value }

        SetTag value ->
            { entry | tag = value }


parseFloat : String -> Float
parseFloat =
    Result.withDefault 0.0 << String.toFloat


parseInt : String -> Int
parseInt =
    Result.withDefault 0 << String.toInt



-- Views


viewTextInput : String -> String -> String -> (String -> msg) -> Html msg
viewTextInput label n v onInputMsg =
    div [ class "input-group" ]
        [ span [ class "input-group-addon col-3 text-md-center" ]
            [ text label ]
        , input
            [ type_ "text"
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
            [ span [ style [ ( "marginRight", "10px" ) ] ]
                [ text (toString v) ]
            , input
                [ style [ ( "width", "100%" ) ]
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


viewAddNewEntry : msg -> msg -> (FormMsg -> msg) -> Entry -> Html msg
viewAddNewEntry saveMsg cancelMsg msgWrap entry =
    div [ class "container" ]
        [ div [ class "display-4" ]
            [ text "Input your Info" ]
        , viewSliderInput
            "Hours of sleep"
            "hoursOfSleep"
            entry.hoursOfSleep
            0
            12
            0.5
            (msgWrap << SetHoursOfSleep)
        , viewSliderInput
            "Resting pulse"
            "restingPulse"
            (toFloat entry.restingPulse)
            40
            110
            1
            (msgWrap << SetRestingPulse)
        , viewTextInput
            "Tag"
            "tag"
            entry.tag
            (msgWrap << SetTag)
        , span [ class "float-left" ]
            [ a
                [ class "btn btn-secondary"
                , onClick cancelMsg
                ]
                [ text "Cancel" ]
            ]
        , span [ class "float-right" ]
            [ button
                [ class "btn btn-primary"
                , onClick saveMsg
                ]
                [ text "Save" ]
            ]
        ]
