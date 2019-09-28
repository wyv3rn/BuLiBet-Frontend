module Main exposing (main)

-- TODO do not clutter global namespace ...

import Browser
import Html exposing (Html, button, div, img, input, text)
import Html.Attributes exposing (placeholder, src, style, type_, value)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode
    exposing
        ( Decoder
        , andThen
        , bool
        , fail
        , field
        , index
        , int
        , list
        , map
        , map2
        , map3
        , map4
        , maybe
        , string
        , succeed
        )
import Regex



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


shortenTeamName : String -> String
shortenTeamName name =
    let
        regex =
            Maybe.withDefault
                Regex.never
                (Regex.fromString "[A-Z0-9\\.]{2,}")

        replaced =
            String.trim (Regex.replace regex (\_ -> "") name)
    in
    if replaced == "Union Berlin" then
        "Union"

    else
        replaced


type alias Team =
    { name : String
    , iconUrl : String
    }


type TeamRole
    = Home
    | Guest


type alias Score =
    Maybe ( Int, Int )


type alias Match =
    { home : Team, guest : Team, score : Score, finished : Bool }


type alias MatchDay =
    List Match


type FetchStatus
    = Idle
    | Fetching Int
    | FetchFailed String
    | FetchDone MatchDay


type alias Model =
    { selectedDay : Maybe Int
    , status : FetchStatus
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Idle, Cmd.none )



-- UPDATE


type Msg
    = DaySelected Int
    | GotMatchDay (Result Http.Error MatchDay)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DaySelected day ->
            selectDay model day

        GotMatchDay result ->
            case result of
                Ok matches ->
                    ( { model | status = FetchDone matches }, Cmd.none )

                Err e ->
                    ( Model Nothing (FetchFailed (fetchErrorMsg e)), Cmd.none )


fetchErrorMsg : Http.Error -> String
fetchErrorMsg e =
    case e of
        Http.BadUrl url ->
            "Bad url " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network error"

        Http.BadStatus status ->
            "Bad status: " ++ String.fromInt status

        Http.BadBody b ->
            "Bad Body: " ++ b


selectDay : Model -> Int -> ( Model, Cmd Msg )
selectDay model day =
    ( { model | selectedDay = Just day, status = Fetching day }
    , Http.get
        { url =
            "https://www.openligadb.de/api/getmatchdata/bl1/2019/"
                ++ String.fromInt day
        , expect = Http.expectJson GotMatchDay matchDayDecoder
        }
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        viewSiteDay =
            viewSite model.selectedDay
    in
    case model.status of
        Idle ->
            viewSiteDay (Html.text "Select a matchday below.")

        Fetching day ->
            viewSiteDay (text ("Fetching day " ++ String.fromInt day ++ " ..."))

        FetchFailed reason ->
            viewSiteDay
                (div [ style "color" "red" ]
                    [ text
                        ("Fetching failed, reason: "
                            ++ reason
                        )
                    ]
                )

        FetchDone matchDay ->
            viewSiteDay (viewMatchDay matchDay)


viewSite : Maybe Int -> Html Msg -> Html Msg
viewSite day content =
    Html.div []
        [ Html.p [] [ content ]
        , Html.hr [] []
        , Html.p [] [ viewSelection day ]
        ]


viewSelection : Maybe Int -> Html Msg
viewSelection day =
    let
        selectedDay =
            case day of
                Just selected ->
                    selected

                Nothing ->
                    -1

        dayList =
            List.range 1 34

        buttonList =
            List.map (createDayButton selectedDay) dayList
    in
    Html.div [] buttonList


createDayButton : Int -> Int -> Html Msg
createDayButton selectedDay day =
    let
        bg =
            if selectedDay == day then
                "#268bd2"

            else
                "white"
    in
    Html.button [ style "background-color" bg, onClick (DaySelected day) ]
        [ text (String.fromInt day) ]


viewTeam : Team -> TeamRole -> List (Html Msg)
viewTeam team role =
    case role of
        Home ->
            [ Html.td [ textAlign Right ] [ text team.name ]
            , Html.td [ textAlign Center ]
                [ img [ src team.iconUrl, style "height" "10pt" ] [] ]
            ]

        Guest ->
            [ Html.td [ textAlign Center ]
                [ img [ src team.iconUrl, style "height" "10pt" ] [] ]
            , Html.td [ textAlign Left ] [ text team.name ]
            ]


viewMatch : Match -> Html Msg
viewMatch match =
    let
        ( homeScore, guestScore, scoreColor ) =
            case match.score of
                Just ( a, b ) ->
                    ( String.fromInt a
                    , String.fromInt b
                    , if match.finished then
                        "black"

                      else
                        "red"
                    )

                Nothing ->
                    ( "-", "-", "black" )
    in
    Html.tr []
        (viewTeam match.home Home
            ++ [ Html.td [ textAlign Right ]
                    [ div [ style "color" scoreColor ] [ text homeScore ] ]
               , Html.td [ textAlign Center ]
                    [ div [ style "color" scoreColor ] [ text ":" ] ]
               , Html.td [ textAlign Left ]
                    [ div [ style "color" scoreColor ] [ text guestScore ] ]
               ]
            ++ viewTeam match.guest Guest
        )


viewMatchDay : MatchDay -> Html Msg
viewMatchDay matchDay =
    Html.table [] (List.map viewMatch matchDay)



-- JSON decoding


matchDayDecoder : Decoder MatchDay
matchDayDecoder =
    list
        (map4
            Match
            (field "Team1" teamDecoder)
            (field "Team2" teamDecoder)
            resultListDecoder
            (field "MatchIsFinished" bool)
        )


scoreFromInts : Int -> Int -> Score
scoreFromInts a b =
    Just ( a, b )


resultListDecoder : Decoder Score
resultListDecoder =
    field "MatchResults" (maybe (index 0 (field "ResultName" string)))
        |> andThen scoreDecoder


scoreDecoder : Maybe String -> Decoder Score
scoreDecoder maybe =
    case maybe of
        Just name ->
            if name == "Endergebnis" then
                map2 scoreFromInts
                    (teamScoreDecoder "PointsTeam1")
                    (teamScoreDecoder "PointsTeam2")

            else
                -- we got results, but not the final ones
                succeed Nothing

        -- no results yet
        Nothing ->
            succeed Nothing


teamScoreDecoder : String -> Decoder Int
teamScoreDecoder fieldName =
    field "MatchResults" (index 0 (field fieldName int))


teamDecoder : Decoder Team
teamDecoder =
    map2 Team teamNameDecoder (field "TeamIconUrl" string)


teamNameDecoder : Decoder String
teamNameDecoder =
    map shortenTeamName (field "ShortName" string)



-- HELPER


type Alignment
    = Left
    | Center
    | Right


textAlign : Alignment -> Html.Attribute msg
textAlign a =
    let
        partial =
            style "text-align"
    in
    case a of
        Left ->
            partial "left"

        Center ->
            partial "center"

        Right ->
            partial "right"
