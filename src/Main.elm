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
        , map5
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


updateScore : Int -> TeamRole -> Score -> Score
updateScore inc role score =
    case score of
        Just ( h, g ) ->
            case role of
                Home ->
                    Just ( max (h + inc) 0, g )

                Guest ->
                    Just ( h, max (g + inc) 0 )

        Nothing ->
            let
                val =
                    max inc 0
            in
            case role of
                Home ->
                    Just ( val, 0 )

                Guest ->
                    Just ( 0, val )


type alias Match =
    { home : Team, guest : Team, score : Score, bet : Score, finished : Bool }


type alias MatchDay =
    List Match


moveMatchUp : Int -> MatchDay -> MatchDay
moveMatchUp idx matchDay =
    matchDay
        |> List.indexedMap Tuple.pair
        |> List.sortBy
            (\( i, e ) ->
                if i == idx then
                    toFloat i - 1.1

                else
                    toFloat i
            )
        |> List.unzip
        |> Tuple.second


updateBet : Int -> TeamRole -> Int -> MatchDay -> MatchDay
updateBet matchIdx role inc matchDay =
    matchDay
        |> List.indexedMap
            (\i match ->
                if i == matchIdx then
                    { match | bet = updateScore inc role match.bet }

                else
                    match
            )


type FetchStatus
    = Idle
    | Fetching Int
    | FetchFailed String
    | FetchDone MatchDay
    | Submitted MatchDay


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
    | MatchUp Int
    | UpdateBet Int TeamRole Int
    | SubmitBet


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

        MatchUp idx ->
            case model.status of
                FetchDone matchDay ->
                    ( { model | status = FetchDone (moveMatchUp idx matchDay) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateBet matchIdx role inc ->
            case model.status of
                FetchDone matchDay ->
                    ( { model
                        | status =
                            FetchDone (updateBet matchIdx role inc matchDay)
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitBet ->
            case model.status of
                FetchDone matchDay ->
                    ( { model | status = Submitted matchDay }, Cmd.none )

                _ ->
                    ( model, Cmd.none )


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

        Submitted matchDay ->
            viewSiteDay (viewSubmittedMatchDay matchDay)


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


viewMatch : Int -> Match -> Html Msg
viewMatch idx match =
    let
        scoreColor =
            case match.score of
                Just _ ->
                    if match.finished then
                        "black"

                    else
                        "red"

                Nothing ->
                    "black"
    in
    Html.tr []
        ([ viewUpBtn idx ]
            ++ viewTeam match.home Home
            ++ [ Html.td [] [ viewScore scoreColor match.score ] ]
            ++ viewTeam match.guest Guest
            ++ viewBet idx match.bet
        )


viewUpBtn : Int -> Html Msg
viewUpBtn matchIdx =
    Html.td
        [ textAlign Center ]
        [ button [ onClick (MatchUp matchIdx) ] [ text "⇧" ] ]


viewScore : String -> Score -> Html Msg
viewScore =
    viewScoreDelim "\u{2009}"


viewScoreDelim : String -> String -> Score -> Html Msg
viewScoreDelim delim color score =
    let
        ( homeScore, guestScore ) =
            case score of
                Just ( a, b ) ->
                    ( String.fromInt a
                    , String.fromInt b
                    )

                Nothing ->
                    ( "-", "-" )
    in
    Html.span
        [ style "color" color ]
        [ text (homeScore ++ delim ++ ":" ++ delim ++ guestScore) ]


viewBet : Int -> Score -> List (Html Msg)
viewBet matchIdx bet =
    [ viewBetBtn matchIdx Home 2
    , viewBetBtn matchIdx Home 1
    , viewBetBtn matchIdx Home -1
    ]
        ++ [ Html.td [] [ viewScore "black" bet ] ]
        ++ [ viewBetBtn matchIdx Guest -1
           , viewBetBtn matchIdx Guest 1
           , viewBetBtn matchIdx Guest 2
           ]


viewBetBtn : Int -> TeamRole -> Int -> Html Msg
viewBetBtn matchIdx role inc =
    let
        sign =
            if inc >= 0 then
                "+"

            else
                ""
    in
    Html.td
        [ textAlign Center ]
        [ button
            [ onClick (UpdateBet matchIdx role inc)
            ]
            [ text (sign ++ String.fromInt inc) ]
        ]


viewMatchDay : MatchDay -> Html Msg
viewMatchDay matchDay =
    Html.div
        []
        [ Html.table [] (List.indexedMap viewMatch matchDay)
        , Html.button [ onClick SubmitBet ] [ text "Submit!" ]
        ]


viewSubmittedMatchDay : MatchDay -> Html Msg
viewSubmittedMatchDay matchDay =
    Html.table [] (List.map viewSubmittedMatch matchDay)


viewSubmittedMatch : Match -> Html Msg
viewSubmittedMatch match =
    Html.tr []
        [ Html.div []
            ([ text (match.home.name ++ " ") ]
                ++ [ viewScoreDelim "" "black" match.bet ]
                ++ [ text (" " ++ match.guest.name) ]
            )
        ]



-- JSON decoding


matchDayDecoder : Decoder MatchDay
matchDayDecoder =
    list
        (map5
            Match
            (field "Team1" teamDecoder)
            (field "Team2" teamDecoder)
            resultListDecoder
            (succeed Nothing)
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
