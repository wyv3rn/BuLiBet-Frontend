module Main exposing (main)

-- TODO do not clutter global namespace ...

import Browser
import Color as C
import Dict exposing (Dict)
import Element exposing (Element)
import Element.Background as Background
import Element.Font as Font
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
        , map6
        , maybe
        , string
        , succeed
        )
import Regex



-- MAIN


main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


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
    { day : Int
    , home : Team
    , guest : Team
    , score : Score
    , bet : Score
    , finished : Bool
    }


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


type TeamResult
    = Win
    | Tie
    | Loss


type alias FullTeamResult =
    { teamName : String
    , day : Int
    , goals : Int
    , goalsAgainst : Int
    , result : TeamResult
    }


type alias ResultDict =
    Dict String (List FullTeamResult)


type alias TeamResultSummary =
    { teamName : String
    , matches : Int
    , wins : Int
    , ties : Int
    , losses : Int
    , goals : Int
    , goalsAgainst : Int
    , points : Int
    }


emptyTeamResultSummary : String -> TeamResultSummary
emptyTeamResultSummary teamName =
    TeamResultSummary teamName 0 0 0 0 0 0 0


type alias Table =
    List TeamResultSummary


generateTable : ResultDict -> Table
generateTable dict =
    let
        unsorted =
            Dict.foldl foldTable [] dict
    in
    -- TODO there are more rules for sorting
    List.sortBy (\summary -> -1 * summary.points) unsorted


foldTable : String -> List FullTeamResult -> Table -> Table
foldTable teamName results table =
    let
        teamSummary =
            List.foldl foldTeamResults (emptyTeamResultSummary teamName) results
    in
    teamSummary :: table


foldTeamResults : FullTeamResult -> TeamResultSummary -> TeamResultSummary
foldTeamResults result summary =
    let
        ( p, ( w, t, l ) ) =
            case result.result of
                Win ->
                    ( 3, ( 1, 0, 0 ) )

                Tie ->
                    ( 1, ( 0, 1, 0 ) )

                Loss ->
                    ( 0, ( 0, 0, 1 ) )
    in
    { summary
        | matches = summary.matches + 1
        , goals = summary.goals + result.goals
        , goalsAgainst = summary.goalsAgainst + result.goalsAgainst
        , points = summary.points + p
        , wins = summary.wins + w
        , ties = summary.ties + t
        , losses = summary.losses + l
    }


extractMatchDay : Int -> List Match -> MatchDay
extractMatchDay d matches =
    List.filter (\m -> m.day == d) matches


type DaySelection
    = Bet
    | Submit


type Selection
    = Day ( Int, MatchDay, DaySelection )
    | Table


type FetchStatus
    = Fetching
    | FetchFailed String
    | Fetched (List Match)


type alias Model =
    { selection : Maybe Selection
    , status : FetchStatus
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model Nothing Fetching
    , Http.get
        { url =
            openligaUrl
        , expect = Http.expectJson ReceivedMatches (list matchDecoder)
        }
    )



-- UPDATE


type Msg
    = DaySelected Int
    | TableSelected
    | ReceivedMatches (Result Http.Error (List Match))
    | MatchUp Int
    | UpdateBet Int TeamRole Int
    | SubmitBet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DaySelected day ->
            selectDay model day

        TableSelected ->
            selectTable model

        ReceivedMatches result ->
            case result of
                Ok matches ->
                    ( { model | status = Fetched matches }, Cmd.none )

                Err e ->
                    ( Model Nothing (FetchFailed (fetchErrorMsg e)), Cmd.none )

        MatchUp idx ->
            case model.selection of
                Just (Day ( d, matchDay, ds )) ->
                    ( { model
                        | selection =
                            Just
                                (Day
                                    ( d
                                    , moveMatchUp idx matchDay
                                    , ds
                                    )
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        UpdateBet matchIdx role inc ->
            case model.selection of
                Just (Day ( d, matchDay, ds )) ->
                    ( { model
                        | selection =
                            Just
                                (Day
                                    ( d
                                    , updateBet matchIdx role inc matchDay
                                    , ds
                                    )
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )

        SubmitBet ->
            case model.selection of
                Just (Day ( d, matchDay, _ )) ->
                    ( { model
                        | selection =
                            Just
                                (Day
                                    ( d
                                    , matchDay
                                    , Submit
                                    )
                                )
                      }
                    , Cmd.none
                    )

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
selectDay model d =
    case model.status of
        Fetched matches ->
            -- TODO allow switching from submit back to betting without loosing bets
            ( { model
                | selection =
                    Just
                        (Day
                            ( d
                            , extractMatchDay d matches
                            , Bet
                            )
                        )
              }
            , Cmd.none
            )

        _ ->
            ( model, Cmd.none )


selectTable model =
    ( { model | selection = Just Table }
    , Cmd.none
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        body =
            Element.html (viewHtml model)

        site =
            Element.column [ Element.width Element.fill ] [ viewMenu, body ]
    in
    Browser.Document "Bundesliga!" [ Element.layout [] site ]


viewMenu : Element Msg
viewMenu =
    Element.el
        [ Background.color C.dark
        , Font.color C.lighterGrey
        , Element.padding 10
        , Element.width Element.fill
        ]
        (Element.text "Your menu could be here")


viewHtml : Model -> Html Msg
viewHtml model =
    let
        ( content, allMatches ) =
            case model.status of
                Fetching ->
                    ( text "Fetching all matches for you ...", Nothing )

                FetchFailed reason ->
                    ( div [ style "color" "red" ]
                        [ text
                            ("Fetching failed, reason: "
                                ++ reason
                            )
                        ]
                    , Nothing
                    )

                Fetched matches ->
                    case model.selection of
                        Nothing ->
                            ( text "Select match day to bet or other actions below"
                            , Just matches
                            )

                        Just (Day ( _, matchDay, ds )) ->
                            case ds of
                                Bet ->
                                    ( viewMatchDay matchDay, Just matches )

                                Submit ->
                                    ( viewSubmittedMatchDay matchDay
                                    , Just matches
                                    )

                        Just Table ->
                            ( viewTable matches, Nothing )

        buttons =
            viewButtons model.selection

        upper =
            [ Html.p [] [ content ]
            , Html.hr [] []
            , Html.p [] [ viewButtons model.selection ]
            , Html.hr [] []
            ]

        elements =
            case allMatches of
                Nothing ->
                    upper

                Just ms ->
                    let
                        fms =
                            -- TODO dynamic filtering (e.g. last 5 matchdays)
                            List.filter (\m -> m.day >= 1) ms
                    in
                    upper
                        ++ [ Html.table []
                                [ Html.tr []
                                    [ Html.td [] [ viewTable ms ]
                                    , Html.td [] [ text "     " ]
                                    , Html.td [] [ viewTable fms ]
                                    ]
                                ]
                           ]
    in
    Html.div [] elements


viewButtons : Maybe Selection -> Html Msg
viewButtons selection =
    let
        dayLabels =
            List.map String.fromInt (List.range 1 34)

        menuLabels =
            dayLabels ++ [ "Table" ]

        buttons =
            List.map (createButton selection) menuLabels
    in
    Html.div [] buttons


createButton : Maybe Selection -> String -> Html Msg
createButton selection label =
    let
        selectedBg =
            "#268bd2"

        bg =
            case selection of
                Nothing ->
                    "white"

                Just (Day ( d, _, _ )) ->
                    if label == String.fromInt d then
                        selectedBg

                    else
                        "white"

                Just Table ->
                    if label == "Table" then
                        selectedBg

                    else
                        "white"

        action =
            if label == "Table" then
                onClick TableSelected

            else
                onClick (DaySelected (Maybe.withDefault -1 (String.toInt label)))
    in
    Html.button [ style "background-color" bg, action ]
        [ text label ]


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


viewTable : List Match -> Html Msg
viewTable matches =
    let
        resultDict =
            List.foldl insertMatchToResultDict Dict.empty matches

        table =
            generateTable resultDict
    in
    Html.div
        []
        [ Html.table [] (List.indexedMap viewTableRow table)
        ]


viewTableRow : Int -> TeamResultSummary -> Html Msg
viewTableRow idx summary =
    let
        ratio =
            toFloat summary.goals / toFloat summary.goalsAgainst

        ratioNorm =
            if ratio >= 1 then
                ratio

            else
                1 / ratio

        ratioTrunc =
            toFloat (truncate (ratioNorm * 100)) / 100.0

        ratioStr =
            String.fromFloat ratioTrunc

        ratioPadded =
            String.padRight 4
                '0'
                (if String.contains "." ratioStr then
                    ratioStr

                 else
                    ratioStr ++ "."
                )

        ratioTxt =
            if ratio >= 1 then
                ratioPadded ++ ":1.00"

            else
                "1.00:" ++ ratioPadded
    in
    Html.tr []
        [ Html.td [] [ text (String.fromInt (idx + 1)) ]
        , Html.td [] [ text summary.teamName ]
        , Html.td [] [ text (String.fromInt summary.points) ]
        , Html.td [] [ text (String.fromInt summary.wins) ]
        , Html.td [] [ text (String.fromInt summary.ties) ]
        , Html.td [] [ text (String.fromInt summary.losses) ]
        , Html.td [] [ text (String.fromInt summary.goals ++ ":" ++ String.fromInt summary.goalsAgainst) ]
        , Html.td [] [ text ratioTxt ]
        , Html.td [] [ text ("(" ++ String.fromInt summary.matches ++ ")") ]
        ]


insertMatchToResultDict : Match -> ResultDict -> ResultDict
insertMatchToResultDict match dict =
    let
        dictWithHome =
            insertMatchToResultDictRole Home match dict
    in
    insertMatchToResultDictRole Guest match dictWithHome


insertMatchToResultDictRole : TeamRole -> Match -> ResultDict -> ResultDict
insertMatchToResultDictRole role match dict =
    case match.score of
        Nothing ->
            dict

        Just ( hg, gg ) ->
            let
                ( partial, goals, goalsAgainst ) =
                    case role of
                        Home ->
                            ( FullTeamResult match.home.name match.day, hg, gg )

                        Guest ->
                            ( FullTeamResult match.guest.name match.day, gg, hg )

                result =
                    if goals > goalsAgainst then
                        partial goals goalsAgainst Win

                    else if goals == goalsAgainst then
                        partial goals goalsAgainst Tie

                    else
                        partial goals goalsAgainst Loss

                -- TODO better "insertWith"/"update"?
                before =
                    Dict.get result.teamName dict

                ( tmpDict, toInsert ) =
                    case before of
                        Nothing ->
                            ( dict, [ result ] )

                        Just resList ->
                            ( Dict.remove result.teamName dict, result :: resList )
            in
            Dict.insert result.teamName toInsert tmpDict



-- JSON decoding


matchDecoder : Decoder Match
matchDecoder =
    map6
        Match
        (field "group" (field "groupOrderID" int))
        (field "team1" teamDecoder)
        (field "team2" teamDecoder)
        resultListDecoder
        (succeed Nothing)
        (field "matchIsFinished" bool)


scoreFromInts : Int -> Int -> Score
scoreFromInts a b =
    Just ( a, b )


resultListDecoder : Decoder Score
resultListDecoder =
    field "matchResults" (maybe (index 1 (field "resultName" string)))
        |> andThen scoreDecoder


scoreDecoder : Maybe String -> Decoder Score
scoreDecoder maybe =
    case maybe of
        Just name ->
            if name == "Endergebnis" then
                map2 scoreFromInts
                    (teamScoreDecoder "pointsTeam1")
                    (teamScoreDecoder "pointsTeam2")

            else
                -- we got results, but not the final ones
                succeed Nothing

        -- no results yet
        Nothing ->
            succeed Nothing


teamScoreDecoder : String -> Decoder Int
teamScoreDecoder fieldName =
    field "matchResults" (index 1 (field fieldName int))


teamDecoder : Decoder Team
teamDecoder =
    map2 Team teamNameDecoder (field "teamIconUrl" string)


teamNameDecoder : Decoder String
teamNameDecoder =
    field "shortName" string



-- HELPER


openligaUrl =
    "https://api.openligadb.de/getmatchdata/bl1/2025"


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
