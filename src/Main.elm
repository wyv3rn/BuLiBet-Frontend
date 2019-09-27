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
    = Fetching Int
    | FetchFailed String


type
    Model
    -- first String is input, second is an error msg to display
    = QueryDay ( String, String )
    | FetchDay FetchStatus
    | ShowDay MatchDay


init : () -> ( Model, Cmd Msg )
init _ =
    ( QueryDay ( "", "" ), Cmd.none )



-- UPDATE


type Msg
    = DayInput String
    | SubmitDay String
    | GotMatchDay (Result Http.Error MatchDay)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        DayInput d ->
            ( QueryDay ( d, "" ), Cmd.none )

        SubmitDay inputStr ->
            updateSubmitDay inputStr

        GotMatchDay result ->
            case result of
                Ok matches ->
                    ( ShowDay matches, Cmd.none )

                Err e ->
                    ( FetchDay (FetchFailed (fetchErrorMsg e)), Cmd.none )


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


updateSubmitDay : String -> ( Model, Cmd Msg )
updateSubmitDay inputStr =
    case String.toInt inputStr of
        Just day ->
            if day < 1 || day > 34 then
                ( QueryDay ( inputStr, "There are only match days 1 to 34!" )
                , Cmd.none
                )

            else
                ( FetchDay (Fetching day)
                , Http.get
                    { url =
                        "https://www.openligadb.de/api/getmatchdata/bl1/2019/"
                            ++ String.fromInt day
                    , expect = Http.expectJson GotMatchDay matchDayDecoder
                    }
                )

        Nothing ->
            ( QueryDay ( inputStr, "Matchday has to be an integer!" )
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    case model of
        QueryDay ( str, error ) ->
            div []
                [ text "Which day are you interested in? "
                , input
                    [ type_ "text"
                    , placeholder ""
                    , value str
                    , onInput DayInput
                    ]
                    []
                , button [ onClick (SubmitDay str) ] [ text "Fetch!" ]
                , div [ style "color" "red" ] [ text error ]
                ]

        FetchDay status ->
            case status of
                Fetching day ->
                    text ("Fetching day " ++ String.fromInt day ++ " ...")

                FetchFailed reason ->
                    text
                        ("Fetching failed, reason: "
                            ++ reason
                        )

        ShowDay matchDay ->
            viewMatchDay matchDay


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
