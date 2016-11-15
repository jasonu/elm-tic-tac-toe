module Main exposing (..)

import Debug exposing (log)
import List exposing (filter, map, foldr)
import Html exposing (Html, div, text, button, table, tr, td)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)


main : Program Never Model Msg
main =
    Html.beginnerProgram
        { model = init
        , update = update
        , view = view
        }



-- Types


type Player
    = P1
    | P2


type Mark
    = NoMark
    | X
    | O


type alias Square =
    { id : Int
    , mark : Mark
    }


type alias Model =
    { current : Player
    , board : List Square
    , finished : Bool
    , winner : Mark
    }


type Msg
    = NewGame
    | Click Int
    | Noop



-- Convenience functions for switching between types


playerToMark : Player -> Mark
playerToMark player =
    case player of
        P1 ->
            X

        P2 ->
            O


markToString : Mark -> String
markToString mark =
    case mark of
        NoMark ->
            ""

        X ->
            "X"

        O ->
            "O"



-- Init


initBoard : List Square
initBoard =
    [ Square 0 NoMark
    , Square 1 NoMark
    , Square 2 NoMark
    , Square 3 NoMark
    , Square 4 NoMark
    , Square 5 NoMark
    , Square 6 NoMark
    , Square 7 NoMark
    , Square 8 NoMark
    ]


init : Model
init =
    Model P1 initBoard False NoMark



-- Update


togglePlayer : Player -> Player
togglePlayer player =
    case player of
        P1 ->
            P2

        P2 ->
            P1


update : Msg -> Model -> Model
update msg model =
    case Debug.log "msg" msg of
        NewGame ->
            init

        Noop ->
            model

        Click id ->
            if model.finished then
                model
            else
                let
                    newBoard =
                        updateBoard id model

                    ( status, winner ) =
                        gameOver newBoard
                in
                    { model
                        | current = togglePlayer model.current
                        , finished = status
                        , winner = winner
                        , board = newBoard
                    }


updateBoard : Int -> Model -> List Square
updateBoard id model =
    let
        newSquare =
            Square id (playerToMark model.current)
    in
        if (id == 0) then
            newSquare :: (List.drop 1 model.board)
        else if (id == 8) then
            (List.take 8 model.board) ++ [ newSquare ]
        else
            (List.take id model.board)
                ++ [ newSquare ]
                ++ (List.drop (id + 1) model.board)


gameOver : List Square -> ( Bool, Mark )
gameOver board =
    let
        x =
            xWins board

        o =
            oWins board

        cat =
            fullBoard board

        winner =
            if x then
                X
            else if o then
                O
            else
                NoMark
    in
        ( x || o || cat, winner )


fullBoard : List Square -> Bool
fullBoard board =
    board
        |> List.map (\square -> square.mark /= NoMark)
        |> foldr (&&) True


xWins : List Square -> Bool
xWins =
    markWins X


oWins : List Square -> Bool
oWins =
    markWins O


markWins : Mark -> List Square -> Bool
markWins mark board =
    let
        markPredicate =
            case mark of
                NoMark ->
                    (\square -> square.mark == NoMark)

                X ->
                    (\square -> square.mark == X)

                O ->
                    (\square -> square.mark == O)

        r1 =
            board
                |> filter (\square -> square.id < 3)
                |> List.map markPredicate
                |> foldr (&&) True

        r2 =
            board
                |> filter (\square -> square.id > 2 && square.id < 6)
                |> List.map markPredicate
                |> foldr (&&) True

        r3 =
            board
                |> filter (\square -> square.id > 5)
                |> List.map markPredicate
                |> foldr (&&) True

        c1 =
            board
                |> filter (\square -> (square.id % 3) == 0)
                |> List.map markPredicate
                |> foldr (&&) True

        c2 =
            board
                |> filter (\square -> (square.id % 3) == 1)
                |> List.map markPredicate
                |> foldr (&&) True

        c3 =
            board
                |> filter (\square -> (square.id % 3) == 2)
                |> List.map markPredicate
                |> foldr (&&) True

        d1 =
            board
                |> filter
                    (\square ->
                        (square.id == 0) || (square.id == 4) || (square.id == 8)
                    )
                |> List.map markPredicate
                |> foldr (&&) True

        d2 =
            board
                |> filter
                    (\square ->
                        (square.id == 2) || (square.id == 4) || (square.id == 6)
                    )
                |> List.map markPredicate
                |> foldr (&&) True
    in
        r1 || r2 || r3 || c1 || c2 || c3 || d1 || d2



-- View


view : Model -> Html Msg
view model =
    div []
        [ viewBoard model
        , viewGameStatus model
        ]


viewSquare : Square -> Html Msg
viewSquare square =
    let
        eventHandler =
            if (square.mark == NoMark) then
                onClick (Click square.id)
            else
                onClick Noop
    in
        td (List.append [ style squareStyle ] [ eventHandler ])
            [ text (markToString square.mark) ]


viewBoard : Model -> Html Msg
viewBoard model =
    table
        []
        [ tr []
            (model.board
                |> List.filter (\square -> square.id < 3)
                |> List.map viewSquare
            )
        , tr []
            (model.board
                |> List.filter (\square -> square.id > 2 && square.id < 6)
                |> List.map viewSquare
            )
        , tr []
            (model.board
                |> List.filter (\square -> square.id > 5)
                |> List.map viewSquare
            )
        ]


viewGameStatus : Model -> Html Msg
viewGameStatus model =
    if model.finished then
        if model.winner == NoMark then
            div []
                [ div [] [ text "Tie Game" ]
                , button [ onClick NewGame ] [ text "New Game" ]
                ]
        else
            div []
                [ div [] [ text ((markToString model.winner) ++ " Wins!") ]
                , button [ onClick NewGame ] [ text "New Game" ]
                ]
    else
        div []
            [ text
                ((markToString (playerToMark model.current) ++ "'s turn"))
            ]


squareStyle : List ( String, String )
squareStyle =
    [ ( "border", "1px solid black" )
    , ( "width", "32px" )
    , ( "height", "32px" )
    , ( "text-align", "center" )
    , ( "padding", "2px" )
    ]
