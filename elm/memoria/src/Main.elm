module Main exposing (main)

{-| Ponto de entrada: liga as regras de `Game` à tela e à API, seguindo a
Arquitetura Elm (modelo, mensagens, `update` e `view`).
-}

import Api exposing (CardInfo, Theme)
import Browser
import Game exposing (Card, Face(..), Game, Turn(..))
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onCheck, onClick, onInput)
import Http
import Process
import Rng exposing (Seed)
import Task


{-| O JavaScript passa um número aleatório como semente (veja `src/main.js`).
-}
type alias Flags =
    Int


main : Program Flags Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }



-- MODELO


type alias Model =
    { seed : Seed
    , state : State
    , themes : List Theme
    , selectedTheme : String
    , equalPairs : Bool
    , moves : Int
    }


type State
    = Loading
    | Failed Http.Error
    | Playing Game


init : Flags -> ( Model, Cmd Msg )
init flags =
    let
        defaultTheme =
            "animals"

        defaultEqualPairs =
            False
    in
    ( { seed = Rng.seed flags
      , state = Loading
      , themes =
            [ { id = "animals", name = "Animais" }
            , { id = "simpsons", name = "Os Simpsons" }
            , { id = "pokemon", name = "Pokémon" }
            ]
      , selectedTheme = defaultTheme
      , equalPairs = defaultEqualPairs
      , moves = 0
      }
    , Cmd.batch
        [ Api.getThemes GotThemes
        , Api.getCards defaultTheme defaultEqualPairs GotCards
        ]
    )



-- ATUALIZAÇÃO


type Msg
    = CardClicked Int
    | MismatchTimeout Turn
    | NewGameClicked
    | GotCards (Result Http.Error (List CardInfo))
    | GotThemes (Result Http.Error (List Theme))
    | ThemeSelected String
    | EqualPairsToggled Bool


{-| Tempo, em milissegundos, que as duas cartas erradas ficam à mostra antes de desvirar.
-}
mismatchDelay : Float
mismatchDelay =
    1000


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotCards (Ok infos) ->
            let
                ( game, nextSeed ) =
                    Game.new model.seed infos
            in
            ( { model | seed = nextSeed, state = Playing game }, Cmd.none )

        GotCards (Err err) ->
            ( { model | state = Failed err }, Cmd.none )

        GotThemes (Ok themes) ->
            ( { model | themes = themes }, Cmd.none )

        GotThemes (Err _) ->
            ( model, Cmd.none )

        ThemeSelected themeId ->
            ( { model | selectedTheme = themeId, state = Loading, moves = 0 }
            , Api.getCards themeId model.equalPairs GotCards
            )

        EqualPairsToggled isChecked ->
            ( { model | equalPairs = isChecked, state = Loading, moves = 0 }
            , Api.getCards model.selectedTheme isChecked GotCards
            )

        CardClicked id ->
            case model.state of
                Playing game ->
                    let
                        newGame =
                            Game.reveal id game

                        cmd =
                            case newGame.turn of
                                TwoRevealed _ _ ->
                                    Process.sleep mismatchDelay
                                        |> Task.perform (\_ -> MismatchTimeout newGame.turn)

                                _ ->
                                    Cmd.none

                        newMoves =
                            case ( game.turn, newGame.turn ) of
                                ( OneRevealed _, TwoRevealed _ _ ) ->
                                    model.moves + 1

                                ( OneRevealed _, Waiting ) ->
                                    model.moves + 1

                                _ ->
                                    model.moves
                    in
                    ( { model | state = Playing newGame, moves = newMoves }, cmd )

                _ ->
                    ( model, Cmd.none )

        MismatchTimeout expectedTurn ->
            case model.state of
                Playing game ->
                    if game.turn == expectedTurn then
                        ( { model | state = Playing (Game.hideMismatch game) }, Cmd.none )

                    else
                        ( model, Cmd.none )

                _ ->
                    ( model, Cmd.none )

        NewGameClicked ->
            ( { model | state = Loading, moves = 0 }
            , Api.getCards model.selectedTheme model.equalPairs GotCards
            )



-- VISUALIZAÇÃO


view : Model -> Html Msg
view model =
    main_
        [ class "table" ]
        [ viewToolbar model
        , case model.state of
            Loading ->
                p [ class "status" ] [ text "Carregando as cartas..." ]

            Failed err ->
                p [ class "status status--error" ] [ text (errorToString err) ]

            Playing game ->
                div []
                    [ ol
                        [ class "board"
                        , attribute "aria-label" "Tabuleiro"
                        ]
                        (game.cards |> List.map (viewCard game.turn))
                    , if Game.isWon game then
                        viewVictory model.moves

                      else
                        text ""
                    ]
        , viewNewGameButton
        ]


viewToolbar : Model -> Html Msg
viewToolbar model =
    div
        [ class "toolbar" ]
        [ label []
            [ text "Tema "
            , select [ onInput ThemeSelected ]
                (model.themes
                    |> List.map
                        (\t ->
                            option
                                [ value t.id
                                , selected (t.id == model.selectedTheme)
                                ]
                                [ text t.name ]
                        )
                )
            ]
        , label []
            [ input
                [ type_ "checkbox"
                , checked model.equalPairs
                , onCheck EqualPairsToggled
                ]
                []
            , text " Pares iguais"
            ]
        ]


viewNewGameButton : Html Msg
viewNewGameButton =
    button
        [ class "fab"
        , type_ "button"
        , attribute "aria-label" "Novo jogo"
        , title "Novo jogo"
        , onClick NewGameClicked
        ]
        []


viewCard : Turn -> Card -> Html Msg
viewCard turn card =
    let
        cardClasses =
            case card.face of
                Up ->
                    if isMissed turn card then
                        "card card--up card--miss"

                    else
                        "card card--up"

                Down ->
                    "card"

                Permanent ->
                    "card card--up card--matched"
    in
    li
        [ class "board__slot"
        ]
        [ button
            [ type_ "button"
            , class cardClasses
            , onClick (CardClicked card.id)
            ]
            [ span
                [ class "card__inner"
                ]
                [ span
                    [ class "card__back"
                    ]
                    []
                , span
                    [ class "card__front"
                    ]
                    [ img
                        [ class "card__image"
                        , src card.url
                        , alt card.description
                        ]
                        []
                    ]
                ]
            ]
        ]


isMissed : Turn -> Card -> Bool
isMissed turn card =
    case turn of
        TwoRevealed ( _, idA ) ( _, idB ) ->
            card.id == idA || card.id == idB

        _ ->
            False


viewVictory : Int -> Html Msg
viewVictory moves =
    div
        [ class "overlay" ]
        [ node "dialog"
            [ class "victory"
            , attribute "open" ""
            , attribute "aria-labelledby" "victory-title"
            ]
            [ h2
                [ id "victory-title"
                , class "victory__title"
                ]
                [ text "Parabéns!" ]
            , p
                [ class "victory__text"
                ]
                [ text (String.fromInt moves ++ " jogadas") ]
            , button
                [ class "button button--primary"
                , type_ "button"
                , autofocus True
                , onClick NewGameClicked
                ]
                [ text "Jogar de novo" ]
            ]
        ]


errorToString : Http.Error -> String
errorToString err =
    case err of
        Http.BadUrl url ->
            "URL inválida: " ++ url

        Http.Timeout ->
            "O servidor demorou demais para responder."

        Http.NetworkError ->
            "Erro de conexão com o servidor."

        Http.BadStatus 404 ->
            "Tema não encontrado (404)."

        Http.BadStatus status ->
            "Erro no servidor: status " ++ String.fromInt status

        Http.BadBody message ->
            "Resposta inesperada do servidor: " ++ message
