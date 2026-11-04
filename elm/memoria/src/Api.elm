module Api exposing (CardInfo, Theme, cardDecoder, charDecoder, getCards, getThemes, themeDecoder)

{-| Tipos, decoders e requisições da API do jogo da memória.
-}

import Http
import Json.Decode as D exposing (Decoder)
import Url.Builder


{-| Informações de um tema disponível na API.
-}
type alias Theme =
    { id : String
    , name : String
    }


{-| Decodifica um tema da lista retornada por /api/themes.
-}
themeDecoder : Decoder Theme
themeDecoder =
    D.map2 Theme
        (D.field "id" D.string)
        (D.field "name" D.string)


{-| Busca a lista de temas disponíveis na API.
-}
getThemes : (Result Http.Error (List Theme) -> msg) -> Cmd msg
getThemes toMsg =
    Http.get
        { url = Url.Builder.absolute [ "api", "themes" ] []
        , expect = Http.expectJson toMsg (D.list themeDecoder)
        }


{-| Uma carta como vem da API, antes de entrar no jogo.
-}
type alias CardInfo =
    { group : Char
    , description : String
    , url : String
    }


{-| Decodifica o objeto JSON de uma carta individual.
-}
cardDecoder : Decoder CardInfo
cardDecoder =
    D.map3 CardInfo
        (D.field "group" charDecoder)
        (D.field "description" D.string)
        (D.field "url" D.string)


{-| Decodifica uma string contendo exatamente um caractere como Char.
-}
charDecoder : Decoder Char
charDecoder =
    D.string
        |> D.andThen
            (\s ->
                case String.toList s of
                    [ c ] ->
                        D.succeed c

                    _ ->
                        D.fail ("esperava uma letra, veio " ++ s)
            )


{-| Faz a requisição HTTP GET para /api/cards com tema e paridade.
-}
getCards : String -> Bool -> (Result Http.Error (List CardInfo) -> msg) -> Cmd msg
getCards theme equalPairs toMsg =
    Http.get
        { url =
            Url.Builder.absolute [ "api", "cards" ]
                [ Url.Builder.string "theme" theme
                , Url.Builder.string "equalPairs" (boolToString equalPairs)
                ]
        , expect = Http.expectJson toMsg (D.list cardDecoder)
        }


boolToString : Bool -> String
boolToString bool =
    if bool then
        "true"

    else
        "false"
