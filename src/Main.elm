module Main exposing (..)

import Browser
import Dict
import Eval
import Html exposing (button, div, input, text)
import Html.Events exposing (onClick, onInput)
import Lang


main : Program () String Msg
main =
    Browser.sandbox
        { init = ""
        , update = update
        , view = view
        }


type Msg
    = Show (Maybe String)


update : Msg -> a -> String
update msg _ =
    case msg of
        Show expr ->
            Maybe.withDefault "error" expr


view : String -> Html.Html Msg
view model =
    div []
        [ input
            [ onInput
                (Show
                    << Maybe.map (\statement -> Debug.toString <| Eval.eval statement Dict.empty)
                    << Result.toMaybe
                    << Lang.parse
                )
            ]
            []
        , div [] []
        , text model
        ]
