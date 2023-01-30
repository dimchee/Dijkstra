module Main exposing (..)

import Browser
import Dict
import Eval
import Html exposing (button, div, input, text)
import Html.Attributes exposing (attribute)
import Html.Events exposing (onClick, onInput)
import Lang
import Parser


main : Program () String Msg
main =
    Browser.sandbox
        { init = ""
        , update = update
        , view = view
        }


type Msg
    = Show (Result (List Parser.DeadEnd) String)


update : Msg -> a -> String
update msg _ =
    case msg of
        Show expr ->
            Debug.toString expr


view : String -> Html.Html Msg
view model =
    div []
        [ input
            [ onInput
                (Show
                    << Result.map (\expr -> Debug.toString expr)
                    -- << Result.toMaybe
                    << Parser.run Lang.expression
                )
            ]
            []
        , input
            [ onInput
                (Show
                    << Result.map (\statement -> Debug.toString <| Eval.eval statement Dict.empty)
                    -- << Result.toMaybe
                    << Parser.run Lang.statement
                )
            ]
            []
        , div [] []
        , text model
        ]
