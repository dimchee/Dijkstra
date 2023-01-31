module Main exposing (..)

import Browser
import Browser.Dom
import Editor
import Eval
import Html exposing (Html)
import Lang
import Task


main : Program () Editor.Model Editor.Msg
main =
    Browser.document
        { init =
            \() ->
                ( Editor.init
                , Browser.Dom.focus "editor"
                    |> Task.attempt (always Editor.NoOp)
                )
        , update = Editor.update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


viewExecutionResultDebug : Editor.Model -> Html Editor.Msg
viewExecutionResultDebug model =
    Editor.getText model
        |> Lang.parse
        |> Result.map (\statement -> Lang.context [] |> Eval.eval statement)
        |> Debug.toString
        |> Html.text


viewContext : Maybe Lang.Context -> Html Editor.Msg
viewContext m = case m of
    Just context ->
        Html.div []
            [ Html.text <| Debug.toString <| context
            ]
    Nothing -> Html.text "ERROR"

viewExecutionResult : Editor.Model -> Html Editor.Msg
viewExecutionResult model =
    Editor.getText model
        |> Lang.parse
        |> Result.map (\statement -> Lang.context [] |> Eval.eval statement)
        |> Result.withDefault Nothing
        |> viewContext


view : Editor.Model -> Browser.Document Editor.Msg
view model =
    { title = "Dijkstra's Legacy"
    , body =
        [ Editor.view model
        , viewExecutionResult model

        --, Editor.viewDebug model
        ]
    }
