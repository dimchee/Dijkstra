module Main exposing (..)

import Browser
import Browser.Dom
import Dict exposing (Dict)
import Editor
import Eval
import Html exposing (Html)
import Lang
import Parser
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


viewState : Lang.State -> Html Editor.Msg
viewState state =
    state |> Dict.toList |> List.map (\ass -> Html.li [] [ Html.text <| Debug.toString ass ]) |> Html.ul []


viewContext : Lang.Context -> Html Editor.Msg
viewContext context =
    case context of
        Lang.Active state ->
            viewState state

        Lang.Halt ->
            Html.text "Halted"


viewParsingError : Parser.DeadEnd -> Html Editor.Msg
viewParsingError err =
    Html.li [] [ Html.text <| Debug.toString err ]


viewParsingErrors : List Parser.DeadEnd -> Html Editor.Msg
viewParsingErrors =
    List.map viewParsingError >> Html.ul []


unwrapResult : Result a a -> a
unwrapResult res =
    case res of
        Result.Ok x ->
            x

        Result.Err x ->
            x


viewExecutionResult : Editor.Model -> Html Editor.Msg
viewExecutionResult model =
    Editor.getText model
        |> Lang.parse
        |> Result.map (\statement -> Lang.context [] |> Eval.eval statement)
        |> Result.mapError viewParsingErrors
        |> Result.map viewContext
        |> unwrapResult


view : Editor.Model -> Browser.Document Editor.Msg
view model =
    { title = "Dijkstra's Legacy"
    , body =
        [ Editor.view model
        , viewExecutionResult model

        --, Editor.viewDebug model
        ]
    }
