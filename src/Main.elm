module Main exposing (..)

import Browser
import Browser.Dom
import Dict
import Editor
import Eval
import Html exposing (Html)
import Html.Attributes as HA
import Lang
import Parser exposing (Problem(..))
import Task



-- import Html.Events as HE


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
    state
        |> Dict.toList
        |> List.map (\( var, val ) -> Html.li [] [ Html.text <| var ++ " = " ++ String.fromInt val ])
        |> Html.ul []


fromContext : a -> (Lang.State -> a) -> Lang.Context -> a
fromContext onHalt onActive context =
    case context of
        Lang.Active state ->
            onActive state

        Lang.Halt ->
            onHalt


viewContext : Lang.Context -> Html Editor.Msg
viewContext context =
    Html.details
        [ HA.style "cursor" "pointer"
        , HA.style "box-shadow" "3px 3px 4px black"
        , HA.style "background-color" "#f0f0f0"
        , HA.style "margin" "0.5em"
        , HA.style "padding" "0.2em 1em"
        ]
        [ fromContext (Html.text "Halted") viewState context
        , Html.summary
            [ HA.style "border" "none"
            , HA.style "color" "#888"
            , HA.style "list-style" "none"
            , HA.style "width" "6em"
            ]
            [ Html.text <| "⚡State: "

            -- , Html.select [] []
            ]
        ]


problemToString : Parser.Problem -> String
problemToString problem =
    case problem of
        ExpectingNumber ->
            "Expecting number like `13`"

        ExpectingVariable ->
            "Expecting variable like `x`"

        ExpectingSymbol symbol ->
            "Expecting symbol `" ++ symbol ++ "`"

        ExpectingKeyword keyword ->
            "Expecting keyword `" ++ keyword ++ "`"

        ExpectingEnd ->
            "Expecting End. Did you forget to add `;`?"

        UnexpectedChar ->
            "Character not recognised"

        Problem msg ->
            msg

        err ->
            "Unknown error: " ++ Debug.toString err


viewParsingError : Parser.DeadEnd -> Html Editor.Msg
viewParsingError { col, problem, row } =
    Html.li
        [-- [ HE.onClick <| Editor.GoToPosition { line = row, column = col }
         -- , HE.onMouseOver <| Editor.Hover <| Editor.HoverChar { line = 0, column = 3 }
        ]
        [ Html.text <|
            -- Debug.toString problem
            problemToString problem
        , Html.mark
            [ HA.style "color" "#888"
            , HA.style "background" "none"
            , HA.style "padding" "1em"
            ]
            [ Html.text <|
                "["
                    ++ String.fromInt col
                    ++ ","
                    ++ String.fromInt row
                    ++ "]"
            ]
        ]


viewParsingErrors : List Parser.DeadEnd -> Html Editor.Msg
viewParsingErrors errors =
    Html.details
        [ HA.style "cursor" "pointer"
        , HA.style "box-shadow" "3px 3px 4px black"
        , HA.style "background-color" "#f0f0f0"
        , HA.style "margin" "0.5em"
        , HA.style "padding" "0.2em 1em"
        ]
        [ errors |> List.map viewParsingError |> Html.ul []
        , Html.summary
            [ HA.style "width" "15em"
            , HA.style "border" "none"
            , HA.style "color" "#888"
            , HA.style "list-style" "none"
            ]
            [ Html.text <| "⚠️ Error parsing " ++ (errors |> List.length |> String.fromInt) ++ " posibilities"
            ]
        ]


errorOr : err -> Result err b -> err
errorOr err res =
    case res of
        Result.Ok _ ->
            err

        Result.Err x ->
            x


unwrapResult : Result a a -> a
unwrapResult res =
    case res of
        Result.Ok x ->
            x

        Result.Err x ->
            x


viewResultsAndErrors : Editor.Model -> Html Editor.Msg
viewResultsAndErrors model =
    let
        res =
            Editor.getText model
                |> Lang.parse
                |> Result.map (\statement -> Lang.context [] |> Eval.eval statement)
    in
    Html.div
        [ HA.style "display" "flex"
        , HA.style "flex-direction" "vertical"
        , HA.style "justify-content" "center"
        ]
        [ Html.div [] [ res |> Result.withDefault (Lang.context []) |> viewContext, Html.div [] [] ]
        , Html.div [] [ res |> errorOr [] |> viewParsingErrors, Html.div [] [] ]
        ]


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
        [ Html.div
            [ HA.style "margin" "10em"
            ]
            [ Editor.view model
            , viewResultsAndErrors model
            ]

        --, Editor.viewDebug model
        ]
    }
