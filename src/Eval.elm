module Eval exposing (..)

import Dict
import Lang


type ExprEvaled
    = EvaledBool Bool
    | EvaledInt Int


evalArith : Lang.Operator -> Int -> Int -> ExprEvaled
evalArith op x y =
    case op of
        Lang.Add ->
            EvaledInt <| x + y

        Lang.Mul ->
            EvaledInt <| x * y

        Lang.Sub ->
            EvaledInt <| x - y

        Lang.Div ->
            EvaledInt <| x // y

        Lang.Les ->
            EvaledBool <| x < y

        Lang.Grt ->
            EvaledBool <| x > y

        Lang.Leq ->
            EvaledBool <| x <= y

        Lang.Geq ->
            EvaledBool <| x >= y

        Lang.Neq ->
            EvaledBool <| x /= y

        Lang.Eq ->
            EvaledBool <| x == y



-- evalBool : Lang.Operator -> Bool -> Bool -> Bool
-- evalBool op x y =
--     Debug.todo "Bool operations"
-- Automatically initialize variables to 0
evalExpr : Context -> Lang.Expr -> Maybe ExprEvaled
evalExpr context expr =
    case expr of
        Lang.Bin op e1 e2 ->
            case ( evalExpr context e1, evalExpr context e2 ) of
                ( Just (EvaledInt x), Just (EvaledInt y) ) ->
                    Just <| evalArith op x y

                -- ( Just (EvaledBool x), Just (EvaledBool y) ) ->
                --     Just <| EvaledBool <| evalBool op x y
                _ ->
                    Nothing

        Lang.Var name ->
            Maybe.map EvaledInt <| Dict.get name context

        Lang.Num x ->
            Just <| EvaledInt <| x


type alias Context =
    Dict.Dict String Int



-- Abort semantics is not good


assign : Context -> String -> Lang.Expr -> Maybe ( String, Int )
assign context var expr =
    case evalExpr context expr of
        Just (EvaledInt x) ->
            Just ( var, x )

        _ ->
            Nothing


evalSimple : Lang.SimpleStatement -> Context -> Context
evalSimple statement context =
    case statement of
        Lang.Skip ->
            context

        Lang.Abort ->
            Dict.empty

        Lang.Assignment vars vals ->
            Dict.union (Dict.fromList <| List.filterMap identity <| List.map2 (assign context) vars vals) context


eval : Lang.Statement -> Context -> Context
eval statement context =
    case statement of
        Lang.Seq statements ->
            List.foldr evalSimple context statements

        Lang.Do guards ->
            List.filterMap (validGuards context) guards |> List.head
                |> Maybe.map (\st -> eval st context) |> Maybe.withDefault context

        Lang.If guards ->
            List.filterMap (validGuards context) guards |> List.head
                |> Maybe.map (\st -> eval st context) |> Maybe.withDefault Dict.empty

validGuards : Context -> Lang.Guard -> Maybe Lang.Statement
validGuards context (Lang.Guard expr statements) =
    case evalExpr context expr of
        Just (EvaledBool True) -> Just statements
        _ -> Nothing
