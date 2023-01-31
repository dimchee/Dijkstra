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



-- Automaticaly initialize to 0


evalExpr : Lang.Context -> Lang.Expr -> Maybe ExprEvaled
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
            Just <| EvaledInt <| Maybe.withDefault 0 <| Lang.get name context

        Lang.Num x ->
            Just <| EvaledInt <| x



-- Abort semantics is not good


assign : Lang.Context -> String -> Lang.Expr -> Maybe ( String, Int )
assign context var expr =
    case evalExpr context expr of
        Just (EvaledInt x) ->
            Just ( var, x )

        _ ->
            Nothing


eval : Lang.Statement -> Lang.Context -> Maybe Lang.Context
eval statement context =
    case statement of
        Lang.Skip ->
            Just context

        Lang.Abort ->
            Nothing

        Lang.Assignment vars vals ->
            List.map2 (assign context) vars vals
                |> List.filterMap identity
                |> Lang.context
                |> Lang.overwrite context
                |> Just

        Lang.Seq statements ->
            List.foldr (\st -> Maybe.andThen (eval st)) (Just context) statements

        Lang.Do guards ->
            List.filterMap (validGuards context) guards
                |> List.head
                |> Maybe.andThen (\st -> eval st context)
                |> Maybe.andThen (\new -> eval (Lang.Do guards) new)
                |> Maybe.withDefault context
                |> Just

        Lang.If guards ->
            List.filterMap (validGuards context) guards
                |> List.head
                |> Maybe.andThen (\st -> eval st context)


validGuards : Lang.Context -> Lang.Guard -> Maybe Lang.Statement
validGuards context (Lang.Guard expr statements) =
    case evalExpr context expr of
        Just (EvaledBool True) ->
            Just statements

        _ ->
            Nothing
