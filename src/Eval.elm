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


evalExpr : Lang.State -> Lang.Expr -> Maybe ExprEvaled
evalExpr state expr =
    case expr of
        Lang.Bin op e1 e2 ->
            case ( evalExpr state e1, evalExpr state e2 ) of
                ( Just (EvaledInt x), Just (EvaledInt y) ) ->
                    Just <| evalArith op x y

                -- ( Just (EvaledBool x), Just (EvaledBool y) ) ->
                --     Just <| EvaledBool <| evalBool op x y
                _ ->
                    Nothing

        Lang.Var name ->
            Just <| EvaledInt <| Maybe.withDefault 0 <| Lang.get name state

        Lang.Num x ->
            Just <| EvaledInt <| x



-- Abort semantics is not good


assign : Lang.State -> String -> Lang.Expr -> Maybe ( String, Int )
assign state var expr =
    case evalExpr state expr of
        Just (EvaledInt x) ->
            Just ( var, x )

        _ ->
            Nothing


eval : Lang.Statement -> Lang.Context -> Lang.Context
eval st = Lang.andThen (evalInState st)

evalInState : Lang.Statement -> Lang.State -> Lang.Context
evalInState statement state =
    case statement of
        Lang.Skip ->
            Lang.Active state

        Lang.Abort ->
            Lang.Halt

        Lang.Assignment vars vals ->
            List.map2 (assign state) vars vals
                |> List.filterMap identity
                |> Dict.fromList
                |> Lang.overwrite state

        Lang.Seq statements ->
            List.foldr (\st -> Lang.andThen (evalInState st)) (Lang.Active state) statements

        Lang.Do guards ->
            List.filterMap (validGuards state) guards
                |> List.head
                |> Maybe.map (\st -> evalInState st state)
                |> Maybe.map (\new -> eval (Lang.Do guards) new)
                |> Maybe.withDefault (Lang.Active state)

        Lang.If guards ->
            List.filterMap (validGuards state) guards
                |> List.head
                |> Maybe.map (\st -> evalInState st state)
                |> Maybe.withDefault Lang.Halt


validGuards : Lang.State -> Lang.Guard -> Maybe Lang.Statement
validGuards state (Lang.Guard expr statements) =
    case evalExpr state expr of
        Just (EvaledBool True) ->
            Just statements

        _ ->
            Nothing
