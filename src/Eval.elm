module Eval exposing (..)

import Dict
import Lang


evalOp : Lang.Operator -> Int -> Int -> Int
evalOp op x y =
    case op of
        Lang.Add ->
            x + y

        Lang.Mul ->
            x * y

        Lang.Sub ->
            x - y

        Lang.Div ->
            x // y


-- Automatically initialize variables to 0
evalExpr : Context -> Lang.Expr -> Int
evalExpr context expr =
    case expr of
        Lang.Bin op e1 e2 ->
            evalOp op (evalExpr context e1) (evalExpr context e2)

        Lang.Var name ->
            Maybe.withDefault 0 <| Dict.get name context

        Lang.Num x ->
            x


type alias Context =
    Dict.Dict String Int



-- List.map2

assign : Context -> String -> Lang.Expr -> (String, Int)
assign context var expr = (var, evalExpr context expr)

evalStatement : Lang.Statement -> Context -> Context
evalStatement statement context =
    case statement of
        Lang.Skip ->
            context

        Lang.Abort ->
            Dict.empty

        Lang.Assignment vars vals ->
            Dict.union (Dict.fromList <| List.map2 (assign context) vars vals) context

eval : Context -> List Lang.Statement -> Context
eval context statements = List.foldl evalStatement context statements
