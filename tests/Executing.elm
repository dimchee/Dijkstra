module Executing exposing (..)

import Eval
import Expect
import Lang
import Test exposing (..)


testStatement : Lang.Statement
testStatement =
    Lang.Seq []

empty : Lang.Context
empty = Lang.context []
valued : String -> Int -> Lang.Context
valued var val = Lang.context <| List.singleton (var, val)


validGuardsTest : Test
validGuardsTest =
    describe "valiGuards function test"
        [ test "a < 3 ~> _ in a=4 context"
            (\_ ->
                Lang.Guard (Lang.Bin Lang.Les (Lang.Var "a") (Lang.Num 3)) testStatement
                    |> Eval.validGuards (valued "a" 4)
                    |> Expect.equal Nothing
            )
        , test "a < 3 ~> _ in a=2 context"
            (\_ ->
                Lang.Guard (Lang.Bin Lang.Les (Lang.Var "a") (Lang.Num 3)) testStatement
                    |> Eval.validGuards (valued "a" 2)
                    |> Expect.equal (Just testStatement)
            )
        , test "a < 3 ~> _ in empty context"
            (\_ ->
                Lang.Guard (Lang.Bin Lang.Les (Lang.Var "a") (Lang.Num 3)) testStatement
                    |> Eval.validGuards empty
                    |> Expect.equal (Just testStatement)
            )
        ]

doTest : Test
doTest =
    describe "DO semantics correctness test"
        [ test "do a < 3 ~> a := a + 1 od"
            (\_ ->
                Lang.parse "do a < 3 ~> a := a + 1 od"
                    |> Result.withDefault testStatement
                    |> (\st -> Eval.eval st empty)
                    |> Expect.equal (Just <| valued "a" 3)
            )
        , test "Euclids algorithm"
            (\_ ->
                Lang.parse "a, b := 30, 18;  do a > b ~> a := a - b | b > a ~> b := b - a od"
                    |> Result.withDefault testStatement
                    |> (\st -> Eval.eval st empty)
                    |> Expect.equal (Just <| Lang.context [("a", 6), ("b", 6)])
            )
        ]
