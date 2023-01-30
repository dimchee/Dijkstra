module Parsing exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Lang exposing (..)
import Parser exposing (succeed, (|.))
import Test exposing (..)


sepTest : Test
sepTest =
    describe "SepList basic functionality"
        [ test "reverse1"
            (\_ ->
                sepReverse ( 1, [ ( "+", 2 ), ( "+", 3 ) ] )
                    |> Expect.equal ( 3, [ ( "+", 2 ), ( "+", 1 ) ] )
            )
        , test "reverse2"
            (\_ ->
                sepReverse ( 1, [ ( "+", 2 ), ( "+", 3 ), ( "+", 4 ) ] )
                    |> Expect.equal ( 4, [ ( "+", 3 ), ( "+", 2 ), ( "+", 1 ) ] )
            )
        ]


exprTest : Test
exprTest =
    describe "Parsing Expressions"
        [ test "1 + 2 + 3"
            (\_ ->
                Parser.run expression "1 + 2 + 3"
                    |> Result.withDefault (Var "error")
                    |> Expect.equal (Bin Add (Bin Add (Num 1) (Num 2)) (Num 3))
            )
        , test "1 + 2 * 3"
            (\_ ->
                Parser.run expression "1 + 2 * 3"
                    |> Result.withDefault (Var "error")
                    |> Expect.equal (Bin Add (Num 1) <| Bin Mul (Num 2) (Num 3))
            )
        , test "1 + 2 * 3 + 4"
            (\_ ->
                Parser.run expression "1 + 2 * 3 + 4"
                    |> Result.withDefault (Var "error")
                    |> Expect.equal (Bin Add (Bin Add (Num 1) (Bin Mul (Num 2) (Num 3))) (Num 4))
            )
        , test "1 + 2 < 10"
            (\_ ->
                Parser.run expression "1 + 2 < 10"
                    |> Result.withDefault (Var "error")
                    |> Expect.equal (Bin Les (Bin Add (Num 1) (Num 2)) (Num 10))
            )
        ]


statementTest : Test
statementTest =
    describe "Parsing statements"
        [ test "do 3<2~>skip od"
            (\_ ->
                True |> Expect.true "test"
            )
        , test "spaces"
            (\_ ->
                Parser.run (succeed identity |. Parser.spaces |. Parser.symbol "~>" |. Parser.spaces) " ~> " 
                    |> Expect.ok
            )
        ]
