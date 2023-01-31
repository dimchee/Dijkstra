module Parsing exposing (..)

-- Every list is reversed (cause of parsing)
-- import Fuzz exposing (Fuzzer, int, list, string)

import Expect
import Lang exposing (Expr(..), Guard(..), Operator(..), Statement(..))
import Test exposing (..)


parseExpr : String -> Lang.Expr
parseExpr =
    Lang.parseExpr >> Result.withDefault (Var "error")


parse : String -> Lang.Statement
parse =
    Lang.parse >> Result.withDefault (Assignment [ "error" ] [ Var "error" ])


sepTest : Test
sepTest =
    describe "SepList basic functionality"
        [ test "reverse1"
            (\_ ->
                Lang.sepReverse ( 1, [ ( "+", 2 ), ( "+", 3 ) ] )
                    |> Expect.equal ( 3, [ ( "+", 2 ), ( "+", 1 ) ] )
            )
        , test "reverse2"
            (\_ ->
                Lang.sepReverse ( 1, [ ( "+", 2 ), ( "+", 3 ), ( "+", 4 ) ] )
                    |> Expect.equal ( 4, [ ( "+", 3 ), ( "+", 2 ), ( "+", 1 ) ] )
            )
        ]


exprTest : Test
exprTest =
    describe "Parsing Expressions"
        [ test "1 + 2 + 3"
            (\_ ->
                parseExpr "1 + 2 + 3"
                    |> Expect.equal (Bin Add (Bin Add (Num 1) (Num 2)) (Num 3))
            )
        , test "1 + 2 * 3"
            (\_ ->
                parseExpr "1 + 2 * 3"
                    |> Expect.equal (Bin Add (Num 1) <| Bin Mul (Num 2) (Num 3))
            )
        , test "1 + 2 * 3 + 4"
            (\_ ->
                parseExpr "1 + 2 * 3 + 4"
                    |> Expect.equal (Bin Add (Bin Add (Num 1) (Bin Mul (Num 2) (Num 3))) (Num 4))
            )
        , test "1 + 2 < 10"
            (\_ ->
                parseExpr "1 + 2 < 10"
                    |> Expect.equal (Bin Les (Bin Add (Num 1) (Num 2)) (Num 10))
            )
        ]


statementTest : Test
statementTest =
    describe "Parsing statements"
        [ test "a := 3; b := 4"
            (\_ ->
                parse "a := 3; b := 4"
                    |> Expect.equal
                        (Seq
                            [ Assignment [ "b" ] [ Num 4 ]
                            , Assignment [ "a" ] [ Num 3 ]
                            ]
                        )
            )
        , test "do 3<2 ~> skip od"
            (\_ ->
                parse "do 3<2 ~> skip od"
                    |> Expect.equal
                        (Seq
                            [ Do
                                [ Guard (parseExpr "3 < 2") (Seq [ Skip ]) 
                                ]
                            ]
                        )
            )

        -- , test "x := 1; do x < 3 ~> x := x+1 od"
        --     (\_ ->
        --         parseExpr "x := 1; do x < 3 ~> x := x+1 od"
        --             |> Result.withDefault (Lang.Seq [ Lang.Abort ])
        --             |> Expect.equal (Seq [ Assignment [("x", 1)], Do] )
        --     )
        ]
