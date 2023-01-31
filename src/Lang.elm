module Lang exposing (..)
-- TODO andThen for checks

import Dict
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), lazy, loop, number, oneOf, spaces, succeed, symbol)
import Set

type Operator
    = Add
    | Sub
    | Mul
    | Div
    | Les
    | Grt
    | Leq
    | Geq
    | Neq
    | Eq


operators : List ( Operator, String )
operators =
    [ ( Mul, "*" )
    , ( Div, "/" )
    , ( Add, "+" )
    , ( Sub, "-" )
    , ( Les, "<" )
    , ( Grt, ">" )
    , ( Leq, "<=" )
    , ( Geq, ">=" )
    , ( Neq, "!=" )
    , ( Eq, "=" )
    ]


type Expr
    = Bin Operator Expr Expr
    | Var String
    | Num Int


getOpSymb : Operator -> Maybe String
getOpSymb op =
    operators |> List.filter (\( x, _ ) -> x == op) |> List.head |> Maybe.map Tuple.second


show : Expr -> String
show expr =
    case expr of
        Var s ->
            s

        Num i ->
            String.fromInt i

        Bin op a b ->
            "(" ++ show a ++ (getOpSymb op |> Maybe.withDefault "???") ++ show b ++ ")"


type alias SepList a sep =
    ( a, List ( sep, a ) )


sepFoldl : (( sep, b ) -> SepList b sep -> SepList b sep) -> SepList b sep -> SepList b sep
sepFoldl func ( x, xs ) =
    List.foldl func ( x, [] ) xs


sepReverse : SepList a sep -> SepList a sep
sepReverse =
    sepFoldl (\( op, cur ) -> \( prev, acc ) -> ( cur, ( op, prev ) :: acc ))


sepCombine : List Operator -> SepList Expr Operator -> SepList Expr Operator
sepCombine ops =
    let
        combine ( op, right ) ( left, remains ) =
            if List.member op ops then
                ( Bin op left right, remains )

            else
                ( right, ( op, left ) :: remains )
    in
    sepReverse >> sepFoldl combine


sepBy : String -> Parser keep -> Parser (List keep)
sepBy sep parser =
    Parser.map (\( x, xs ) -> x :: List.map Tuple.second xs) <| chain parser (symbol sep)


step : List ( sep, a ) -> a -> Maybe sep -> Step (List ( sep, a )) (SepList a sep)
step revOps expr nextOp =
    case nextOp of
        Nothing ->
            Done ( expr, revOps )

        Just op ->
            Loop <| ( op, expr ) :: revOps


chain : Parser a -> Parser b -> Parser (SepList a b)
chain parseA parseB =
    loop [] <|
        \revOps ->
            succeed (step revOps)
                |= parseA
                |. spaces
                |= oneOf
                    [ succeed Just
                        |= parseB
                        |. spaces
                    , succeed Nothing
                    ]


operator : Parser Operator
operator =
    oneOf <| List.map (\( op, symb ) -> succeed op |. symbol symb) operators


variable : Parser String
variable =
    Parser.variable
        { start = Char.isLower
        , inner = Char.isAlphaNum
        , reserved = Set.fromList [ "skip", "abort", "do", "od", "if", "fi" ]
        }


atom : Parser Expr
atom =
    oneOf
        [ number
            { int = Just Num
            , hex = Nothing
            , octal = Nothing
            , binary = Nothing
            , float = Nothing
            }
        , succeed Var
            |= variable
        ]



expression : Parser Expr
expression =
    chain atom operator
        |> Parser.map (sepCombine <| [ Mul, Div ])
        |> Parser.map (sepCombine <| [ Add, Sub ])
        |> Parser.map (sepCombine <| [ Les, Grt, Leq, Geq, Neq, Eq ])
        |> Parser.map Tuple.first

type Guard
    = Guard Expr Statement

type Statement
    = Skip
    | Abort
    | Assignment (List String) (List Expr)
    | Seq (List Statement)
    | Do (List Guard)
    | If (List Guard)


guard : Parser Guard
guard =
    succeed Guard |= expression |. spaces |. symbol "~>" |. spaces |= lazy (\_ -> statement)


statementSimple : Parser Statement
statementSimple =
    oneOf
        [ succeed Skip |. symbol "skip" |. spaces
        , succeed Abort |. symbol "abort" |. spaces
        , succeed Do
            |. symbol "do"
            |. spaces
            |= sepBy "|" guard
            |. spaces
            |. symbol "od"
            |. spaces
        , succeed If
            |. symbol "if"
            |. spaces
            |= sepBy "|" guard
            |. spaces
            |. symbol "fi"
            |. spaces
        , succeed Assignment
            |= sepBy "," variable
            |. spaces
            |. symbol ":="
            |. spaces
            |= sepBy "," expression
            |. spaces
        ] -- |. spaces -- remove spaces from every other


statement : Parser Statement
statement =
    succeed Seq |= sepBy ";" statementSimple


parse : String -> Result (List Parser.DeadEnd) Statement
parse =
    Parser.run statement


parseExpr : String -> Result (List Parser.DeadEnd) Expr
parseExpr =
    Parser.run expression


type alias Context =
    Dict.Dict String Int


context : List ( String, Int ) -> Context
context =
    Dict.fromList


get : String -> Context -> Maybe Int
get =
    Dict.get


overwrite : Context -> Context -> Context
overwrite c1 c2 =
    Dict.union c2 c1
