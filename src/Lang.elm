module Lang exposing (..)

import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), loop, number, oneOf, spaces, succeed, symbol, lazy)
import Set



-- Expr

type Operator
    = Add | Sub | Mul | Div | Les | Grt | Leq | Geq | Neq | Eq
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
getOpSymb op = operators |> List.filter (\(x, _) -> x == op) |> List.head |> Maybe.map Tuple.second

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


step : List (sep, a) -> a -> Maybe sep -> Step (List (sep, a)) (SepList a sep)
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
                        |. spaces
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

-- use andThen for checks
-- List.foldl (\ops -> Parser.map (sepCombine ops)) (chain atom operator) operators |> andThen notEmpty
expression : Parser Expr
expression =
    chain atom operator
        |> Parser.map (sepCombine <| [ Mul, Div ])
        |> Parser.map (sepCombine <| [ Add, Sub ])
        |> Parser.map (sepCombine <| [ Les, Grt, Leq, Geq, Neq, Eq ])
        |> Parser.map Tuple.first



-- Statement


type Guard
    = Guard Expr Statement


type SimpleStatement
    = Skip
    | Abort
    | Assignment (List String) (List Expr)


type Statement
    = Seq (List SimpleStatement)
    | Do (List Guard)
    | If (List Guard)

sepBy : String -> Parser keep -> Parser (List keep)
sepBy sep parser =
    Parser.map (\( x, xs ) -> x :: List.map Tuple.second xs) <| chain parser (symbol sep)

guard : Parser Guard
guard =
    succeed Guard |=  expression |. spaces |. symbol "~>" |. spaces |= lazy (\_ -> statement)


simpleStatement : Parser SimpleStatement
simpleStatement =
    oneOf
        [ succeed Skip |. symbol "skip" |. spaces
        , succeed Abort |. symbol "abort" |. spaces
        , succeed Assignment
            |= sepBy "," variable
            |. spaces
            |. symbol ":="
            |. spaces
            |= sepBy "," expression
            |. spaces
        ]

statement : Parser Statement
statement =
    oneOf
        [ succeed Seq |= sepBy ";" simpleStatement
        , succeed Do
            |. symbol "do" |. spaces |= sepBy "|" guard |. spaces |. symbol "od"
        , succeed If
            |. symbol "if" |. spaces |= sepBy "|" guard |. spaces |. symbol "fi"
        ]
