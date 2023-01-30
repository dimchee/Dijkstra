module Lang exposing (..)

import Dict
import Parser exposing ((|.), (|=), Parser, Step(..), Trailing(..), andThen, loop, number, oneOf, problem, spaces, succeed, symbol)
import Set



-- Expr


type Expr
    = Bin Operator Expr Expr
    | Var String
    | Num Int


show : Expr -> String
show expr =
    case expr of
        Var s ->
            s

        Num i ->
            String.fromInt i

        Bin op a b ->
            "("
                ++ show a
                ++ (operatorToStr
                        |> List.filterMap
                            (\( x, y ) ->
                                if x == op then
                                    Just y

                                else
                                    Nothing
                            )
                        |> List.head
                        |> Maybe.withDefault "???"
                   )
                ++ show b
                ++ ")"


notEmpty : SepList a sep -> Parser a
notEmpty ( x, xs ) =
    if List.isEmpty xs then
        succeed x

    else
        problem "not one expr"


expression : Parser Expr
expression =
    List.foldl (\ops -> Parser.map (chainLeft ops)) (chain atom operator) operators |> andThen notEmpty


type alias SepList a sep =
    ( a, List ( sep, a ) )


sepFoldl : (( sep, b ) -> SepList b sep -> SepList b sep) -> SepList b sep -> SepList b sep
sepFoldl func ( x, xs ) =
    List.foldl func ( x, [] ) xs


sepReverse : SepList a sep -> SepList a sep
sepReverse =
    sepFoldl (\( op, cur ) -> \( prev, acc ) -> ( cur, ( op, prev ) :: acc ))


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
                |= oneOf
                    [ succeed Just
                        |. spaces
                        |= parseB
                        |. spaces
                    , succeed Nothing
                    ]



-- if multiple values  tak es first (maybe should return error?)


getDict : a -> List ( a, b ) -> Maybe b
getDict x =
    List.filter (\( y, _ ) -> x == y) >> List.head >> Maybe.map Tuple.second


combine : List Operator -> ( Operator, Expr ) -> SepList Expr Operator -> SepList Expr Operator
combine ops ( op, right ) ( left, remains ) =
    if List.member op ops then
        ( Bin op left right, remains )

    else
        ( right, ( op, left ) :: remains )


chainLeft : List Operator -> SepList Expr Operator -> SepList Expr Operator
chainLeft ops =
    sepReverse >> sepFoldl (combine ops)



-->> Tuple.mapSecond List.reverse


variable : Parser String
variable =
    Parser.variable
        { start = Char.isLower
        , inner = Char.isAlphaNum
        , reserved = Set.fromList [ "do", "od", "if", "fi" ]
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


operatorToStr : List ( Operator, String )
operatorToStr =
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


operators : List (List Operator)
operators =
    [ [ Mul, Div ]
    , [ Add, Sub ]
    , [ Les, Grt, Leq, Geq, Neq, Eq ]
    ]


operator : Parser Operator
operator =
    oneOf <| List.map (\( op, symb ) -> succeed op |. symbol symb) operatorToStr



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



-- sepBy : String -> Parser keep -> Parser (List keep)
-- sepBy sep parser =
--     loop [] <| sepByHelp sep parser


sepByHelp : String -> Parser keep -> List keep -> Parser (Step (List keep) (List keep))
sepByHelp sep parser revParsed =
    oneOf
        [ succeed (\parsed -> Loop (parsed :: revParsed))
            |= parser
            |. spaces
            |. symbol sep
            |. spaces
        , succeed ()
            |> Parser.map (\_ -> Done (List.reverse revParsed))
        , parser |> Parser.map (\parsed -> Done (parsed :: List.reverse revParsed))
        ]


sepBy : String -> Parser keep -> Parser (List keep)
sepBy sep parser =
    Parser.map (\( x, xs ) -> x :: List.map Tuple.second xs) <| chain parser (symbol sep)


guard : Parser Guard
guard =
    succeed Guard |= expression |. spaces |. symbol "$" |. spaces |= Parser.lazy (\_ -> statement)


simpleStatement : Parser SimpleStatement
simpleStatement =
    oneOf
        [ succeed Skip |. symbol "skip"
        , succeed Abort |. symbol "abort"
        , succeed Assignment
            |= sepBy "," variable
            |. spaces
            |. symbol ":="
            |. spaces
            |= sepBy "," expression
        ]


statement : Parser Statement
statement =
    oneOf
        [ succeed Seq
            |= sepBy ";" simpleStatement

        -- Do not working
        , succeed Do
            |= Parser.sequence
                { start = "do"
                , separator = "|"
                , end = "od"
                , spaces = spaces
                , item = guard
                , trailing = Mandatory -- demand a trailing semi-colon
                }
        ]
