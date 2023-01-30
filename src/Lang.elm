module Lang exposing (..)

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

        Bin Add a b ->
            "(" ++ show a ++ " + " ++ show b ++ ")"

        Bin Sub a b ->
            "(" ++ show a ++ " - " ++ show b ++ ")"

        Bin Mul a b ->
            "(" ++ show a ++ " * " ++ show b ++ ")"

        Bin Div a b ->
            "(" ++ show a ++ " / " ++ show b ++ ")"

        Bin Les a b ->
            show a ++ " < " ++ show b

        Bin Grt a b ->
            show a ++ " > " ++ show b

        Bin Leq a b ->
            show a ++ " <= " ++ show b

        Bin Geq a b ->
            show a ++ " >= " ++ show b

        Bin Neq a b ->
            show a ++ " != " ++ show b

        Bin Eq a b ->
            show a ++ " == " ++ show b


parse : String -> Result (List Parser.DeadEnd) Statement
parse =
    Parser.run statement


notEmpty : ( List a, b ) -> Parser b
notEmpty ( l, expr ) =
    case l of
        [] ->
            succeed expr

        _ ->
            problem "not one expr"


expression : Parser Expr
expression =
    chain atom operator
        |> Parser.map (chainLeft <| [ ( Mul, Bin Mul ), ( Div, Bin Div ) ])
        |> Parser.map (chainLeft <| [ ( Add, Bin Add ), ( Sub, Bin Sub ) ])
        |> andThen notEmpty


rightFold : ( List ( Expr, Operator ), Expr ) -> Expr
rightFold ( revOps, finalExpr ) =
    case revOps of
        [] ->
            finalExpr

        ( expr, op ) :: others ->
            rightFold ( others, Bin op expr finalExpr )


chainLeft : List ( op, a -> a -> a ) -> ( List ( a, op ), a ) -> ( List ( a, op ), a )
chainLeft ops ( revOpsList, finalExpr ) =
    case revOpsList of
        [] ->
            ( [], finalExpr )

        ( expr, op ) :: otherRevOps ->
            case List.filter (\( first, _ ) -> first == op) ops of
                [ ( _, toA ) ] ->
                    Tuple.mapSecond (\x -> toA x finalExpr) <| chainLeft ops ( otherRevOps, expr )

                _ ->
                    let
                        ( newOtherRevOps, newExpr ) =
                            chainLeft ops ( otherRevOps, expr )
                    in
                    ( ( newExpr, op ) :: newOtherRevOps, finalExpr )


termChain : Parser ( List ( Expr, Operator ), Expr )
termChain =
    let
        step revOps expr nextOp =
            case nextOp of
                Nothing ->
                    Done ( revOps, expr )

                Just op ->
                    Loop <| ( expr, op ) :: revOps
    in
    loop [] <|
        \revOps ->
            succeed (step revOps)
                |= atom
                |= oneOf
                    [ succeed Just
                        |. spaces
                        |= operator
                        |. spaces
                    , succeed Nothing
                    ]


variable : Parser String
variable =
    Parser.variable
        { start = Char.isLower
        , inner = Char.isAlphaNum
        , reserved = Set.fromList []
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


operator : Parser Operator
operator =
    oneOf
        [ succeed Add |. symbol "+"
        , succeed Sub |. symbol "-"
        , succeed Mul |. symbol "*"
        , succeed Div |. symbol "/"
        , succeed Les |. symbol "<"
        , succeed Grt |. symbol ">"
        , succeed Leq |. symbol "<="
        , succeed Geq |. symbol ">="
        , succeed Neq |. symbol "!="
        , succeed Eq |. symbol "="
        ]



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


chain : Parser a -> Parser b -> Parser ( List ( a, b ), a )
chain parseA parseB =
    let
        step revOps expr nextOp =
            case nextOp of
                Nothing ->
                    Done ( revOps, expr )

                Just op ->
                    Loop <| ( expr, op ) :: revOps
    in
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


sepBy : String -> Parser keep -> Parser (List keep)
sepBy sep parser =
    Parser.map (\( xs, x ) -> x :: List.map Tuple.first xs) <| chain parser (symbol sep)


guard : Parser Guard
guard =
    succeed Guard |= expression |. spaces |. symbol "$" |. spaces |= Parser.lazy (\_ -> statement)


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
