module Calculator where
import Data.Char
import Prelude
import Data.List

data Expr = SumExpr Expr Expr
          | SubExpr Expr Expr
          | MulExpr Expr Expr
          | DivExpr Expr Expr
          | PowExpr Expr Expr
          | ModExpr Expr Expr
          | ValPositiveExpr Expr
          | ValNegativeExpr Expr
          | ValExpr Double
          | ErrExpr
          deriving (Show, Eq)

data Operator = Plus
                | Minus
                | Times
                | Div
                | Power
                | Modulo
                | PLeft
                | PRight
                | Err String
                deriving (Show, Eq)

data Token = TokenOperator Operator
            | TokenValue Double
            | TokenError String
            deriving (Show, Eq)

type AST = (Expr, [Token])

------------------------------------------------------------------------ TOKENIZER

-- Transform the math exp (string) to tokens
-- @param String math expr
-- @return [Token] the math expr transformed in tokens
getTokens :: String -> [Token]
getTokens [] = []
getTokens (x:xs)
    | x == '+' = TokenOperator (Plus) : getTokens xs
    | x == '-' = TokenOperator (Minus) : getTokens xs
    | x == '*' = TokenOperator (Times) : getTokens xs
    | x == '/' = TokenOperator (Div) : getTokens xs
    | x == '^' = TokenOperator (Power) : getTokens xs
    | x == '%' = TokenOperator (Modulo) : getTokens xs
    | x == '(' = TokenOperator (PLeft) : getTokens xs
    | x == ')' = TokenOperator (PRight) : getTokens xs
    | isDigit x = getFullNumber x xs
    | otherwise = [TokenError ("Wrong token" ++ [x] )]

-- Check if char is a double number
-- @param Char char composing a string number
-- @return Bool true if the char if from the float number lexical
isDoubleNumber :: Char -> Bool
isDoubleNumber c
        | c `elem` ['0'..'9'] = True
        | c == '.' = True
        | otherwise = False

-- From a given char and string, it returns the full number
-- @param Char the first char (which is a number)
-- @param String the rest of the string
-- @return [Token] once the function finished, it adds the number as token to the token list and return the token list
getFullNumber :: Char -> String -> [Token]
getFullNumber c cs = let (digs, list) = span isDoubleNumber cs
                     in
                        TokenValue (read (c : digs) :: Double) : getTokens list

------------------------------------------------------------------------ PARSER

-- Retrieves number and its sign (- or +)
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrievePlusAndMinus :: [Token] -> AST
retrievePlusAndMinus ((TokenValue nbr) : tokenList) = ((ValExpr nbr), tokenList)
retrievePlusAndMinus ((TokenOperator op) : tokenList) | op == Plus =
                    let (tree, rest) = retrievePlusAndMinus tokenList
                    in
                        ((ValPositiveExpr tree), rest)
retrievePlusAndMinus ((TokenOperator op) : tokenList) | op == Minus =
                    let (tree, rest) = retrievePlusAndMinus tokenList
                        in
                            ((ValNegativeExpr tree), rest)
retrievePlusAndMinus ((TokenOperator op) : tokenList) | op == PLeft =
                    let (tree, rest) = retrievePlus tokenList
                    in
                        case rest of
                            ((TokenOperator op):rest) | op == PRight -> (tree, rest)
                                              | otherwise -> ((ErrExpr), [TokenError "Wrong parenthesis"])
retrievePlusAndMinus token = ((ErrExpr), [TokenError "No more token"])

-- Retrieves the MODULO (%) operator
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrieveModulo :: [Token] -> AST
retrieveModulo tokenList = let (leftTree, rest) = retrievePlusAndMinus tokenList
                         in
                            case rest of
                                ((TokenOperator op):tokenList) | op == Modulo ->
                                    let (rightTree, rest') = retrieveModulo tokenList
                                    in
                                        ((ModExpr leftTree rightTree), rest')
                                otherwise -> (leftTree, rest)

-- Retrieves the POWER (^) operator
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrievePower :: [Token] -> AST
retrievePower tokenList = let (leftTree, rest) = retrieveModulo tokenList
                         in
                            case rest of
                                ((TokenOperator op):tokenList) | op == Power ->
                                    let (rightTree, rest') = retrievePower tokenList
                                    in
                                        ((PowExpr leftTree rightTree), rest')
                                otherwise -> (leftTree, rest)

-- Retrieves the DIV (/) operator
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrieveDiv :: [Token] -> AST
retrieveDiv tokenList = let (leftTree, rest) = retrievePower tokenList
                         in
                            case rest of
                                ((TokenOperator op):tokenList) | op == Div ->
                                    let (rightTree, rest') = retrieveDiv tokenList
                                    in
                                        ((DivExpr leftTree rightTree), rest')
                                otherwise -> (leftTree, rest)

-- Retrieves the TIMES (*) operator
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrieveTimes :: [Token] -> AST
retrieveTimes tokenList = let (leftTree, rest) = retrieveDiv tokenList
                         in
                            case rest of
                                ((TokenOperator op):tokenList) | op == Times ->
                                    let (rightTree, rest') = retrieveTimes tokenList
                                    in
                                        ((MulExpr leftTree rightTree), rest')
                                otherwise -> (leftTree, rest)

-- Retrieves the SUB (-) operator
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrieveSub :: [Token] -> AST
retrieveSub tokenList = let (leftTree, rest) = retrieveTimes tokenList
                         in
                            case rest of
                                ((TokenOperator op):tokenList) | op == Minus ->
                                    let (rightTree, rest') = retrieveSub tokenList
                                    in
                                        ((SubExpr leftTree rightTree), rest')
                                otherwise -> (leftTree, rest)

-- Retrieves the PLUS (+) operator
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrievePlus :: [Token] -> AST
retrievePlus tokenList = let (leftTree, rest) = retrieveSub tokenList
                         in
                            case rest of
                                ((TokenOperator op) : tokenList) | op == Plus ->
                                    let (rightTree, rest') = retrievePlus tokenList
                                    in
                                        ((SumExpr leftTree rightTree), rest')
                                otherwise -> (leftTree, rest)

-- Retrieves an expression from a mathematical expression (under the token form)
-- First we examine the val then the parenthesis then the modulos then the powers then the divs then the muls then the subs and then the plus
-- VAL -> () -> MODULO -> POWER -> DIV -> MUL -> - -> +
-- and in code we use recurssion so it looks like that: VAL <- () <- MODULO <- POWER <- DIV <- MUL <- - <- +
-- @param [Token] the expression under the token form
-- @return Expr the expression
getExpression :: [Token] -> Expr
getExpression tokenList = let (tree, tokens) = retrievePlus tokenList
                          in
                            case tree of
                                ((ErrExpr)) -> (ErrExpr)
                                otherwise -> tree

-- Calculates an expr
-- @param Expr the mathematical expression
-- @return Double the result
calculateExpression :: Expr -> Double
calculateExpression (ErrExpr) = -2000000000000000000000
calculateExpression (SumExpr left right) = (calculateExpression left) + (calculateExpression right)
calculateExpression (SubExpr left right) = (calculateExpression left) - (calculateExpression right)
calculateExpression (MulExpr left right) = (calculateExpression left) * (calculateExpression right)
calculateExpression (DivExpr left right) = let rightValue = (calculateExpression right)
                                            in
                                                case rightValue of
                                                    rightValue | rightValue == 0.0 -> -2000000000000000000000
                                                    otherwise -> ((calculateExpression left) / rightValue)
calculateExpression (PowExpr left right) = (calculateExpression left) ** (calculateExpression right)
calculateExpression (ModExpr left right) = modulo (calculateExpression left) (calculateExpression right)
calculateExpression (ValPositiveExpr expr) = (calculateExpression expr)
calculateExpression (ValNegativeExpr expr) = 0 - (calculateExpression expr)
calculateExpression (ValExpr nbr) = nbr

-- Calculates a mathematical expression
-- This evalExpr currently supports: sum(+), sub(-), mul(*), div(/), pow(^), mod(%)
-- @param String the mathematical expression
-- @return Double the result
-- @example evalExpr "2 + 3" => 5.0
evalExpr :: String -> Double
evalExpr str = let (clearedStr) = filterChars str " \t"
                in
                    let (tokens) = getTokens clearedStr
                    in
                        case tokens of
                            [TokenError e] -> calculateExpression ErrExpr
                            otherwise -> let (expr) = getExpression tokens
                                         in
                                            case expr of
                                                ErrExpr -> calculateExpression ErrExpr
                                                otherwise -> let (result) = calculateExpression expr
                                                            in
                                                                (result)

------------------------------------------------------------------------ Utils

-- Removes chars from a string
-- @param String expression
-- @param String the filter
-- @return String the cleared string
-- @example filterChars "2 + 3" " " => "2+3"
filterChars :: String -> String -> String
filterChars string f = (filter (flip notElem f) string)

-- Calculates the modulo
-- @param Double the left number
-- @param Double the right number
-- @return String the result between the left and right numbers
-- @example modulo "modulo 5 2" => 1
modulo :: Double -> Double -> Double
modulo a b = fromIntegral $ mod (round a) (round b)
