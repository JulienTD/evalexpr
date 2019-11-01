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
          deriving (Show, Eq)

data Operator = Plus | Minus | Times | Div | Power | Modulo | PLeft | PRight | Err String
    deriving (Show, Eq)

data Token = TokOp Operator
            | TokNum Double
            | TokErr String
            deriving (Show, Eq)

type AST = (Expr, [Token])

------------------------------------------------------------------------ TOKENIZER

operator :: Char -> Operator
operator c | c == '+' = Plus
           | c == '-' = Minus
           | c == '*' = Times
           | c == '/' = Div
           | c == '^' = Power
           | c == '%' = Modulo
           | c == '(' = PLeft
           | c == ')' = PRight
           | otherwise = Err $ "Wrong operator" ++ [c]

getTokens :: String -> [Token]
getTokens [] = []
getTokens (x:xs)
    | x `elem` "+-*/^()%" = TokOp (operator x) : getTokens xs
    | isDigit x = number x xs
    | otherwise = [TokErr ("Cannot getTokens " ++ [x] )]

isDouble :: Char -> Bool
isDouble c = c `elem` "0123456789."

number :: Char -> String -> [Token]
number c cs =
    let (digs, list) = span isDouble cs in
    TokNum (read (c : digs) :: Double) : getTokens list

------------------------------------------------------------------------ PARSER

-- Retrieves number and its sign (- or +)
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrievePlusAndMinus :: [Token] -> AST
retrievePlusAndMinus ((TokNum nbr) : tokenList) = ((ValExpr (fromIntegral (round nbr))), tokenList)
retrievePlusAndMinus ((TokOp op) : tokenList) | op == Plus =
                    let (tree, rest) = retrievePlusAndMinus tokenList
                    in
                        ((ValPositiveExpr tree), rest)
retrievePlusAndMinus ((TokOp op) : tokenList) | op == Minus =
                    let (tree, rest) = retrievePlusAndMinus tokenList
                        in
                            ((ValNegativeExpr tree), rest)
retrievePlusAndMinus ((TokOp op) : tokenList) | op == PLeft =
                    let (tree, rest) = retrievePlus tokenList
                    in
                        case rest of
                            ((TokOp op):rest) | op == PRight -> (tree, rest) -- Return
                                              | otherwise -> error $ "Mismatching parenthesis"
retrievePlusAndMinus token = error $ "No more token"

-- Retrieves the MODULO (%) operator
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrieveModulo :: [Token] -> AST
retrieveModulo tokenList = let (leftTree, rest) = retrievePlusAndMinus tokenList
                         in
                            case rest of
                                ((TokOp op):tokenList) | op == Modulo -> -- Si l'operateur est un plus ou un moins
                                    let (rightTree, rest') = retrieveModulo tokenList
                                    in
                                        ((ModExpr leftTree rightTree), rest') -- Return
                                otherwise -> (leftTree, rest)

-- Retrieves the POWER (^) operator
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrievePower :: [Token] -> AST
retrievePower tokenList = let (leftTree, rest) = retrieveModulo tokenList
                         in
                            case rest of
                                ((TokOp op):tokenList) | op == Power -> -- Si l'operateur est un plus ou un moins
                                    let (rightTree, rest') = retrievePower tokenList
                                    in
                                        ((PowExpr leftTree rightTree), rest') -- Return
                                otherwise -> (leftTree, rest)

-- Retrieves the DIV (/) operator
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrieveDiv :: [Token] -> AST
retrieveDiv tokenList = let (leftTree, rest) = retrievePower tokenList
                         in
                            case rest of
                                ((TokOp op):tokenList) | op == Div -> -- Si l'operateur est un plus ou un moins
                                    let (rightTree, rest') = retrieveDiv tokenList
                                    in
                                        ((DivExpr leftTree rightTree), rest') -- Return
                                otherwise -> (leftTree, rest)

-- Retrieves the TIMES (*) operator
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrieveTimes :: [Token] -> AST
retrieveTimes tokenList = let (leftTree, rest) = retrieveDiv tokenList
                         in
                            case rest of
                                ((TokOp op):tokenList) | op == Times -> -- Si l'operateur est un plus ou un moins
                                    let (rightTree, rest') = retrieveTimes tokenList
                                    in
                                        ((MulExpr leftTree rightTree), rest') -- Return
                                otherwise -> (leftTree, rest)

-- Retrieves the SUB (-) operator
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrieveSub :: [Token] -> AST
retrieveSub tokenList = let (leftTree, rest) = retrieveTimes tokenList
                         in
                            case rest of
                                ((TokOp op):tokenList) | op == Minus -> -- Si l'operateur est un plus ou un moins
                                    let (rightTree, rest') = retrieveSub tokenList
                                    in
                                        ((SubExpr leftTree rightTree), rest') -- Return
                                otherwise -> (leftTree, rest)

-- Retrieves the PLUS (+) operator
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrievePlus :: [Token] -> AST
retrievePlus tokenList = let (leftTree, rest) = retrieveSub tokenList
                         in
                            case rest of
                                ((TokOp op) : tokenList) | op == Plus -> -- Si l'operateur est un plus ou un moins
                                    let (rightTree, rest') = retrievePlus tokenList
                                    in
                                        ((SumExpr leftTree rightTree), rest') -- Return
                                otherwise -> (leftTree, rest)

-- Retrieves an expression from a mathematical expression (under the token form)
-- @param [Token] the expression under the token form
-- @return Expr the expression
getExpression :: [Token] -> Expr
getExpression tokenList = let (tree, tokens) = retrievePlus tokenList
                          in
                            if null tokens
                                then tree
                                else error $ "Syntax Error: " ++ show tokens

-- Calculates an expr
-- @param Expr the mathematical expression
-- @return Double the result
calculateExpression :: Expr -> Double
calculateExpression (SumExpr left right) = (calculateExpression left) + (calculateExpression right)
calculateExpression (SubExpr left right) = (calculateExpression left) - (calculateExpression right)
calculateExpression (MulExpr left right) = (calculateExpression left) * (calculateExpression right)
calculateExpression (DivExpr left right) = (calculateExpression left) / (calculateExpression right)
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
                        let (expr) = getExpression tokens
                        in
                            let (result) = calculateExpression expr
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
