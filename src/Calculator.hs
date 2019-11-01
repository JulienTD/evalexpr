module Calculator where
import Data.Char
import Prelude
import Data.List

-- data Expr = Add Expr Expr
--           | Sub Expr Expr
--           | Mul Expr Expr
--           | Lit Integer

-- import Control.Applicative ((<$>), (<*>))

-- type Operator = Double -> Double -> Double
-- type Entry = (String, Operator)
-- type Register = [Entry]

-- modulu :: Double -> Double -> Double
-- modulu a b = fromIntegral $ mod (round a) (round b)

-- operatorRegister :: Register
-- operatorRegister = [
--                 ("-", (-)),
--                 ("+", (+)),
--                 ("/", (/)),
--                 ("*", (*)),
--                 ("%", modulu)
--             ]

-- main = print $ calculate "2 * 3 * 2 + 5 % 2"

-- calculate :: String -> Maybe Double
-- calculate = eval operatorRegister . words

-- eval :: Register -> [String] -> Maybe Double
-- eval [] _ = Nothing -- No operator found.
-- eval _ [] = Nothing -- If a operator don't have anything to operate on.
-- eval _ [number] = Just $ read number
-- eval ((operator, function):rest) unparsed =
--     case span (/=operator) unparsed of
--         (_, []) -> eval rest unparsed
--         (beforeOperator, afterOperator) -> 
--             function
--                 <$> (eval operatorRegister beforeOperator)
--                 <*> (eval operatorRegister $ drop 1 afterOperator)

-- strParser :: String -> [Expr]
-- strParser (x:xs) =
--     case x of
--         '+' -> 
--         '-' -> 


-- Removes chars from a string
-- @param String expression
-- @param String the filter
-- @return String the cleared string
-- @example filterChars "2 + 3" " " => "2+3"
filterChars :: String -> String -> String
filterChars string f = (filter (flip notElem f) string)

data Expr = SumExpr Expr Expr
          | SubExpr Expr Expr
          | MulExpr Expr Expr
          | DivExpr Expr Expr
          | PowExpr Expr Expr
          | ModExpr Expr Expr
          | ValSignedExpr Operator Expr
          | ValExpr Double
          deriving (Show, Eq)

data Operator = Plus | Minus | Times | Div | Power | Modulo | PLeft | PRight | Err String
    deriving (Show, Eq)

data Token = TokOp Operator
            | TokNum Double
            | TokErr String
    deriving (Show, Eq)

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
            | otherwise = Err $ "This operator is not valid: " ++ [c]

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
retrievePlusAndMinus :: [Token] -> (Expr, [Token])
retrievePlusAndMinus ((TokNum nb) : tokenList) = ((ValExpr (fromIntegral (round nb))), tokenList)
retrievePlusAndMinus ((TokOp op) : tokenList) | elem op [Plus, Minus] =
                    let (tree, rest) = retrievePlusAndMinus tokenList
                    in
                        ((ValSignedExpr op tree), rest)
retrievePlusAndMinus token = error $ "No more token"

-- power :: [Token] -> (Expression, [Token])
-- power ((TokNum nb) : t) = ((NumNode (fromIntegral (round nb))), t) -- if first token is a number just return nbr
-- power ((TokOp op) : t) | elem op [Plus, Minus] = -- if operator is + or - like +3 or -2, we parse the next token to get the nbr or - or +
--         let (tree , rest) = power t
--         in
--             ((UnaryNode op tree), rest) -- if first token is operator
-- -- power ((TokOp op):t) | op == PLeft =
-- --     let (tree, rest) = expression t
-- --     in
-- --         case rest of
-- --             ((TokOp op):rest) | op == PRight -> (tree, rest) -- Return
-- --                               | otherwise -> error $ "Mismatching parenthesis"
--     | otherwise = error $ "Empty Tokens" -- if there is no token left

-- Retrieves the operator MODULO (%)
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrieveModulo :: [Token] -> (Expr, [Token])
retrieveModulo tokenList = let (leftTree, rest) = retrievePlusAndMinus tokenList
                         in
                            case rest of
                                ((TokOp op):tokenList) | op == Modulo -> -- Si l'operateur est un plus ou un moins
                                    let (rightTree, rest') = retrieveModulo tokenList
                                    in
                                        ((ModExpr leftTree rightTree), rest') -- Return
                                otherwise -> (leftTree, rest)

-- Retrieves the operator POWER (^)
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrievePower :: [Token] -> (Expr, [Token])
retrievePower tokenList = let (leftTree, rest) = retrieveModulo tokenList
                         in
                            case rest of
                                ((TokOp op):tokenList) | op == Power -> -- Si l'operateur est un plus ou un moins
                                    let (rightTree, rest') = retrievePower tokenList
                                    in
                                        ((PowExpr leftTree rightTree), rest') -- Return
                                otherwise -> (leftTree, rest)

-- Retrieves the operator DIV (/)
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrieveDiv :: [Token] -> (Expr, [Token])
retrieveDiv tokenList = let (leftTree, rest) = retrievePower tokenList
                         in
                            case rest of
                                ((TokOp op):tokenList) | op == Div -> -- Si l'operateur est un plus ou un moins
                                    let (rightTree, rest') = retrieveDiv tokenList
                                    in
                                        ((DivExpr leftTree rightTree), rest') -- Return
                                otherwise -> (leftTree, rest)

-- Retrieves the operator TIMES (*)
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrieveTimes :: [Token] -> (Expr, [Token])
retrieveTimes tokenList = let (leftTree, rest) = retrieveDiv tokenList
                         in
                            case rest of
                                ((TokOp op):tokenList) | op == Times -> -- Si l'operateur est un plus ou un moins
                                    let (rightTree, rest') = retrieveTimes tokenList
                                    in
                                        ((MulExpr leftTree rightTree), rest') -- Return
                                otherwise -> (leftTree, rest)

-- Retrieves the operator SUB (-)
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrieveSub :: [Token] -> (Expr, [Token])
retrieveSub tokenList = let (leftTree, rest) = retrieveTimes tokenList
                         in
                            case rest of
                                ((TokOp op):tokenList) | op == Minus -> -- Si l'operateur est un plus ou un moins
                                    let (rightTree, rest') = retrieveSub tokenList
                                    in
                                        ((SubExpr leftTree rightTree), rest') -- Return
                                otherwise -> (leftTree, rest)


-- Retrieves the operator PLUS (+)
-- @param Token tokens composing the base string
-- @return (Expr, [Token]) the current expression + the tokens left
retrievePlus :: [Token] -> (Expr, [Token])
retrievePlus tokenList = let (leftTree, rest) = retrieveSub tokenList
                         in
                            case rest of
                                ((TokOp op):tokenList) | op == Plus -> -- Si l'operateur est un plus ou un moins
                                    let (rightTree, rest') = retrievePlus tokenList
                                    in
                                        ((SumExpr leftTree rightTree), rest') -- Return
                                otherwise -> (leftTree, rest)

parse :: [Token] -> Expr
parse t = let (tree, tokens) = retrievePlus t in
            if null tokens
                then tree
                else error $ "Syntax Error: " ++ show tokens

modulu :: Double -> Double -> Double
modulu a b = fromIntegral $ mod (round a) (round b)

evaluate :: Expr -> Double
evaluate (SumExpr left right) = (evaluate left) + (evaluate right)
evaluate (SubExpr left right) = (evaluate left) - (evaluate right)
evaluate (MulExpr left right) = (evaluate left) * (evaluate right)
evaluate (DivExpr left right) = (evaluate left) / (evaluate right)
evaluate (PowExpr left right) = (evaluate left) ** (evaluate right)
evaluate (ModExpr left right) = modulu (evaluate left) (evaluate right)
evaluate (ValSignedExpr operator expr) | operator == Minus = 0 - (evaluate expr)
                                       | operator == Plus = (evaluate expr)
evaluate (ValExpr nbr) = nbr

-- evalExpr :: String -> Expr
-- evalExpr = parse . getTokens

evalExpr :: String -> Double
evalExpr str = let (clearedStr) = filterChars str " \t" in
    let (tokens) = getTokens clearedStr in
        let (expr) = parse tokens in
            let (result) = evaluate expr in
                case result of
                    otherwise -> (result)
