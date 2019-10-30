module Main where

import Lib
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Control.Monad
import Text.Printf
import System.Exit

-- data Ret = RetIO IO ()
--          | RetInt Int

getUserInput :: Int -> [String] -> String
getUserInput size args
        | size == 1 = args !! 0
        | otherwise = "error"

-- verifyInput :: [Char] -> Int -> Ret
-- verifyInput (char:expression) index
--         | index >= 0 && index < (length expression) && char == 'a' && index == ((length expression) - 1) = 0
--         | index >= 0 && index < (length expression) && char == 'a' = (verifyInput expression (index + 1))
--         | otherwise = (exitWith (ExitFailure 84))

-- data Maybe a = Nothing | Just a

data Expr = SumExpr Expr Expr
          | SubExpr Expr Expr
          | MultExpr Expr Expr
          | DivExpr Expr Expr
          | Power Expr Expr
          | Val Float

type Parser a = String -> Maybe (a, String)

data Cmd = GET -- | POST | PUT | DELETE
        deriving Show

parseString :: a -> String -> Parser a
        --     :: a -> String -> (String -> Maybe (a, String))
parseString a "" input = Just(a, input)
parseString a (x:xs) "" = Nothing
parseString a (x:xs) (y:ys) | x == y = parseString a xs ys
parseString a _ _ = Nothing

parseGet :: Parser Cmd
parseGet = parseString GET "GET"
-- parsePost = _
-- parsePut = _

anyOf :: [Parser a] -> Parser a
-- anyOf :: [String -> Maybe (a, String) -> (String -> Maybe (a, String))]
anyOf [] _ = Nothing
anyOf (p: ps) input =
        case p input of
                Nothing -> anyOf ps input
                ret -> ret

parseCmd :: Parser Cmd
parseCmd = anyOf [parseGet]

main :: IO ()
main = do
    args <- getArgs

    putStrLn (if (length args) > 0 then (args !! 0) else "Not enough arguments")
--     verifyInput (getUserInput (length args) args) 0
