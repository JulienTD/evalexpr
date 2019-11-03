module Main where

import Calculator
import System.Environment
import Text.Printf
import Control.Exception
import System.Exit

catchAny :: IO a -> (SomeException -> IO a) -> IO a
catchAny = Control.Exception.catch

checkChar :: Char -> Bool
checkChar x
        | x == '+' = True
        | x == '-' = True
        | x == '*' = True
        | x == '/' = True
        | x == '^' = True
        | x == '%' = True
        | x == '(' = True
        | x == ')' = True
        | x == '.' = True
        | x == ' ' = True
        | x == '\t' = True
        | x `elem` ['0'..'9'] = True
        | otherwise           = False

dangerous :: String -> IO Double
dangerous str = do
                if (((all checkChar str) == False) || (evalExpr str) < -2000000000000000000) then error "error" else return (evalExpr str)

main :: IO ()
main = do
    args <- getArgs
    result <- catchAny (dangerous (args!!0))$ \e -> do
        putStrLn $ "Got an exception: " ++ show e
        exitWith (ExitFailure 84)
    (putStrLn $ (printf "%.2f" $ (result)))
