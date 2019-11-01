module Main where

import Calculator
import System.Environment
import Text.Printf

main :: IO ()
main = do
    args <- getArgs

    case args of
        [] -> putStrLn "Not enough arguments"
        [str] -> (putStrLn $ (printf "%.2f" $ (evalExpr (args!!0))))
        otherwise -> putStrLn "Too many arguments"
