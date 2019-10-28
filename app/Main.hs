module Main where

import Lib
import System.Console.GetOpt
import System.IO
import System.Exit
import System.Environment
import Data.List
import Data.Char
import Control.Monad
import Text.Printf

checkLength :: Int List -> String
checkLength size list   | size == 1 = list !! 0
                        | otherwise = ""

main :: IO ()
main = do

    args <- getArgs
    -- case 
    -- str <- return( args !! 0)
    putStrLn ("The first arg is " $ checkLength length args, args)

-- checkLength :: (List list, Integral size) => list => size -> String
-- checkLength 1 = "One!"
-- checkLength x = "Not between 1 and 5"
