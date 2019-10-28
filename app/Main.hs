module Main where

import Lib

main :: IO ()
main = do
    putStrLn "hello world"
    -- args <- getArgs

    -- let len [] = 0
    -- len (h:t) = 1 + len t

    -- case len args of
    --     1 -> do
    --         putStrLn "1 argument"
    --     2 -> do
    --         putStrLn "2 argument"
    --     _ -> do
    --             putStrLn "Too many arguments"
    --             return()
