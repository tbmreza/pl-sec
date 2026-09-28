module Main (main) where

import Lib

main :: IO ()
main = do
    putStrLn $ showtm [] (eval [] example)
