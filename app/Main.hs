module Main where

import Lexer
import Control.Monad

main :: IO ()
main = forever $ do
    putStrLn "Enter file name: "
    fName <- getLine
    content <- readFile fName
    print $ fromStringToTokens content
