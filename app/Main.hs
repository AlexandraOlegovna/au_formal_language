module Main where

import Lexer
import Control.Monad
import System.Environment

main :: IO ()
main = do
    file <- getArgs
    when (length file > 0) $ readFile (head file) >>= print . fromStringToTokens
