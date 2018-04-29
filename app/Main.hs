module Main where

import Lexer
import Parser
import Expr
import Control.Monad
import System.Environment

main :: IO ()
main = do
    file <- getArgs
    when (length file > 0) $ readFile (head file) >>= showAST parseExpr
    -- when (length file > 0) $ readFile (head file) >>=
    --     print . fromStringToTokens


showAST :: (String -> Prog) -> String -> IO ()
showAST parser input = do
    let ast = parser input
    print ast

-- main :: IO ()
-- main = do
--     input <- getContents
--     showAST parseExpr input
