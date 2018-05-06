module Main where

import Lexer
import Parser
import Expr
import Control.Monad
import System.Environment

main :: IO ()
main = do
    args <- getArgs
    if (length args > 0) then do
        content <- readFile (head args)
        if ((length args) > 1) && (args !! 1 == "large")
            then showAST parseExpr content
            else showASTTree parseExpr content
    else return ()
    -- when (length file > 0) $ readFile (head file) >>=
    --     print . fromStringToTokens


-- main :: IO ()
-- main = do
--     input <- getContents
--     showAST parseExpr input
