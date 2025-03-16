-- src/Main.hs
module Main where

import System.IO
import Text.Parsec (parse)
import Control.Monad (forever)
import Control.Monad.Except
import AST
import Types
import Parser
import Evaluator
import Test

repl :: IO ()
repl = do
    putStrLn "Modern Haskell Interpreter (quit: :q, test: :t)"
    forever $ do
        putStr "> "
        hFlush stdout
        line <- getLine
        case line of
            ":q" -> return ()
            ":t" -> runTests
            _ -> case parse parser "" line of
                Left err -> print err
                Right expr -> runEvalM expr >>= either putStrLn print

main :: IO ()
main = repl