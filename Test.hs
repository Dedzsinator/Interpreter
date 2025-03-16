-- src/Test.hs
module Test where

import Control.Monad (forM_)
import Text.Parsec (parse)
import AST
import Types
import Parser
import Evaluator

data TestCase = TestCase
    { input :: String
    , expected :: Either String Value
    } deriving (Show)

testCases :: [TestCase]
testCases =
    [ TestCase "1 + 2" (Right $ IntVal 3)
    , TestCase "3 * 4" (Right $ IntVal 12)
    , TestCase "10 / 2" (Right $ IntVal 5)
    , TestCase "true && false" (Right $ BoolVal False)
    , TestCase "\"Hello\" ++ \" World\"" (Right $ StrVal "Hello World")
    , TestCase "3 / 0" (Left "division by zero")
    , TestCase "1 + true" (Left "type error: expected integers")
    , TestCase "!true" (Right $ BoolVal False)
    , TestCase "true ? 1 : 0" (Right $ IntVal 1)
    , TestCase "1 ? 2 : 3" (Left "type error: expected boolean condition")
    , TestCase "true || false" (Right $ BoolVal True)
    , TestCase "\"Hello\" ++ 1" (Left "type error: expected strings")
    ]

runTests :: IO ()
runTests = do
    putStrLn "Running tests...\n"
    results <- mapM runTest testCases
    summarizeResults results
  where
    runTest tc = do
        result <- case parse parser "" (input tc) of
            Left err -> pure $ Left $ show err
            Right expr -> runEvalM expr
        let passed = result == expected tc
        printResult tc result passed
        pure passed

    printResult tc result passed = putStrLn $ unlines
        [ "Input: " ++ input tc
        , "Expected: " ++ show (expected tc)
        , "Got: " ++ show result
        , if passed then "✓ PASS" else "✗ FAIL"
        , ""
        ]

    summarizeResults results = do
        let total = length results
            passed = length $ filter id results
        putStrLn $ "Test Summary: " ++ show passed ++ "/" ++ show total ++ " tests passed"