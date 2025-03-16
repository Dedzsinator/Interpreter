-- src/Evaluator.hs
{-# LANGUAGE LambdaCase #-}
module Evaluator where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import AST
import Types

class Evaluable a where
    eval :: a -> EvalM Value

instance Evaluable Expr where
    eval = \case
        Num n -> pure $ IntVal n
        Var x -> asks (lookup x) >>= maybe (throwError $ "undefined: " ++ x) pure
        Add e1 e2 -> evalBinOp (+) e1 e2
        Mul e1 e2 -> evalBinOp (*) e1 e2
        Div e1 e2 -> evalDiv e1 e2
        Ternary c t e -> evalTernary c t e
        Bool b -> pure $ BoolVal b
        And e1 e2 -> evalBoolOp (&&) e1 e2
        Or e1 e2 -> evalBoolOp (||) e1 e2
        Not e -> evalNot e
        Str s -> pure $ StrVal s
        Concat e1 e2 -> evalConcat e1 e2

evalBinOp :: (Int -> Int -> Int) -> Expr -> Expr -> EvalM Value
evalBinOp op e1 e2 = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (IntVal n1, IntVal n2) -> pure $ IntVal (op n1 n2)
        _ -> throwError "type error: expected integers"

evalDiv :: Expr -> Expr -> EvalM Value
evalDiv e1 e2 = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (IntVal _, IntVal 0) -> throwError "division by zero"
        (IntVal n1, IntVal n2) -> pure $ IntVal (div n1 n2)
        _ -> throwError "type error: expected integers"

evalTernary :: Expr -> Expr -> Expr -> EvalM Value
evalTernary c t e = do
    condVal <- eval c
    case condVal of
        BoolVal b -> if b then eval t else eval e
        _ -> throwError "type error: expected boolean condition"

evalBoolOp :: (Bool -> Bool -> Bool) -> Expr -> Expr -> EvalM Value
evalBoolOp op e1 e2 = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (BoolVal b1, BoolVal b2) -> pure $ BoolVal (op b1 b2)
        _ -> throwError "type error: expected booleans"

evalNot :: Expr -> EvalM Value
evalNot e = do
    v <- eval e
    case v of
        BoolVal b -> pure $ BoolVal (not b)
        _ -> throwError "type error: expected boolean"

evalConcat :: Expr -> Expr -> EvalM Value
evalConcat e1 e2 = do
    v1 <- eval e1
    v2 <- eval e2
    case (v1, v2) of
        (StrVal s1, StrVal s2) -> pure $ StrVal (s1 ++ s2)
        _ -> throwError "type error: expected strings"

runEvalM :: Expr -> IO (Either String Value)
runEvalM expr = evalStateT (runExceptT (runReaderT (eval expr) [])) []