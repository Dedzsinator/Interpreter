-- src/Types.hs
module Types where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import AST

type ErrorMsg = String
type EvalM a = ReaderT Env (ExceptT ErrorMsg (StateT Env IO)) a