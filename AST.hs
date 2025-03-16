-- src/AST.hs
module AST where

data Expr = Num Int 
         | Var String 
         | Add Expr Expr 
         | Mul Expr Expr 
         | Div Expr Expr
         | Ternary Expr Expr Expr -- Complex if-else
         | Bool Bool 
         | And Expr Expr 
         | Or Expr Expr 
         | Not Expr
         | Str String 
         | Concat Expr Expr 
         deriving (Show, Eq)

data Value = IntVal Int 
          | BoolVal Bool 
          | StrVal String 
          deriving (Show, Eq)

type Env = [(String, Value)]