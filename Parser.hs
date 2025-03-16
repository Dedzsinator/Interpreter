module Parser where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Expr
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Token
import Control.Monad (void)
import Data.Functor.Identity (Identity)
import AST

-- Create a lexer
languageDef = emptyDef {
    Token.commentStart    = "/*",
    Token.commentEnd      = "*/",
    Token.commentLine     = "//",
    Token.nestedComments  = True,
    Token.identStart      = letter,
    Token.identLetter     = alphaNum <|> oneOf "_'",
    Token.reservedNames   = ["true", "false"],
    Token.reservedOpNames = ["+", "*", "/", "&&", "||", "!", "?", ":", "++"]
}

lexer :: Token.TokenParser ()
lexer = Token.makeTokenParser languageDef

-- Lexer helpers using the created lexer
identifier :: Parser String
identifier = Token.identifier lexer

reserved :: String -> Parser ()
reserved = Token.reserved lexer

reservedOp :: String -> Parser ()
reservedOp = Token.reservedOp lexer

parens :: Parser a -> Parser a
parens = Token.parens lexer

integer :: Parser Integer
integer = Token.integer lexer

stringLiteral :: Parser String
stringLiteral = Token.stringLiteral lexer

whiteSpace :: Parser ()
whiteSpace = Token.whiteSpace lexer

class Parseable a where
    parser :: Parser a

instance Parseable Expr where
    parser = expr

-- Operator table for expressions
operatorTable :: OperatorTable String () Identity Expr
operatorTable = [ [Prefix (reservedOp "!" >> return Not)]
                , [binary "*" Mul, binary "/" Div]
                , [binary "++" Concat]
                , [binary "+" Add]
                , [binary "&&" And]
                , [binary "||" Or]
                ]

binary :: String -> (Expr -> Expr -> Expr) -> Operator String () Identity Expr
binary name fun = Infix (do
    reservedOp name
    return fun) AssocLeft

-- src/Parser.hs [relevant parts]
expr :: Parser Expr
expr = try ternaryExpr <|> buildExpressionParser operatorTable term <?> "expression"

term :: Parser Expr
term = parens expr
    <|> (Num . fromInteger <$> integer)
    <|> (Bool True <$ reserved "true")
    <|> (Bool False <$ reserved "false")
    <|> (Str <$> stringLiteral)
    <|> (Var <$> identifier)

ternaryExpr :: Parser Expr
ternaryExpr = do
    c <- buildExpressionParser operatorTable term  -- Parse condition using normal expression parser
    reservedOp "?"
    t <- buildExpressionParser operatorTable term  -- Parse then-expr using normal expression parser
    reservedOp ":"
    e <- buildExpressionParser operatorTable term  -- Parse else-expr using normal expression parser
    return $ Ternary c t e