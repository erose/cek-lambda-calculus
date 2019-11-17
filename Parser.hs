module Parser (parseExpr) where
-- Based on Stephen Diehl's implementation:
-- https://github.com/sdiehl/write-you-a-haskell/blob/master/chapter4/untyped/Parser.hs

import Data.Char
import Text.Parsec
import Text.Parsec.Char
import Text.Parsec.String (Parser)
import Text.Parsec.Language (haskellStyle)

import qualified Text.Parsec.Expr as Ex
import qualified Text.Parsec.Token as Tok

import ExprTypes

lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser style
  where ops = ["->","\\","+","*","-","="]
        names = []
        style = haskellStyle {Tok.reservedOpNames = ops,
                              Tok.reservedNames = names,
                              Tok.commentLine = "#"}

identifier :: Parser String
identifier = do
  c <- noneOf ['(', ')', '.', ' ']
  return [c]

parens :: Parser a -> Parser a
parens = Tok.parens lexer

-- contents :: Parser a -> Parser a
-- contents p = do
--   Tok.whiteSpace lexer
--   r <- p
--   eof
--   return r

-- natural :: Parser Integer
-- natural = Tok.natural lexer

variable :: Parser Expr
variable = do
  x <- identifier
  return (Ref x)

-- number :: Parser Expr
-- number = do
--   n <- natural
--   return (Lit (LInt (fromIntegral n)))

lambda :: Parser Expr
lambda = do
  arg <- identifier
  char '.'
  body <- expr
  return $ Lam (arg :=> body)

term :: Parser Expr
term =  (try $ parens expr)
    <|> (try lambda)
    <|> variable

expr :: Parser Expr
expr = do
  terms <- many1 term
  return $ foldl1 (:@) terms

parseExpr :: String -> Either ParseError Expr
parseExpr = parse expr "<stdin>"
