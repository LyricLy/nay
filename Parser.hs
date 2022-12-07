{-# LANGUAGE OverloadedStrings #-}

module Parser where

import Data.Void
import Data.Text (Text)
import qualified Data.Text as T
import Text.Megaparsec
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import Control.Monad.Combinators
import Control.Monad.Combinators.Expr
import AST

type Parser = Parsec Void Text

sc = L.space space1 empty empty
lexeme = L.lexeme sc
symbol = L.symbol sc

parens = between (symbol "(") (symbol ")")
braces = between (symbol "{") (symbol "}")

int :: Parser Int
int = lexeme L.decimal

intLit :: Parser Expr
intLit = IntLit <$> int

symbol'' :: Parser Text
symbol'' = (lexeme $ T.pack <$> some ((noneOf :: [Char] -> Parser Char) "\n ;\"\',=.\\{}_()")) <?> "identifier"

name :: Parser Expr
name = lexeme $ Name <$> symbol''

pattern :: Parser Pattern
pattern = try (TuplePat <$> tuply pattern) <|> parens pattern <|> (Wildcard <$ symbol "_") <|> (IntPat <$> int) <|> (NamePat <$> symbol'')

tuply :: Parser a -> Parser [a]
tuply p = symbol "(" >> (([] <$ symbol ")") <|> (:) <$> p <*> (symbol "," >> sepBy1 p (symbol ",")) <* symbol ")")

lambda :: Parser Expr
lambda = symbol "\\" >> Lambda <$> pattern <*> (symbol "." >> expr)

let' :: Parser Expr
let' = symbol "let" >> Let <$> pattern <*> (symbol "=" >> expr') <*> (symbol ";" >> expr) 

tuple :: Parser Expr
tuple = TupleLit <$> tuply expr

case' :: Parser Expr
case' = symbol "case" >> Case <$> expr <*> braces (sepBy ((,) <$> pattern <*> (symbol "." >> expr)) (symbol ","))

term :: Parser Expr
term = try tuple <|> parens expr <|> let' <|> case' <|> lambda <|> intLit <|> name

expr' = makeExprParser term [[InfixL (pure App)]]
expr = makeExprParser expr' [[InfixR (Seq <$ symbol ";")]]

program = space >> expr <* eof
