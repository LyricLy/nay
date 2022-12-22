module AST where

import Data.Text (Text)

type Symbol = Text
data Pattern
  = IntPat Int
  | NamePat Symbol
  | TuplePat [Pattern]
  | Wildcard
    deriving Show
data Expr
  = IntLit Int
  | Name Symbol
  | TupleLit [Expr]
  | App Expr Expr
  | Let Pattern Expr Expr
  | Case Expr [(Pattern, Expr)]
  | Lambda Pattern Expr
  | Seq Expr Expr
    deriving Show
