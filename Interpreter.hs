{-# LANGUAGE BangPatterns #-}

module Interpreter where

import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad
import Data.Maybe
import Data.List
import AST

data Value = Int Int | Tuple [Value] | Function (Value -> IO Value)

instance Show Value where
  show (Int n) = show n
  show (Tuple t) = "(" ++ intercalate "," (map show t) ++ ")"
  show _ = "f"

--data Expr = IntLit Int | Name Symbol | TupleLit [Expr] | App Expr Expr | Let Pattern Expr Expr | Case Expr [(Pattern, Expr)] | Lambda Pattern Expr | Seq Expr Expr deriving Show

assignPattern :: Map Symbol Value -> Pattern -> Value -> Maybe (Map Symbol Value)
assignPattern m (IntPat n) (Int n') | n == n' = Just m
assignPattern m (NamePat n) v = Just $ Map.insert n v m
assignPattern m (TuplePat []) (Tuple []) = Just m
assignPattern m (TuplePat (x:xs)) (Tuple (y:ys)) = assignPattern m x y >>= (\h -> assignPattern h (TuplePat xs) (Tuple ys))
assignPattern m Wildcard _ = Just m
assignPattern _ _ _ = Nothing

eval :: Map Symbol Value -> Expr -> IO Value
eval !_ (IntLit n) = pure $ Int n
eval !m (Name n) = pure $ m Map.! n
eval !m (TupleLit x) = Tuple <$> mapM (eval m) x
eval !m (App x y) = do
  (Function f) <- eval m x
  x <- eval m y
  f x
eval !m (Let p v e) = eval m v >>= \v'' -> eval (fromJust (assignPattern m p v'')) e
eval !m (Seq x y) = eval m x >> eval m y
eval !m (Lambda p e) = pure $ Function (\v -> eval (fromJust (assignPattern m p v)) e)
eval !_ (Case _ []) = error "case failed"
eval !m (Case v ((x, e):xs)) = eval m v >>= \v' -> case assignPattern m x v' of
  Just m' -> eval m' e
  Nothing -> eval m (Case v xs)
