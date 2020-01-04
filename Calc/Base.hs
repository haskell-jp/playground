module Calc.Base where

-- | 数式を表すデータ型
data Expr = Val Int
  | Add Expr Expr
  | Mul Expr Expr
  deriving Show

eval :: Expr -> Int
eval (Val x) = x
eval (Add a b) = eval a + eval b
eval (Mul a b) = eval a * eval b

exprs :: [Expr]
exprs =
  [ Val 42
  , Add (Val 2) (Mul (Val 3) (Val 5))
  , Add (Mul (Val 6) (Val 4)) (Mul (Val 8) (Val 8))
  , Mul (Val 0) (Val 1) `Add` Mul (Val 2) (Val 3) `Add` Mul (Val 4) (Val 5)
  ]
