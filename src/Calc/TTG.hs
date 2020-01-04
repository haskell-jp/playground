{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE FlexibleContexts, UndecidableInstances #-}
module Calc.TTG where

import Data.Kind
import Data.Void

type family XCVal x :: Type
type family XCAdd x :: Type
type family XCMul x :: Type
type family XXExpr x :: Type

type instance XCVal () = ()
type instance XCAdd () = ()
type instance XCMul () = ()
type instance XXExpr () = Void

-- | Trees that growスタイルで定義した型(cf. https://gitlab.haskell.org/ghc/ghc/wikis/implementing-trees-that-grows)
data Expr x = Val (XCVal x) Int
  | Add (XCAdd x) (Expr x) (Expr x)
  | Mul (XCMul x) (Expr x) (Expr x)
  | XExpr (XXExpr x)

deriving instance (Show (XCVal x), Show (XCAdd x), Show (XCMul x), Show (XXExpr x)) => Show (Expr x)

data PowX

type instance XCVal PowX = ()
type instance XCAdd PowX = ()
type instance XCMul PowX = ()
type instance XXExpr PowX = Pow

data Pow = Pow (Expr PowX) (Expr PowX)

convert :: Expr PowX -> Expr ()
convert (Val x a) = Val x a
convert (Add x a b) = Add x (convert a) (convert b)
convert (Mul x a b) = Add x (convert a) (convert b)
convert (XExpr (Pow a b)) = foldr (\_ r -> Mul () (convert a) r) (Val () 1) [1..eval (convert b)]

eval :: Expr () -> Int
eval (Val _ x) = x
eval (Add _ a b) = eval a + eval b
eval (Mul _ a b) = eval a * eval b

exprs :: [Expr PowX]
exprs =
  [ val 42
  , add (val 2) (mul (val 3) (val 5))
  , add (mul (val 6) (val 4)) (mul (val 8) (val 8))
  , mul (val 0) (val 1) `add` mul (val 2) (val 3) `add` mul (val 4) (val 5)
  , pow (val 2) (val 10)
  ]
  where
    pow a b = XExpr (Pow a b)
    val = Val ()
    add = Add ()
    mul = Mul ()

{-
所感
・極めて拡張性が高い
　・例えばFooX Bar = Voidのように定義すれば、コンストラクタを無効化することも可能
・Trees That Grow形式で定義する初期コストが高い
　・Template Haskellで解決できそう？
・余分なフィールド・コンストラクタの対応が面倒
-}
