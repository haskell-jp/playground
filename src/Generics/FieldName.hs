{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- ここまで入力するのが地味に面倒
module Generics.FieldName where

import GHC.Generics
import GHC.TypeLits
import Data.Proxy
import Data.String
import Control.Applicative

{-
エレガントに定義できる。
しかし、Generic1のインスタンス導出はGHC 8.8現在HKDに対応していないため、まだ実用できない

data Example h = Example
  { foo :: h Int
  , bar :: h Doubl
  } deriving Generic1

-}

class FieldNamesB b where
  bFieldNames :: IsString a => b (Const a)

  default bFieldNames :: (Generic1 b, FieldNamesB (Rep1 b), IsString a) => b (Const a)
  bFieldNames = to1 bFieldNames

instance FieldNamesB U1 where
  bFieldNames = U1

instance (FieldNamesB t) => FieldNamesB (M1 C m t) where
  bFieldNames = M1 bFieldNames

instance (FieldNamesB t) => FieldNamesB (M1 D m t) where
  bFieldNames = M1 bFieldNames

instance (FieldNamesB f, FieldNamesB g) => FieldNamesB (f :*: g) where
  bFieldNames = bFieldNames :*: bFieldNames

instance (m ~ 'MetaSel ('Just name) su ss ds, IsString a, KnownSymbol name) => FieldNamesB (M1 S m (Rec1 (Const a))) where
  bFieldNames = M1 $ Rec1 $ Const $ fromString $ symbolVal (Proxy :: Proxy name)
