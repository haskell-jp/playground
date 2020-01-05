{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DataKinds #-}
module Extension.DerivingVia where

import Data.Aeson
import Data.List (stripPrefix)
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import GHC.TypeLits

modded :: forall str. KnownSymbol str => Options
modded = defaultOptions
    { fieldLabelModifier = \x -> maybe x id $ stripPrefix (symbolVal (Proxy @ str)) x
    , omitNothingFields = True
    }

-- | Strip str from the field names in JSON representation
newtype Prefixed str a = Prefixed { unPrefixed :: a }

instance (KnownSymbol str, Generic a, GFromJSON Zero (Rep a)) => FromJSON (Prefixed str a) where
    parseJSON = fmap Prefixed . genericParseJSON (modded @ str)

instance (KnownSymbol str, Generic a, GToJSON Zero (Rep a)) => ToJSON (Prefixed str a) where
    toJSON = genericToJSON (modded @ str) . unPrefixed

data User = User
  { user_id :: !Int
  , user_name :: !Text
  } deriving (Show, Generic)
  deriving (FromJSON, ToJSON) via Prefixed "user_" User

sample :: User
sample = User 42 "Ash"
