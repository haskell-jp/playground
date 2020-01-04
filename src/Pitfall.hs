{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Pitfall where

import qualified Data.ByteString as B

-- *Pitfall> bytestringIsString
-- S�kaoL
bytestringIsString :: IO ()
bytestringIsString = B.putStr "こんにちは世界\n"
