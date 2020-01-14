{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeOperators #-}

module Extensible.GetOpt where

import Control.Lens ((^.), (^?), folded)
import Data.Extensible
import Data.Extensible.GetOpt

type Options =
  RecordOf OptDescr'
    '[ "verbose" >: Int,
       "extra" >: [String]
     ]

opts :: Options
opts =
  #verbose @= optNoArg "v" ["verbose"] "verbose"
    <: #extra @= optReqArg "e" ["extra"] "ARG" "extra arguments"
    <: nil

{-
λ> :main
verbose: False
extra: Nothing

λ> :main -v
verbose: True
extra: Nothing

λ> :main -e haskell
verbose: False
extra: Just "haskell"
-}
main :: IO ()
main = withGetOpt "test" opts $ \r _args -> do
  putStrLn $ "verbose: " ++ show (r ^. #verbose > 0)
  putStrLn $ "extra: " ++ show (r ^? #extra . folded)
