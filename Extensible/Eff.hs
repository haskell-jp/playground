{-# LANGUAGE DataKinds #-}

module Extensible.Eff where

import Control.Monad (replicateM_)
import Control.Monad.Reader (ask)
import Control.Monad.State (get, put)
import Control.Monad.Writer (tell)
import Data.Extensible.Effect (Eff, leaveEff)
import Data.Extensible.Effect.Default
  ( ReaderDef, runReaderDef,
    StateDef, runStateDef,
    WriterDef, runWriterDef,
  )
import Data.Monoid (Sum (Sum))

type ExampleM =
  Eff
    '[ ReaderDef Int,
       StateDef Int,
       WriterDef (Sum Int)
     ]

testEff :: ExampleM ()
testEff = replicateM_ 100 $ do
  i <- ask
  acc <- get
  tell (Sum acc)
  put $! acc + i

runEff :: ExampleM a -> ((a, Int), Sum Int)
runEff =
  leaveEff
    . runWriterDef
    . flip runStateDef 0
    . flip runReaderDef 1

{-
λ> example
(((),100),Sum {getSum = 4950})
-}
example :: IO ()
example = print $ runEff testEff
