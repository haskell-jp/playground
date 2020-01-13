{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedLabels #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}

module Extensible.NamedEff where

import Data.Extensible (Lookup)
import Data.Extensible.Effect
  ( Eff, leaveEff,
    WriterEff, runWriterEff, tellEff
  )

type FooBarM xs =
  ( Lookup xs "foo" (WriterEff String),
    Lookup xs "bar" (WriterEff String)
  )

test :: FooBarM xs => Eff xs ()
test = do
  tellEff #foo "Hello "
  tellEff #bar "hoge"
  tellEff #foo "world"
  tellEff #bar "fuga"

{-
λ> example
(((),"hogefuga"),"Hello world")
(((),"Hello world"),"hogefuga")
-}
example :: IO ()
example = do
  print $ leaveEff $ runWriterEff @"foo" $ runWriterEff @"bar" test
  print $ leaveEff $ runWriterEff @"bar" $ runWriterEff @"foo" test
