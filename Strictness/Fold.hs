{-# OPTIONS_GHC -ddump-simpl -ddump-to-file -dsuppress-all -dsuppress-uniques #-}
module Strictness.Fold where
import Data.Foldable

exL,exR,exL',exR' :: Int

exL = foldl (+) 0　[1..1000]
exR = foldr (+) 0 [1..1000]
exL' = foldl' (+) 0 [1..1000]
exR' = foldr' (+) 0 [1..1000]

{-
Rec {
-- RHS size: {terms: 24, types: 15, coercions: 0, joins: 0/0}
exR'_go
  = \ ds eta eta1 ->
      case ds of {
        [] -> eta eta1;
        : y ys ->
          exR'_go
            ys
            (\ z ->
               case y of { I# x -> case z of { I# y1 -> eta (I# (+# x y1)) } })
            eta1
      }
end Rec }

-- RHS size: {terms: 2, types: 0, coercions: 0, joins: 0/0}
exR'1 = I# 0#

-- RHS size: {terms: 6, types: 1, coercions: 0, joins: 0/0}
exR' = exR'_go (eftInt 1# 1000#) id exR'1

Rec {
-- RHS size: {terms: 16, types: 3, coercions: 0, joins: 0/0}
$wgo
  = \ w ww ->
      case w of wild {
        __DEFAULT -> $wgo (+# wild 1#) (+# ww wild);
        1000# -> +# ww 1000#
      }
end Rec }

-- RHS size: {terms: 7, types: 1, coercions: 0, joins: 0/0}
exL' = case $wgo 1# 0# of ww { __DEFAULT -> I# ww }

Rec {
-- RHS size: {terms: 15, types: 3, coercions: 0, joins: 0/0}
$wgo1
  = \ w ->
      case w of wild {
        __DEFAULT ->
          case $wgo1 (+# wild 1#) of ww { __DEFAULT -> +# wild ww };
        1000# -> 1000#
      }
end Rec }

-- RHS size: {terms: 6, types: 1, coercions: 0, joins: 0/0}
exR = case $wgo1 1# of ww { __DEFAULT -> I# ww }

-- RHS size: {terms: 1, types: 0, coercions: 0, joins: 0/0}
exL = exL'
-}
