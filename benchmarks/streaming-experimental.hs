{-# LANGUAGE BangPatterns, LambdaCase, DataKinds, FlexibleContexts #-}
module Main where
import qualified Data.Conduit      as C
import qualified Data.Conduit.List as C
import qualified Data.Conduit.Combinators as CC
import qualified Streaming.Experimental.Feeder       as F
import qualified Streaming.Experimental.Predator       as Pd
import qualified Data.Machine      as M
import qualified Data.Iteratee.Iteratee as I
import qualified Pipes             as P
import qualified Pipes.Prelude     as P
import qualified Data.Boombox as B
import qualified Data.Boombox.Tap as B
import qualified Data.Extensible as E
import qualified Data.Extensible.Effect.Pipe as E
import Data.Functor.Identity
import Control.Monad
import Criterion.Main
import Data.List
import Data.Function

drainM :: M.ProcessT Identity Int o -> ()
drainM m = runIdentity $ M.runT_ (sourceM M.~> m)

drainP :: P.Proxy () Int () a Identity () -> ()
drainP p = runIdentity $ P.runEffect $ P.for (sourceP P.>-> p) P.discard

drainC :: C.Conduit Int Identity a -> ()
drainC c = runIdentity $ (sourceC C.$= c) C.$$ C.sinkNull

drainPd :: Pd.Heterotroph Int a Identity () -> ()
drainPd h = fst $ runIdentity $ Pd.prey Pd.sinkNull $ sourcePd Pd.@-> h

drainF :: F.Rancher Int a Identity () -> ()
drainF h = runIdentity $ F.killEater $ snd $ runIdentity $ F.feed sourceF $ h F.>-$ F.sinkNull

drainI :: I.Nullable a => I.Enumeratee Int a Identity () -> ()
drainI h = runIdentity $ I.run $ runIdentity $ I.run $ runIdentity $ sourceI $ h $ I.mapChunksM_ $ const $ return ()

drainB :: B.Recorder Identity Identity Maybe Int a -> ()
drainB h = maybe () (\(_,_,r) -> r) $ sourceB B.@.$ h B.>-$ forever B.await

drainE :: E.Eff [E.AwaitDef Int, E.YieldDef a] r -> ()
drainE h = E.leaveEff $ sourceE E.>-> h
  E.>-> fix (\self -> E.await >>= maybe (return ()) (const self))

instance I.NullPoint Int where
  empty = 0

instance I.Nullable Int where
  nullC = (==0)

value :: Int
value = 10000

sourceM = M.enumerateFromTo 1 value
sourceC = C.enumFromTo 1 value
sourceP = P.each [1..value]

sourcePd :: Pd.Prey Int Identity ()
sourcePd = Pd.yieldMany [1..value]

sourceF :: F.Feeder Int Identity Identity ()
sourceF = F.yieldMany [1..value]

sourceI :: I.Enumerator Int Identity a
sourceI = I.enumList [1..value]

sourceB :: B.Tape Identity Maybe Int
sourceB = B.tap [1..value]

sourceE :: E.Eff '[E.YieldDef Int] ()
sourceE = mapM_ E.yield [1..value]

scanB :: (b -> a -> b) -> b -> B.Recorder Identity Identity m a b
scanB f = go where
  go b = B.Tape $ B.await >>= \x -> let !b' = f b x in return (b', pure $ go b')

main = defaultMain
  [ bgroup "scan"
      [ bench "boombox" $ whnf drainB (scanB (+) 0)
      , bench "extensible" $ whnf drainE (E.scan (+) 0)
      , bench "feeders" $ whnf drainF (F.scan (+) 0)
      , bench "predators" $ whnf drainPd (Pd.scan (+) 0)
      , bench "iteratee" $ whnf drainI (I.unfoldConvStream (\x -> I.liftI $ \case
        I.Chunk i -> let !r = x + i in return (r, r)
        I.EOF _ -> return (x, x)) 0)
      , bench "machines" $ whnf drainM (M.scan (+) 0)
      , bench "pipes" $ whnf drainP (P.scan (+) 0 id)
      , bench "conduit" $ whnf drainC (CC.scanl (+) 0)
      ]
  ]
