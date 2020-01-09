{-# LANGUAGE Rank2Types, BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving, GADTs, LambdaCase #-}
module Streaming.Experimental.Feeder where

import Control.Monad
import Control.Monad.Trans.Class
import Control.Monad.Skeleton

-- | Monadic consumer

data Phage s m x where
  Await :: Phage s m (Maybe s)
  Leftover :: s -> Phage s m ()
  Lift :: m a -> Phage s m a

type Eater s m = Skeleton (Phage s m)

leftover :: s -> Eater s m ()
leftover = bone . Leftover

await :: Eater s m (Maybe s)
await = bone Await
{-# INLINE await #-}

liftP :: m a -> Eater s m a
liftP = bone . Lift
{-# INLINE liftP #-}

killEater :: Monad m => Eater s m a -> m a
killEater m = case debone m of
  Return a -> return a
  Await :>>= k -> killEater $ k Nothing
  Leftover _ :>>= k -> killEater $ k ()
  Lift m' :>>= k -> m' >>= killEater . k

-- | Monadic producer
newtype Feeder s n m a = Feeder { unFeeder :: forall x r. Eater s n x -> (a -> Eater s n x -> m r) -> m r }

instance Functor (Feeder s n m) where
  fmap = liftM

instance Applicative (Feeder s n m) where
  pure = return
  (<*>) = ap
  (*>) = (>>)

instance Monad (Feeder s n m) where
  return a = Feeder $ \s k -> k a s
  Feeder m >>= k = Feeder $ \s cont -> m s $ \a s' -> unFeeder (k a) s' cont
  Feeder m >> Feeder n = Feeder $ \s cont -> m s $ \_ s' -> n s' cont

instance MonadTrans (Feeder s n) where
  lift m = Feeder $ \s k -> m >>= \a -> k a s

yieldOn :: forall s n m. Monad m => (forall x. n x -> m x) -> s -> Feeder s n m ()
yieldOn t s = Feeder $ \e cont -> go e >>= cont () where
  go :: Eater s n x -> m (Eater s n x)
  go e = case debone e of
    Return a -> return $ a <$ leftover s
    Await :>>= k -> return $ k (Just s)
    Leftover _ :>>= _ -> return $ leftover s >> e
    Lift m :>>= k -> t m >>= go . k
{-# INLINE yieldOn #-}

yield :: Monad m => s -> Feeder s m m ()
yield = yieldOn id
{-# INLINE yield #-}


yieldMany :: forall s m. Monad m => [s] -> Feeder s m m ()
yieldMany xs0 = Feeder $ \e cont -> go xs0 e >>= cont () where
  go :: [s] -> Eater s m x -> m (Eater s m x)
  go [] e = return e
  go (x : xs) e = case debone e of
    Return a -> return $ a <$ mapM_ leftover (x : xs)
    Await :>>= k -> go xs $ k (Just x)
    Leftover _ :>>= _ -> return $ leftover x >> e
    Lift m :>>= k -> m >>= go (x : xs) . k


sinkNull :: Monad m => Eater s m ()
sinkNull = await >>= maybe (return ()) (const sinkNull)

sinkList :: Monad m => Eater s m [s]
sinkList = await >>= \case
  Nothing -> return []
  Just x -> (x:) <$> sinkList

type Rancher a b m = Feeder b m (Eater a m)

feed :: Monad m => Feeder s n m a -> Eater s n x -> m (a, Eater s n x)
feed m s = unFeeder m s ((return.) . (,))

scan :: Monad m => (b -> a -> b) -> b -> Rancher a b m ()
scan f b = lift await >>= \case
  Nothing -> return ()
  Just a -> do
    let !b' = f b a
    yieldOn liftP b'
    scan f b'

(>-$) :: Monad m => Rancher a b m x -> Eater b m r -> Eater a m r
r >-$ e = do
  (_, e') <- feed r e
  liftP $ killEater e'
