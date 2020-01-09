{-# LANGUAGE Rank2Types, BangPatterns, ScopedTypeVariables, GeneralizedNewtypeDeriving, GADTs, LambdaCase #-}
module Streaming.Experimental.Predator where

import Control.Monad
import Control.Monad.Trans.Class

-- | Monadic producer
data Prey s m a = Yield s (Prey s m a)
  | Lift (m (Prey s m a))
  | Pure a

instance MonadTrans (Prey s) where
  lift = Lift . fmap Pure

instance Monad m => Functor (Prey s m) where
  fmap = liftM

instance Monad m => Applicative (Prey s m) where
  pure = return
  (<*>) = ap

instance Monad m => Monad (Prey s m) where
  return = Pure
  Pure a >>= k = k a
  Yield s c >>= k = Yield s $ c >>= k
  Lift m >>= k = Lift $ fmap (>>=k) m
  Pure _ >> k = k
  Yield s c >> k = Yield s $ c >> k
  Lift m >> k = Lift $ fmap (>>k) m

killPrey :: Monad m => Prey s m a -> m a
killPrey = \case
  Pure a -> return a
  Yield _ k -> killPrey k
  Lift m -> m >>= killPrey

-- | Produce one element.
yield :: Monad m => s -> Prey s m ()
yield s = Yield s $ Pure ()
{-# INLINE yield #-}

-- | Produce many values.
yieldMany :: (Foldable f, Monad m) => f s -> Prey s m ()
yieldMany = foldr Yield (Pure ())
{-# INLINE yieldMany #-}

-- | Monadic consumer
newtype Predator s n m a = Predator { unPredator :: forall x r. Prey s n x -> (a -> Prey s n x -> m r) -> m r }

instance Functor (Predator s n m) where
  fmap = liftM

instance Applicative (Predator s n m) where
  pure = return
  (<*>) = ap
  (*>) = (>>)

instance Monad (Predator s n m) where
  return a = Predator $ \s k -> k a s
  Predator m >>= k = Predator $ \s cont -> m s $ \a s' -> unPredator (k a) s' cont
  Predator m >> Predator n = Predator $ \s cont -> m s $ \_ s' -> n s' cont

instance MonadTrans (Predator s n) where
  lift m = Predator $ \s k -> m >>= \a -> k a s

awaitOn :: forall n m s. (Monad n, Monad m) => (forall x. n x -> m x) -> Predator s n m (Maybe s)
awaitOn t = Predator go where
  go :: Prey s n x -> (Maybe s -> Prey s n x -> m r) -> m r
  go p cont = case p of
    Pure x -> cont Nothing (return x)
    Yield s k -> cont (Just s) k
    Lift m -> t m >>= flip go cont
{-# INLINE awaitOn #-}

await :: Monad m => Predator s m m (Maybe s)
await = awaitOn id
{-# INLINE await #-}

sinkNull :: Monad m => Predator s m m ()
sinkNull = await >>= maybe (return ()) (const sinkNull)

sinkList :: Monad m => Predator s m m [s]
sinkList = await >>= \case
  Nothing -> return []
  Just x -> (x:) <$> sinkList

type Heterotroph a b m = Predator a m (Prey b m)

prey :: Monad m => Predator s n m a -> Prey s n x -> m (a, Prey s n x)
prey m s = unPredator m s ((return.) . (,))

scan :: Monad m => (b -> a -> b) -> b -> Heterotroph a b m ()
scan f b = awaitOn lift >>= \case
  Nothing -> return ()
  Just a -> do
    let !b' = f b a
    lift $ yield b'
    scan f b'

(@->) :: Monad m => Prey a m x -> Heterotroph a b m r -> Prey b m r
p @-> h = do
  (a, p') <- prey h p
  _ <- lift $ killPrey p'
  return a
