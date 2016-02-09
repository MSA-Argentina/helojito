{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{-# OPTIONS_GHC -fno-warn-orphans #-}

module Web.Helojito.Concurrent where

import           Control.Concurrent.Async.Lifted
import           Control.Monad
import           Control.Monad.Base
import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Control
import           Control.Monad.Trans.Free
import           Data.Functor.Identity

-- | A Concurrent Transformer. Stolen from https://github.com/jwiegley/functors
newtype ConcurrentT m a = ConcurrentT { getConcurrentT :: FreeT Identity m a }

-- | Run concurrently:
--      * @>>=@ runs sequentially
--      * @<*>@ runs in parallel
runConcurrentT :: Monad m => ConcurrentT m a -> m a
runConcurrentT (ConcurrentT m) = iterT runIdentity m

instance (MonadBase b m, Functor f) => MonadBase b (FreeT f m) where
    liftBase = liftBaseDefault

instance (MonadBaseControl b m, Functor f)
         => MonadBaseControl b (FreeT f m) where
    type StM (FreeT f m) a = StM m (FreeF f a (FreeT f m a))
    liftBaseWith f =
        FreeT $ liftM Pure $ liftBaseWith $ \runInBase -> f $ \k -> runInBase $ runFreeT k
    restoreM = FreeT . restoreM

instance Monad m => Functor (ConcurrentT m) where
    fmap f (ConcurrentT m) = ConcurrentT (fmap f m)

instance (Monad m, MonadBaseControl IO m) => Monad (ConcurrentT m) where
    return = ConcurrentT . FreeT . return . Pure
    ConcurrentT (FreeT m) >>= k = ConcurrentT . FreeT $ do
        a <- m -- serialize actions in the Monad
        case a of
            Pure a' -> runFreeT . getConcurrentT $ k a'
            Free r  -> return . Free $ fmap (>>= getConcurrentT . k) r

instance MonadBaseControl IO m => Applicative (ConcurrentT m) where
    pure = return
    ConcurrentT f <*> ConcurrentT a =
        -- run actions concurrently in the Applicative
        ConcurrentT $ withAsync a $ \a' -> ($) <$> f <*> wait a'

instance MonadTrans ConcurrentT where
    lift = ConcurrentT . lift

instance (MonadIO m, MonadBaseControl IO m) => MonadIO (ConcurrentT m) where
    liftIO = ConcurrentT . liftIO

instance (MonadBaseControl IO m, MonadBase IO m)
         => MonadBase IO (ConcurrentT m) where
    liftBase = liftBaseDefault

instance MonadBaseControl IO m => MonadBaseControl IO (ConcurrentT m) where
    type StM (ConcurrentT m) a = StM (FreeT Identity m) a
    liftBaseWith f =
        ConcurrentT $ liftBaseWith $ \runInBase -> f $ \k -> runInBase $ getConcurrentT k
    restoreM = ConcurrentT . restoreM
