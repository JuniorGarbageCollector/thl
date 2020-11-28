{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE UndecidableInstances#-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
module Logger.Logger where

import Control.Monad.IO.Class (MonadIO(..))
import Control.Monad.Trans (MonadTrans(..))
import Control.Monad.Logger (LogLevel(..), LogStr, Loc(..), ToLogStr(..), MonadLogger(..))
import System.Log.FastLogger.Types (FormattedTime)
import System.Log.FastLogger (LogType, newFastLogger)
import Data.Functor.Identity (Identity(..))

instance (Monad m, MonadLogger m) => MonadLogger (LoggerT 'Timed () m) where
  monadLoggerLog loc _ lvl msg = LoggerT $ \logFunc -> (\logged -> (logged, logged)) <$> logFunc True loc lvl (toLogStr msg) 

instance (Monad m, MonadLogger m) => MonadLogger (LoggerT 'NoTime () m) where
  monadLoggerLog loc _ lvl msg = LoggerT $ \logFunc -> (\logged -> (logged, logged)) <$> logFunc False loc lvl (toLogStr msg) 

instance (Monoid log) => MonadTrans (LoggerT timed log) where
  lift action = LoggerT $ const $ (, mempty) <$> action

instance (Monoid log, MonadIO m) => MonadIO (LoggerT timed log m) where
  liftIO = lift . liftIO 

instance Monad m => Functor (LoggerT timed log m) where 
  fmap f (LoggerT logger) = LoggerT $ \logFunc -> do
    (a, log') <- logger logFunc
    pure (f a, log')

instance (Monoid log, Monad m) => Applicative (LoggerT timed log m) where
  pure a = LoggerT $ \_ -> pure (a, mempty)
  l <*> r = l >>= (<$> r)

instance (Monoid log, Monad m) => Monad (LoggerT timed log m) where
  LoggerT logger >>= f = LoggerT $ \logFunc -> do
    (a, logL) <- logger logFunc
    (b, logR) <- runLoggerT (f a) logFunc
    pure (b, logL <> logR)

data Timed = Timed | NoTime

newtype LoggerT (timed :: Timed) log m a = LoggerT { runLoggerT :: LogFunctionT m log -> m (a, log) }
type Logger timed log = LoggerT timed log Identity 

type LogFunctionT m log = Bool -> Loc -> LogLevel -> LogStr -> m log
type LogFunction log = LogFunctionT Identity log

putLog :: (MonadLogger (LoggerT timed () m), ToLogStr msg) => Loc -> LogLevel -> msg -> LoggerT timed () m ()
putLog a = monadLoggerLog a undefined

runLogger :: Logger timed log a -> LogFunction log -> (a, log)
runLogger logger = runIdentity . runLoggerT logger

evalLogger :: Logger timed log a -> LogFunction log -> a
evalLogger logger = fst . runIdentity . runLoggerT logger

evalLoggerT :: Functor m => LoggerT timed log m a -> LogFunctionT m log -> m a
evalLoggerT logger = fmap fst . runLoggerT logger

showLogLevel :: LogLevel -> LogStr
showLogLevel LevelDebug = "[DEBUG]"
showLogLevel LevelWarn =  "[WARNING]"
showLogLevel LevelError = "[ERROR]"
showLogLevel LevelInfo =  "[INFO]"
showLogLevel (LevelOther lvl) = "[" <> toLogStr lvl <> "]"

liftLogFunction :: (MonadTrans g, Monad m, Monoid log) => LogFunctionT m log -> LogFunctionT (g m) log
liftLogFunction f a b c d = lift $ f a b c d

ioLogFunction :: LogType -> LogFunctionT IO LogStr -> IO (LogFunctionT IO ())
ioLogFunction logType logFunction = do
  (logger, _) <- newFastLogger logType
  let wrappedFunc a b c d = logFunction a b c d >>= logger
  pure wrappedFunc

defaultLogFunction :: Monad m => Maybe (m FormattedTime) -> LogFunctionT m LogStr
defaultLogFunction time timed loc level str = do 
  let noTime = pure mempty
  t <- if timed then maybe noTime ((`mappend` " ")<$>) time else noTime
  let message = toLogStr t <> showLogLevel level <> " {" <> toLogStr moduleName <> ":" 
        <> toLogStr line <> "} " <> str <> "\n"
      (line, _) = loc_start loc
      moduleName = loc_module loc
  pure message
