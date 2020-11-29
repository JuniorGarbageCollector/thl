module Control.Monad.THLogger
  ( module FastLogger
  , module LoggerMonad
  , LoggerT(..)
  , Logger
  , Timed(..)
  , Loc(..)
  , LogLevel(..)
  , LogFunction
  , LogFunctionT
  , showLogLevel
  , ioLogFunction
  , liftLogFunction
  , defaultLogFunction
  , mkLogger
  , runLogger
  , evalLogger
  , evalLoggerT
  , dbg
  , wrn
  , err
  , inf
  ) where

import Logger.Logger 
  ( LoggerT(..)
  , Logger
  , LogFunctionT
  , LogFunction
  , Timed(..)
  , showLogLevel
  , ioLogFunction
  , liftLogFunction
  , defaultLogFunction
  , runLoggerT
  , evalLoggerT
  , runLogger
  , evalLogger
  )
import Logger.Quasi (mkLogger)
import Control.Monad.Logger (LogLevel(..))
import Language.Haskell.TH.Syntax (Loc(..))
import Language.Haskell.TH.Quote (QuasiQuoter)
import System.Log.FastLogger as FastLogger
import Control.Monad.Logger as LoggerMonad

dbg, wrn, err, inf :: QuasiQuoter
dbg = mkLogger LevelDebug
wrn = mkLogger LevelWarn
err = mkLogger LevelError 
inf = mkLogger LevelInfo


