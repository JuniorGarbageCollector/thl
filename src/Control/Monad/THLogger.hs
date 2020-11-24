module Control.Monad.THLogger
  ( LoggerT(..)
  , Logger
  , MonadLogger(..)
  , Timed(..)
  , Loc(..)
  , LogLevel(..)
  , LogFunction
  , LogFunctionT
  , LogType'(..)
  , LogType
  , LogStr
  , defaultBufSize
  , showLogLevel
  , ioLogFunction
  , liftLogFunction
  , defaultLogFunction
  , mkLogger
  , runLogger
  , evalLogger
  , runLoggerT
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
  , MonadLogger(..)
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
import System.Log.FastLogger 
  ( LogType'(..)
  , LogType
  , LogStr
  , defaultBufSize
  )

dbg, wrn, err, inf :: QuasiQuoter
dbg = mkLogger LevelDebug
wrn = mkLogger LevelWarn
err = mkLogger LevelError 
inf = mkLogger LevelInfo


