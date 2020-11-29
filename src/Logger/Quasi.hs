{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE QuasiQuotes #-}
module Logger.Quasi (mkLogger) where

import Language.Haskell.TH.Syntax (Q, Exp(..), Loc(..), location, lift)
import Language.Haskell.TH.Quote (QuasiQuoter(..))
import Logger.Logger (putLog)
import Control.Monad.Logger (LogLevel)
import Data.Char (isSpace)
import Data.List (dropWhileEnd)
import Data.String.Interpolate (i)

mkLogger :: LogLevel -> QuasiQuoter
mkLogger lvl = QuasiQuoter 
  { quoteExp = \str ->
    [| putLog $(location >>= liftLoc) lvl $(quoteExp i $ trim str) |]
  , quotePat = error "Not implemented as pattern"
  , quoteType = error "Not implemented as type"
  , quoteDec = error "Not implemented as declaration"
  }

trim :: String -> String
trim = dropWhile isSpace . dropWhileEnd isSpace

liftLoc :: Loc -> Q Exp
liftLoc (Loc a b c (d1, d2) (e1, e2)) = [|Loc
    $(lift a)
    $(lift b)
    $(lift c)
    ($(lift d1), $(lift d2))
    ($(lift e1), $(lift e2))
    |]