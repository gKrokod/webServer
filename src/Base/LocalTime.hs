module Base.LocalTime (localtimeTemplate) where

import Language.Haskell.TH (runIO, Q, Exp(LitE), Lit(StringL))
import Data.Time (getCurrentTime)
import Data.Time (UTCTime)
import GHC.Base

localtimeTemplate :: Q Exp
localtimeTemplate = do
  t <- runIO getCurrentTime
  return $ LitE ( StringL (show t) )

