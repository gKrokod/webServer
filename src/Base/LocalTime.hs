module Base.LocalTime (localtimeTemplate) where

import Language.Haskell.TH
import Data.Time (getCurrentTime)

localtimeTemplate :: Q Exp
localtimeTemplate = do
  t <- runIO getCurrentTime
  return $ LitE ( StringL (show t) )
  -- return $ LitE (t)
