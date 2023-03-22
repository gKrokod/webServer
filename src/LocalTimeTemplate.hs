module LocalTimeTemplate where

import Language.Haskell.TH
import Data.Time

localtimeTemplate :: Q Exp
localtimeTemplate = do
  t <- runIO getCurrentTime
  return $ LitE ( StringL (show t) )
