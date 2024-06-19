module Base.LocalTime (localtimeTemplate) where

import Language.Haskell.TH (runIO, Q, Exp(LitE), Lit(StringL))
import Data.Time (getCurrentTime)
import Data.Time (UTCTime)

localtimeTemplate :: Q Exp
localtimeTemplate = (LitE . StringL . show) <$> runIO getCurrentTime
