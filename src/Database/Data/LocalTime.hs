module Database.Data.LocalTime (localtimeTemplate) where

import Data.Time (getCurrentTime)
import Language.Haskell.TH (Exp (LitE), Lit (StringL), Q, runIO)

localtimeTemplate :: Q Exp
localtimeTemplate = LitE . StringL . show <$> runIO getCurrentTime
