module Base.LocalTime (localtimeTemplate) where

import Language.Haskell.TH (runIO, Q, Exp(LitE), Lit(StringL))
import Data.Time (getCurrentTime)

localtimeTemplate :: Q Exp
localtimeTemplate = LitE . StringL . show <$> runIO getCurrentTime
