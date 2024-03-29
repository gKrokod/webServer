-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE QuasiQuotes                #-}
-- {-# LANGUAGE TypeFamilies               #-}
-- {-# LANGUAGE UndecidableInstances       #-}
-- {-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DerivingStrategies #-}
--
--
-- {-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE GADTs                      #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE FlexibleInstances          #-}
-- {-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Main (main) where

import Network.Wai (responseLBS, Application)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200)
import Network.HTTP.Types.Header (hContentType)

import qualified Data.ByteString.Char8 as BC

main :: IO ()
main = do
  let host = "127.0.0.1"
      port = 4221
  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack ( show port)
  run port app

app :: Application
app _req f =
  f $ responseLBS status200 [(hContentType, "text/plain")] "Hello serverok!"
