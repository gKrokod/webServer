-- {-# LANGUAGE TemplateHaskell            #-}
-- {-# LANGUAGE QuasiQuotes                #-} -- {-# LANGUAGE TypeFamilies               #-}
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

import Network.Wai (responseLBS, Application, rawPathInfo, responseBuilder)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200,  notFound404)
import Network.HTTP.Types.Header (hContentType, hContentLength)
import Control.Exception (bracket_)
import Data.Binary.Builder

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.ByteString as B

main :: IO ()
main = do
  -- will read from config in future ..
  let host = "127.0.0.1"
      port = 4221
  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack ( show port)
  let myApp = checkPath app
  run port myApp
--old  type Application = Request -> ResourceT IO Response
-- 
--last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived   
-- type Respond = Response -> IO ResponseReceived

app :: Application
app _req f = bracket_ 
  (putStrLn $ show "Open200")
  (putStrLn $  "Close200 \n" ++ show _req)
  (f $ responseLBS status200 [(hContentType, "text/plain")] "status200\n")

checkPath :: Application -> Application
checkPath nextApp req f = bracket_
  (putStrLn $ show "Open CheckPath")
  (putStrLn $ show "Close CheckPath")
  (do 
    case rawPathInfo req of
      "/" -> nextApp req f
      -- path | BC.take 6 path == "/echo/" -> appBody req f
      path | BC.take 6 path == "/echo/" -> appBody req f
      _ -> app404 req f)

app404 :: Application
app404 _req f = bracket_ 
  (putStrLn $ show "Open404")
  (putStrLn $  "Close404 \n" ++ show _req)
  (f $ responseLBS notFound404 [] "status404\n")

appBody :: Application
appBody req f = bracket_
  (putStrLn $ show "OpenBody")
  (putStrLn $  "CloseBody \n" ++ show req)
  -- (f $ responseLBS status200 [(hContentType, "text/plain"), (hContentLength, (BC.pack $ show bodyLen))] (body <> "\r\n") )
  (f $ responseLBS status200 [(hContentType, "text/plain"), (hContentLength, (BC.pack $ show bodyLen))] (body) )
    where 
          body = (1+2) `seq` (B.fromStrict $ BC.drop 6 $ (<> "\n")$rawPathInfo req) -- <> "\r\n"
          -- bodyLen = (+2) $ LBC.length body
          bodyLen =  LBC.length body
