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

import Network.Wai (responseLBS, Application, rawPathInfo, responseBuilder, requestHeaderUserAgent)
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (status200,  notFound404)
import Network.HTTP.Types.Header (hContentType, hContentLength)
import Control.Exception (bracket_)
import Data.Binary.Builder

import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import qualified Data.ByteString as B

import System.Environment --(getArgs, getProgName)
import System.Directory

isDirectory :: [String] -> Maybe FilePath
isDirectory ("--directory" : path : args) = Just path
isDirectory [_] = Nothing 
isDirectory []  = Nothing 

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
      path | BC.take 6 path == "/echo/" -> appBody req f 
      path | BC.take 11 path == "/user-agent" -> appUserAgent req f 
      path | BC.take 7 path == "/files/" -> appFile req f 
      _ -> app404 req f)

app404 :: Application
app404 _req f = bracket_ 
  (putStrLn $ show "Open404")
  (putStrLn $ "Close404 \n" ++ show _req)
  (f $ responseLBS notFound404 [] "status404\n")

appFile :: Application
appFile req f = bracket_
  (putStrLn $ show "OpenFile")
  (putStrLn $  "CloseFile \n" ++ show req)
  -- (f $ responseLBS status200 [(hContentType, "text/plain"), (hContentLength, (BC.pack $ show bodyLen))] (body <> "\r\n") )
  (do
     fileDirectory <- isDirectory <$> getArgs
     putStrLn "Directory:"
     print fileDirectory
     f undefined
    )
  -- (f $ responseLBS status200 [(hContentType, "text/plain"), (hContentLength, (BC.pack $ show bodyLen))] (body) )
    where 
          body = (1+2) `seq` (B.fromStrict $ BC.drop 6 $ (<> "\n")$rawPathInfo req) -- <> "\r\n"
          -- bodyLen = (+2) $ LBC.length body
          bodyLen =  LBC.length body 

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

appUserAgent :: Application
appUserAgent req f = bracket_
  (putStrLn $ show "OpenUserAgent")
  (putStrLn $  "CloseUserAgent \n" ++ show req)
  (f $ responseLBS status200 [(hContentType, "text/plain"), (hContentLength, (BC.pack $ show bodyLen))] (body) )
    where 
          -- body = (1+2) `seq` (B.fromStrict $ BC.drop 6 $ (<> "\n")$rawPathInfo req) -- <> "\r\n"
          body = (B.fromStrict $ maybe "" (<> "\n") $ requestHeaderUserAgent req)
          bodyLen =  LBC.length body 
                                               -- fileDirectory <- isDirectory <$> getArgs
                                               -- putStrLn "Directory:"
                                               -- case fileDirectory of
                                               --   Nothing -> sendAll clientSocket http404 
                                               --   Just dir -> do 
                                               --     let fileName = dir <>  drop (length ("/files" :: String)) (BC.unpack path)
                                               --     exist <- doesFileExist fileName
                                               --     case exist of
                                               --       False -> do
                                               --         if POST `elem` setLine then do
                                               --           print "POST EST"
                                               --           case  isBody setLine of
                                               --             Nothing -> sendAll clientSocket http404
                                               --             Just bodyFile -> do 
                                               --               BC.writeFile fileName bodyFile
                                               --               let body' = mconcat[ http201 
                                               --                                  , "Location: " <> BC.pack fileName
                                               --                                  , endResponse
                                               --                                  , "Content-Type: application/octet-stream"
                                               --                                  , endResponse
                                               --                                  , endResponse]
                                               --               sendAll clientSocket body'
                                               --         else sendAll clientSocket http404
