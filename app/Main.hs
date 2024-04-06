module Main (main) where

import Control.Exception (bracket_)
import Data.Binary.Builder
import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy.Char8 as LBC
import Network.HTTP.Types (notFound404, status200, status201)
import Network.HTTP.Types.Header (hContentLength, hContentType, hLocation)
import Network.HTTP.Types.Method (methodGet, methodPost)
import Network.Wai (Application, getRequestBodyChunk, rawPathInfo, requestHeaderUserAgent, requestMethod, responseBuilder, responseLBS)
import Network.Wai.Handler.Warp (run)
-- (getArgs, getProgName)
import System.Directory
import System.Environment

isDirectory :: [String] -> Maybe FilePath
isDirectory ("--directory" : path : args) = Just path
isDirectory [_] = Nothing
isDirectory [] = Nothing

main :: IO ()
main = do
  -- will read from config in future ..
  let host = "127.0.0.1"
      port = 4221
  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack (show port)
  let myApp = checkPath app
  run port myApp

-- old  type Application = Request -> ResourceT IO Response
--
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- type Respond = Response -> IO ResponseReceived

app :: Application
app _req f =
  bracket_
    (putStrLn $ show "Open200")
    -- (putStrLn $  "Close200 \n" ++ show _req)
    (putStrLn $ "Close200 \n")
    (f $ responseLBS status200 [(hContentType, "text/plain")] "status200\n")

checkPath :: Application -> Application
checkPath nextApp req f =
  bracket_
    (putStrLn $ show "Open CheckPath" ++ show req)
    (putStrLn $ show "Close CheckPath")
    ( do
        case rawPathInfo req of
          "/" -> nextApp req f
          path | BC.take 6 path == "/echo/" -> appBody req f
          path | BC.take 11 path == "/user-agent" -> appUserAgent req f
          path | BC.take 7 path == "/files/" -> do
            case requestMethod req of
              "GET" -> appFile req f
              "POST" -> appFileSave req f
          _ -> app404 req f
    )

app404 :: Application
app404 _req f =
  bracket_
    (putStrLn $ show "Open404")
    -- (putStrLn $ "Close404 \n" ++ show _req)
    (putStrLn $ "Close404 \n")
    (f $ responseLBS notFound404 [] "status404\n")

appFileSave :: Application
appFileSave req f =
  bracket_
    (putStrLn $ show "OpenFileSave")
    -- (putStrLn $  "CloseFileSave \n")
    (putStrLn $ "CloseFile \n" ++ show req)
    ( do
        fileDirectory <- isDirectory <$> getArgs
        let fileName = (maybe "" id fileDirectory) <> drop (length ("/files/" :: String)) (BC.unpack (rawPathInfo req))
        -- let fileName2 = "/home/w/projects/warp/sh/README.me"
        -- let fileName2 = "/home/h/projects/webServer/sh/README.me"
        exist <- doesFileExist fileName
        case exist of
          True -> (do print exist; print fileName; app404 req f)
          False -> do
            body <- getRequestBodyChunk req
            BC.writeFile fileName body
            (f $ responseLBS status201 [(hContentType, "application/json"), (hLocation, BC.pack fileName)] "status201\n")
    )

appFile :: Application
appFile req f =
  bracket_
    (putStrLn $ show "OpenFile")
    (putStrLn $ "CloseFile \n")
    ( do
        fileDirectory <- isDirectory <$> getArgs
        let fileName = (maybe "" id fileDirectory) <> drop (length ("/files/" :: String)) (BC.unpack (rawPathInfo req))
        -- let fileName2 = "/home/w/projects/warp/sh/README.me"
        exist <- doesFileExist fileName
        case exist of
          False -> app404 req f
          True -> do
            body <- LBC.pack <$> readFile fileName
            let bodyLen = (+ 2) $ LBC.length body -- for "\r\n" in the end
            (f $ responseLBS status200 [(hContentType, "application/octet-stream"), (hContentLength, (BC.pack $ show bodyLen))] (body <> "\r\n"))
    )

appBody :: Application
appBody req f =
  bracket_
    (putStrLn $ show "OpenBody")
    (putStrLn $ "CloseBody \n")
    -- (putStrLn $  "CloseBody \n" ++ show req)
    -- (f $ responseLBS status200 [(hContentType, "text/plain"), (hContentLength, (BC.pack $ show bodyLen))] (body <> "\r\n") )
    (f $ responseLBS status200 [(hContentType, "text/plain"), (hContentLength, (BC.pack $ show bodyLen))] (body))
  where
    body = (1 + 2) `seq` (B.fromStrict $ BC.drop 6 $ (<> "\n") $ rawPathInfo req) -- <> "\r\n"
    -- bodyLen = (+2) $ LBC.length body
    bodyLen = LBC.length body

appUserAgent :: Application
appUserAgent req f =
  bracket_
    (putStrLn $ show "OpenUserAgent")
    (putStrLn $ "CloseUserAgent \n")
    -- (putStrLn $  "CloseUserAgent \n" ++ show req)
    (f $ responseLBS status200 [(hContentType, "text/plain"), (hContentLength, (BC.pack $ show bodyLen))] (body))
  where
    -- body = (1+2) `seq` (B.fromStrict $ BC.drop 6 $ (<> "\n")$rawPathInfo req) -- <> "\r\n"
    body = (B.fromStrict $ maybe "" (<> "\n") $ requestHeaderUserAgent req)
    bodyLen = LBC.length body
