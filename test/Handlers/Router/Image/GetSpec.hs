{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.Image.GetSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (fromByteString)
import Data.ByteString.Base64 as B64
import Data.Proxy (Proxy (..))
import qualified Data.Text.Encoding as E
import Database.Data.FillTables (image1, image2, image3, user1test, user2test, user3test)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Router (doLogic)
import qualified Handlers.Web.Base as WB
import Network.HTTP.Types (hContentType, status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (Image (..), User (..))
import Test.Hspec (Spec, it, shouldBe)
import Types (Login (..), NumberImage (..))

spec :: Spec
spec = do
  it "123" $ do head [23,14] `shouldBe` (23 :: Int)
--   -- curl "127.0.0.1:4221/images?id=1" --output -
--   --- todo
--   let req = defaultRequest
--       req' = req {rawPathInfo = "/images", queryString = [("id", Just "1")]}
--       logHandle =
--         Handlers.Logger.Handle
--           { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
--             Handlers.Logger.writeLog = \_ -> pure ()
--           }
--
--       imagesInBase = [image1, image2, image3]
--
--       baseHandle =
--         DB.Handle
--           { DB.logger = logHandle,
--             DB.pullImage = \(MkNumberImage num) -> get >>= pure . Right . Just . flip (!!) (fromIntegral num)
--           }
--       webHandle =
--         WB.Handle
--           { WB.logger = logHandle,
--             WB.base = baseHandle,
--             WB.mkResponseForImage = testImage
--           } ::
--           WB.Handle (State [Image])
--
--   it "All clients can get a image" $ do
--     let baseHandle' = baseHandle
--         clientAdminUser1 = WB.Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)
--         clientAdminUser2 = WB.Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2test)
--         clientAdminUser3 = WB.Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3test)
--         clientAdminUser4 = WB.Client Nothing Nothing Nothing
--
--         webHandle1 =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser1
--             }
--         webHandle2 =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser2
--             }
--         webHandle3 =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser3
--             }
--         webHandle4 =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser4
--             }
--
--     evalState (doLogic webHandle1 req') imagesInBase
--       `shouldBe` testImage (imagesInBase !! 1)
--     evalState (doLogic webHandle2 req') imagesInBase
--       `shouldBe` testImage (imagesInBase !! 1)
--     evalState (doLogic webHandle3 req') imagesInBase
--       `shouldBe` testImage (imagesInBase !! 1)
--     evalState (doLogic webHandle4 req') imagesInBase
--       `shouldBe` testImage (imagesInBase !! 1)
--
-- testImage :: Image -> Response
-- testImage (Image header base) = responseBuilder status200 [(hContentType, contentType)] content
--   where
--     contentType = E.encodeUtf8 header
--     content = BU.fromByteString . B64.decodeBase64Lenient . E.encodeUtf8 $ base
--
-- instance Show Response where
--   show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
--   show _ = "Response"
--
-- instance Eq Response where
--   (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
--   (==) _ _ = undefined
