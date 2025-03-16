{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.Image.GetSpec (spec) where

import Handlers.Web.Image.Get (existingImages)
import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder (fromByteString)
import Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as E
import Database.Data.FillTables (image1, image2, image3)

import Network.HTTP.Types (hContentType, status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (Image (..))
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)
import Types (NumberImage (..))

import qualified Handlers.Web.Image
import qualified Handlers.Database.Image
import qualified Handlers.Logger
import qualified Web.Utils as WU

spec :: Spec
spec = do
  let req = defaultRequest
      req' = req {rawPathInfo = "/images", queryString = [("id", Just "1")]}
      imagesInBase = [image1, image2, image3]
      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
            }
      baseImageHandle =
        Handlers.Database.Image.Handle
          { Handlers.Database.Image.logger = logHandle,
            Handlers.Database.Image.pullImage = \(MkNumberImage num) -> get >>= pure . Right . Just . flip (!!) (fromIntegral num)
            }
      imageHandle = Handlers.Web.Image.Handle {
          Handlers.Web.Image.logger = logHandle,
          Handlers.Web.Image.base = baseImageHandle,
          Handlers.Web.Image.response404 = WU.response404,
          Handlers.Web.Image.response400 = WU.response400,
          Handlers.Web.Image.response500 = WU.response500,
          Handlers.Web.Image.mkGoodResponse = WU.mkGoodResponse,
          Handlers.Web.Image.mkResponseForImage = testImage
        } ::
          Handlers.Web.Image.Handle (State [Image])

  it "Can get an image by the ID" $ do
    evalState (existingImages imageHandle req') imagesInBase
      `shouldBe` testImage (imagesInBase !! 1)

    let req'' = req {queryString = [("id", Just "2")]}
    evalState (existingImages imageHandle req'') imagesInBase
      `shouldNotBe` testImage (imagesInBase !! 1)
    
testImage :: Image -> Response
testImage (Image header base) = responseBuilder status200 [(hContentType, contentType)] content
  where
    contentType = E.encodeUtf8 header
    content = fromByteString . B64.decodeBase64Lenient . E.encodeUtf8 $ base

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined
