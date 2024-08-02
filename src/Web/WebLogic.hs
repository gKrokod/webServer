module Web.WebLogic (getBody, response404, response200, mkGoodResponse, mkResponseForImage, response404WithImage) where

import Base.TestImage (image404)
import Data.Binary.Builder (Builder, fromByteString)
import Data.ByteString (ByteString)
import Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as E
import Network.HTTP.Types (notFound404, status200)
import Network.HTTP.Types.Header (hContentType)
import Network.Wai (Request, Response, getRequestBodyChunk, responseBuilder)
import Scheme (Image (..))

getBody :: Request -> IO ByteString
getBody = getRequestBodyChunk

response404 :: Response
response404 = responseBuilder notFound404 [] "Not ok. status 404\n"

response200 :: Response
response200 = responseBuilder status200 [] "All ok. status 200\n"

mkGoodResponse :: Builder -> Response
mkGoodResponse = responseBuilder status200 []

mkResponseForImage :: Image -> Response
mkResponseForImage (Image header base) = responseBuilder status200 [(hContentType, contentType)] content
  where
    contentType = E.encodeUtf8 header
    content = fromByteString . B64.decodeBase64Lenient . E.encodeUtf8 $ base

response404WithImage :: Response
response404WithImage = responseBuilder notFound404 [(hContentType, contentType)] content
  where
    Image header base = image404
    contentType = E.encodeUtf8 header
    content = fromByteString . B64.decodeBase64Lenient . E.encodeUtf8 $ base
