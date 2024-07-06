module Web.WebLogic where
import Scheme 
import Network.Wai (getRequestBodyChunk, responseBuilder)
import Network.Wai (Request, Response)
import Network.HTTP.Types (notFound404, status200, status201, Status, ResponseHeaders)
import Network.HTTP.Types.Header (hContentType)
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as L 
import Data.Binary.Builder as BU (fromByteString, Builder, fromLazyByteString, putStringUtf8)

import Data.Aeson (eitherDecode, eitherDecodeStrict, encode, ToJSON)
import Data.ByteString.Base64 as B64
import qualified Data.Text.Encoding as E

getBody :: Request -> IO (B.ByteString)
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
    content = fromByteString $ B64.decodeBase64Lenient $ E.encodeUtf8 base 
