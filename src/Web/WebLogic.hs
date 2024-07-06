module Web.WebLogic where
import Scheme 
import Network.Wai (getRequestBodyChunk, responseBuilder)
import Network.Wai (Request, Response)
import Network.HTTP.Types (notFound404, status200, status201, Status, ResponseHeaders)
import qualified Data.ByteString as B 
import qualified Data.ByteString.Lazy as L 
import Data.Binary.Builder as BU (fromByteString, Builder, fromLazyByteString, putStringUtf8)

import Data.Aeson (eitherDecode, eitherDecodeStrict, encode, ToJSON)

getBody :: Request -> IO (B.ByteString)
getBody = getRequestBodyChunk

response404 :: Response
response404 = responseBuilder notFound404 [] "Not ok. status 404\n" 

response200 :: Response
response200 = responseBuilder status200 [] "All ok. status 200\n" 

mkGoodResponse = undefined
-- mkGoodResponse :: Builder -> Response
-- mkGoodResponse xs = responseBuilder status200 [] ("All ok. User list:\n" <> xs)
--   where body = mkJSON xs
--
-- mkJSON :: (ToJSON a) =>  [a] -> L.ByteString
-- mkJSON xs = mconcat ["{\"answer\":[",L.intercalate "," (Prelude.map encode xs),"]}"]  -- let body = mkJSON users
--

userToBuilder :: User -> Builder
userToBuilder = undefined
  -- -- let body = mkJSON (Handlers.Base.bank baseHandle)
  -- pure $ responseBuilder status200 [] ("All ok. User list:\n" <> B.fromLazyByteString body)

