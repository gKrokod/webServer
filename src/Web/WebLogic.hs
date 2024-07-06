module Web.WebLogic where
import Network.Wai (getRequestBodyChunk, responseBuilder)
import Network.Wai (Request, Response)
import Network.HTTP.Types (notFound404, status200, status201, Status, ResponseHeaders)

import qualified Data.ByteString as B 

getBody :: Request -> IO (B.ByteString)
getBody = getRequestBodyChunk

response404 :: Response
response404 = responseBuilder notFound404 [] "Not ok. status 404\n" 

response200 :: Response
response200 = responseBuilder status200 [] "All ok. status 200\n" 

