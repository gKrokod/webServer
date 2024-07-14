module Base.Crypto (makeHashPassword, validPassword) where

import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA1, Parameters(..))
import qualified Data.Text.Encoding as  E
import  qualified Data.Text as T
import Data.Time (UTCTime)
import Data.ByteString.Base64 as B64

type Salt = T.Text
type Hash = T.Text
type Password = T.Text

makeHashPassword :: Password -> UTCTime -> Hash
makeHashPassword pass time = 
  let salt = timeToSalt time
  in  salt <> makeHash pass salt






validPassword :: Password -> Hash -> Bool
validPassword pass hash_ = (==) hash_ (salt <> makeHash pass salt)
  where salt = T.take 16 hash_

makeHash :: Password -> Salt -> Hash 
makeHash txt salt = E.decodeUtf8 $ B64.encodeBase64' $ fastPBKDF2_SHA1 (Parameters 4000 16) txt' salt'
-- makeHash txt salt = E.decodeUtf16BE $ fastPBKDF2_SHA1 (Parameters 4000 16) txt' salt'
  where 
    txt' = E.encodeUtf8 txt
    salt' = E.encodeUtf8 salt 

timeToSalt :: UTCTime -> Salt
timeToSalt time = makeHash ( T.pack . show $ time) ""

