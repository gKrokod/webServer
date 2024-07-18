module Base.Crypto (makeHashPassword, validPassword) where

import Crypto.KDF.PBKDF2 (fastPBKDF2_SHA256, Parameters(..))
import qualified Data.Text.Encoding as  E
import  qualified Data.Text as T
import Data.Time (UTCTime)
import Data.ByteString.Base64 as B64
import Data.ByteString as B
import Data.Function (on)

type Salt = T.Text
type Hash = T.Text
type Password = T.Text
type SizeSaltAndPassword = Int
type NumberIterations = Int

makeHashPassword :: Password -> UTCTime -> Hash
makeHashPassword pass time = 
  let salt = timeToSalt time
  in  salt <> makeHash pass salt

validPassword :: Password -> Hash -> Bool
validPassword pass hash_ = (==) hash_ (salt <> makeHash pass salt)
  where salt = E.decodeUtf8 . B64.encodeBase64' . B.take sizeSalt . B64.decodeBase64Lenient . E.encodeUtf8 $ hash_
        
----------------------------------------------------
sizeSalt,sizePassword :: SizeSaltAndPassword
--todo add in config?
sizeSalt = 16
sizePassword = sizeSalt

iterations :: NumberIterations
--todo add in config?
iterations = 4000

makeHash :: Password -> Salt -> Hash 
makeHash = ((E.decodeUtf8 . B64.encodeBase64') . ) . fastPBKDF2_SHA256 (Parameters iterations sizePassword) `on` E.encodeUtf8

timeToSalt :: UTCTime -> Salt
timeToSalt = flip makeHash "" . (T.pack . show)

