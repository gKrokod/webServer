module Database.Crypto (makeHashPassword, validPassword) where

import Crypto.KDF.PBKDF2 (Parameters (..), fastPBKDF2_SHA256)
import Data.ByteString as B
import Data.ByteString.Base64 as B64
import Data.Function (on)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time (UTCTime)
import Types (PasswordUser (..))

type Salt = T.Text

type Hash = T.Text

type Password = T.Text

type SizeSaltAndPassword = Int

type NumberIterations = Int

makeHashPassword :: PasswordUser -> UTCTime -> Hash
makeHashPassword (MkPasswordUser pass) time =
  let salt = timeToSalt time
   in salt <> makeHash pass salt

validPassword :: Password -> Hash -> Bool
validPassword pass hash_ = (==) hash_ (salt <> makeHash pass salt)
  where
    salt = E.decodeUtf8 . B64.encodeBase64' . B.take sizeSalt . B64.decodeBase64Lenient . E.encodeUtf8 $ hash_

-----------------------Internal---------------------------
sizeSalt, sizePassword :: SizeSaltAndPassword
sizeSalt = 16
sizePassword = sizeSalt

iterations :: NumberIterations
iterations = 4000

makeHash :: Password -> Salt -> Hash
makeHash = ((E.decodeUtf8 . B64.encodeBase64') .) . fastPBKDF2_SHA256 (Parameters iterations sizePassword) `on` E.encodeUtf8

timeToSalt :: UTCTime -> Salt
timeToSalt = flip makeHash "" . (T.pack . show)
