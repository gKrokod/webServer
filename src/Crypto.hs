module Crypto

-- import Language.Haskell.TH -- (runIO, Q, Exp(LitE), Lit(StringL))
--
import Crypto.KDF.PBKDF2
import Data.ByteString (ByteString, putStr)
import qualified Data.ByteString as B
import qualified Data.Text.Encoding as  E
-- import  qualified Data.Text.IO as TIO
import  qualified Data.Text as T
import qualified Crypto.KDF.BCrypt as V
import Data.Time
import Data.ByteString.Base64 as B64

password1 :: T.Text
password1 = "parol sloznuj!"

type Salt = T.Text
type Hash = T.Text
type Password = T.Text

getSalt :: IO Salt 
getSalt = do
  t <- getCurrentTime
  let salt = makeHash ( T.pack . show $ t) ""
  pure salt 

makeHash :: Password -> Salt -> Hash 
makeHash txt salt = E.decodeUtf8 $ B64.encodeBase64' $ fastPBKDF2_SHA1 (Parameters 4000 16) txt' salt'
-- makeHash txt salt = E.decodeUtf8 $ B64.encodeBase64' $ fastPBKDF2_SHA1 (Parameters 4000 16) txt' salt'
  where 
    txt' = E.encodeUtf8 txt
    salt' = E.encodeUtf8 salt 

makeNewPass :: Password -> IO (Hash , Salt)
makeNewPass pass = do
  salt <- getSalt
  let hashPass = salt <> makeHash pass salt
  pure (hashPass, salt)

validPassword :: Passowrd -> Salt -> Hash -> Bool
validPassword pass salt hash_ = (==) hash_ (salt <> makeHash pass salt)
