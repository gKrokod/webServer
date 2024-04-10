{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Users where 

import qualified Data.Text as T
import Data.Aeson (eitherDecode, encode, ToJSON(..), FromJSON (..), eitherDecodeStrict)
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Lazy as L 
import qualified Data.ByteString as B 

data User = MkUser
  { name :: T.Text
  , login :: T.Text
  , password :: T.Text
  , isAdmin :: Bool
  , isPublisher :: Bool  
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- data ConfigDB = ConfigDB {
--     cHost :: T.Text
--   , cPort :: T.Text
--   , cUser :: T.Text
--   , cDBname :: T.Text
--   , cPassword :: T.Text
-- } deriving stock (Show, Generic)
--   deriving anyclass (ToJSON, FromJSON)
--
-- --for work 
-- loadConfigDB :: IO (Either String ConfigDB)
-- loadConfigDB =  eitherDecode <$> L.readFile "config/db.cfg"
--
-- -- for testing
-- createConfigFile :: IO ()
-- createConfigFile = do
--   let testConfig = ConfigDB {
--       cHost = ""
--     , cPort = ""
--     , cUser = ""
--     , cDBname = ""
--     , cPassword = ""
--   } 
--   let configToJSON = encode testConfig :: BC.ByteString
--   L.writeFile "config/db.cfg" (configToJSON)
--
createUser :: IO ()
createUser = do
  let testUser = MkUser {
      name = "Piteraaaaaa"
    , login = "Dager"
    , password = "qwerty"
    , isAdmin = True
    , isPublisher = True
  } 
  let userToJSON = encode testUser :: BC.ByteString
  L.writeFile "sh/user1.cfg" (userToJSON)
--
-- --for work 
loadUser1 :: IO (Either String User)
-- loadUser1 =  eitherDecode <$> L.readFile "sh/user1.cfg"
loadUser1 =  eitherDecodeStrict <$> B.readFile "sh/user1.cfg"

loadUser2 :: IO (Either String User)
loadUser2 =  eitherDecode <$> L.readFile "sh/user2.cfg"

createUser2 :: IO ()
createUser2 = do
  let testUser = MkUser {
      name = "Roza"
    , login = "rozo4ja"
    , password = "flower"
    , isAdmin = True
    , isPublisher = True
  } 
  let userToJSON = encode testUser :: BC.ByteString
  L.writeFile "sh/user2.cfg" (userToJSON)
