{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module News where 

import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
-- import qualified Data.Text.Encoding as E
import Data.Aeson (eitherDecode, encode, ToJSON(..), FromJSON (..))
import GHC.Generics (Generic)
-- -- import qualified Data.ByteString.Lazy.Char8 as BC
-- import qualified Data.ByteString.Lazy as L 
-- import qualified Data.ByteString as B 
-- import Data.ByteString.Base64
--
-- type Users = [User] 
--
-- user1 = MkUser "Masha" "m" "1" False False
-- user2 = MkUser "Sasha" "s" "2" False True
-- user3 = MkUser "Pasha" "p" "3" True False
-- user4 = MkUser "Dasha" "d" "4" True True
--
--
-- data User = MkUser
--   { name :: T.Text
--   , login :: T.Text
--   , password :: T.Text
--   , isAdmin :: Bool
--   , isPublisher :: Bool  
--   } deriving stock (Show, Generic)
--     deriving anyclass (ToJSON, FromJSON)
--
-- data WebConfig = MkWebConfig
--   { limit :: Int
--   } deriving stock (Show, Generic)
--     deriving anyclass (ToJSON, FromJSON)

--
-- createConfig :: IO ()
-- createConfig = do
--   let testConfig = MkWebConfig {
--       limit = 3 -- paginate
--   } 
--   let webToJSON = encode testConfig :: L.ByteString
--   L.writeFile "config/web.cfg" (webToJSON)
--
-- createUser1 :: IO ()
-- createUser1 = do
--   let testUser = MkUser {
--       name = "Петр"
--     , login = "Дагер"
--     , password = "qwerty"
--     , isAdmin = True
--     , isPublisher = True
--   } 
--   let userToJSON = encode testUser :: L.ByteString
--   L.writeFile "sh/user1.cfg" (userToJSON)
-- --
-- loadWeb :: IO (Either String WebConfig)
-- loadWeb =  eitherDecode <$> L.readFile "config/web.cfg"
--
-- -- --for work 
-- loadUser1 :: IO (Either String User)
-- -- loadUser1 =  eitherDecode <$> L.readFile "sh/user1.cfg"
-- loadUser1 =  eitherDecode <$> L.readFile "sh/user1.cfg"
--
-- loadUser2 :: IO (Either String User)
-- loadUser2 =  eitherDecode <$> L.readFile "sh/user2.cfg"
--
-- createUser2 :: IO ()
-- createUser2 = do
--   let testUser = MkUser {
--       name = "Roza"
--     , login = "rozo4ja"
--     , password = "flower"
--     , isAdmin = True
--     , isPublisher = True
--   } 
--   let userToJSON = encode testUser :: L.ByteString
--   L.writeFile "sh/user2.cfg" (userToJSON)
--
