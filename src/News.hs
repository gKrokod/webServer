{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}
module News where 
import qualified Data.Text as T
-- import qualified Data.Text.IO as TIO
-- import qualified Data.Text.Encoding as E
import Data.Aeson --(eitherDecode, encode, ToJSON(..), FromJSON (..))
import Data.Aeson.Types
import GHC.Generics (Generic)
-- -- import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Lazy as L 
-- import qualified Data.ByteString as B 
-- import Data.ByteString.Base64
import Images
import Data.Function (on)

data News = MkNews
  { title :: T.Text
  , dataCreated :: T.Text
  , userName :: T.Text 
  , category :: T.Text
  , textBody :: T.Text
  , imageBody :: [Image]  
  , isPublish :: Bool  
  } deriving stock (Show, Generic)
    deriving anyclass (ToJSON, FromJSON)
--
newtype BoxNews = BoxNews { unBoxNews :: News}
   deriving stock (Show, Generic)
   -- deriving anyclass (ToJSON, FromJSON)


type OldNews = News
mergeNews :: OldNews -> News -> News
mergeNews n' n = MkNews { title = (helper `on` title) n' n,
                          dataCreated = (helper `on` dataCreated) n' n,
                          userName = (helper `on` userName) n' n,
                          category = (helper `on` category) n' n,
                          textBody = (helper `on` textBody) n' n,
                          isPublish = isPublish n' && isPublish n, -- tyt vopros
                          imageBody = if null (imageBody n) then ([]) else (imageBody n)
                        }
  where helper :: T.Text -> T.Text -> T.Text
        helper o "" = o
        helper o n = n

instance FromJSON BoxNews where -- for edit news
  parseJSON (Object v) = do
    pTitle <- v .:? "title" .!= ""
    pDataCreated <- v .:? "dataCreated" .!= ""
    pUserName <- v .:? "userName" .!= ""
    pCategory <- v .:? "category" .!= ""
    pTextBody <- v .:? "textBody" .!= ""
    pIsPublish <- v .:? "isPublish" .!= False
    pImageBody <- v .:? "imageBody" .!= [] -- remake
    return BoxNews {unBoxNews = MkNews { title = pTitle, dataCreated=pDataCreated, userName=pUserName, category=pCategory, textBody=pTextBody, imageBody=pImageBody, isPublish=pIsPublish }}
  parseJSON invalid = prependFailure "parsing BoxNews failed, " (typeMismatch "Object" invalid)
--
createNews1 :: IO ()
createNews1 = do
  let testNews = MkNews {
        title = "GOod MOrning!",
        dataCreated = "17.04.2024",
        userName = "Петр",
        category = "Warrior",
        isPublish = True,
        textBody = "Warrior Peter drink water",
        imageBody = [testImage]  
  } 
  let userToJSON = encode testNews :: L.ByteString
  L.writeFile "config/news1.cfg" (userToJSON)
-- --
loadNews :: IO (Either String News)
loadNews =  eitherDecode <$> L.readFile "config/news1.cfg"
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
-- data Image = MkImage 
--   { iId :: Int
--   , iHeader :: T.Text
--   , iBase64 :: T.Text
--   } deriving stock (Show, Generic)
--     deriving anyclass (ToJSON, FromJSON)
