module Base.MVar where
--simple database for debug, base on MVar
import Users
import Images
import Control.Concurrent (MVar, newMVar, putMVar, takeMVar)
import qualified Data.Map.Strict as Map

newtype UserDataBase = UserDataBase (MVar Users)

newBaseUser :: IO UserDataBase
newBaseUser = do
  m <- newMVar ([user1,user2,user3,user4])
  return $ UserDataBase m
            
updateUser :: UserDataBase -> User -> IO ()
updateUser (UserDataBase m) user = do
  base <- takeMVar m
  let base' = (user : base)
  putMVar m base'
  seq base' (return ())

takeUsers :: UserDataBase -> IO Users 
takeUsers (UserDataBase m) = do
  base <- takeMVar m
  putMVar m base
  return base

type ImageDB = Map.Map Int Image
newtype ImageDataBase = ImageDataBase (MVar ImageDB)

newBaseImage :: IO ImageDataBase
newBaseImage = do
  m <- newMVar (Map.singleton 1 testImage)
  return $ ImageDataBase m
            
findImage :: ImageDataBase -> Int -> IO (Maybe Image)
findImage (ImageDataBase m) k = do
  base <- takeMVar m
  putMVar m base
  return $ (Map.lookup k base)
