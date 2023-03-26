module Handlers.Base where

data Handle m = Handle
  { 
    updateUser :: User -> m ()
  , createUser :: User -> m (Key User)
  , deleteUser :: User -> m ()
  , readUser :: Key User -> m (Maybe User)
  }

data Key a
data User
   
