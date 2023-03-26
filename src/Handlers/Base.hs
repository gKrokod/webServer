module Handlers.Base where

data Handle m = Handle
  { 
  , updateUser :: User -> m ()
  , createUser :: User -> m ()
  , deleteUser :: User -> m ()
  , readUser :: Key -> m (Maybe User)
  }

data Key
   
