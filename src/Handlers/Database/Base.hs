module Handlers.Database.Base (Success (..), Offset (..), Limit (..)) where

data Success = Put | Change | Get deriving (Show, Eq)

newtype Offset = MkOffset {getOffset :: Int}

newtype Limit = MkLimit {getLimit :: Int}
