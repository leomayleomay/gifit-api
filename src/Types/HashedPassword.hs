module Types.HashedPassword (HashedPassword (..)) where

import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Relude (ByteString, Eq, Read, Show)

newtype HashedPassword
  = HashedPassword ByteString
  deriving (Show, Eq, Read, FromField, ToField)