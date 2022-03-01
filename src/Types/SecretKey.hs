module Types.SecretKey where

import Relude (ByteString, Eq)

newtype SecretKey = SecretKey
  { unSecretKey :: ByteString
  }
  deriving (Eq)