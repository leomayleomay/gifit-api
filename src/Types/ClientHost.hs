module Types.ClientHost where

import Relude (Eq, Show, String)

newtype ClientHost = ClientHost
  { unClientHost :: String
  }
  deriving (Eq, Show)