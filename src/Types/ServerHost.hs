module Types.ServerHost where

import Relude (Eq, Show, String)

newtype ServerHost = ServerHost
  { unServerHost :: String
  }
  deriving (Eq, Show)