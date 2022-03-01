module Types.CurrentUser (CurrentUser (..)) where

import Relude
import Data.Aeson (FromJSON, ToJSON)
import Servant.Auth.Server (FromJWT, ToJWT)

newtype CurrentUser
  = CurrentUser Int
  deriving (Eq, Show, Read, Generic)

instance FromJSON CurrentUser

instance ToJSON CurrentUser

instance FromJWT CurrentUser

instance ToJWT CurrentUser