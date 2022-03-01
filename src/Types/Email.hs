module Types.Email where

import Data.Aeson (FromJSON, ToJSON)
import Database.PostgreSQL.Simple.FromField (FromField (..))
import Database.PostgreSQL.Simple.ToField (ToField (..))
import Relude (Eq, Read, Show, Text)

newtype Email
  = Email Text
  deriving (Show, Eq, Read, FromField, ToField, FromJSON, ToJSON)