module Types.User (User (..), id, email, handle, password, resetPasswordToken, createdAt, updatedAt) where

import Control.Lens (makeFields, (^.))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Relude
  ( Eq,
    Generic,
    Int,
    Maybe,
    Show,
    Text,
  )
import Types.Email (Email)
import Types.HashedPassword (HashedPassword)

data User = User
  { _userId :: Int,
    _userEmail :: Email,
    _userHandle :: Text,
    _userPassword :: HashedPassword,
    _userResetPasswordToken :: Maybe Text,
    _userCreatedAt :: UTCTime,
    _userUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

makeFields ''User

instance ToJSON User where
  toJSON u = object ["id" .= (u ^. id), "email" .= (u ^. email), "handle" .= (u ^. handle)]

instance FromRow User