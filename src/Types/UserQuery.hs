module Types.UserQuery (UserQuery (..)) where

import qualified Refined as R
import Relude (Either (..), Eq, Show, encodeUtf8, show, ($))
import qualified Text.Email.Validate as Email
import Types.EmailInput (EmailInput)
import Types.HandleInput (HandleInput)
import Web.HttpApiData (FromHttpApiData (..))

data UserQuery
  = UserEmail EmailInput
  | UserHandle HandleInput
  deriving (Show, Eq)

instance FromHttpApiData UserQuery where
  parseQueryParam input =
    if Email.isValid $ encodeUtf8 input
      then case R.refine input of
        Left err -> Left $ show err
        Right email -> Right $ UserEmail email
      else case R.refine input of
        Left err -> Left $ show err
        Right handle -> Right $ UserHandle handle