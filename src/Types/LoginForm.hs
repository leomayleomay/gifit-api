module Types.LoginForm (LoginForm, emailInput, passwordInput) where

import Control.Lens (makeFields)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Relude (Eq, Generic, Show, (<$>), (<*>))
import Types.EmailInput (EmailInput)
import Types.PasswordInput (PasswordInput)

data LoginForm = LoginForm
  { _loginFormEmailInput :: !EmailInput,
    _loginFormPasswordInput :: !PasswordInput
  }
  deriving (Show, Eq, Generic)

makeFields ''LoginForm

instance FromJSON LoginForm where
  parseJSON (Object v) = LoginForm <$> v .: "email" <*> v .: "password"
  parseJSON invalid =
    prependFailure
      "parsing LoginForm failed, "
      (typeMismatch "Object" invalid)