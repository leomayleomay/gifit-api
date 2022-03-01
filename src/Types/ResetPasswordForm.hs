module Types.ResetPasswordForm (ResetPasswordForm, tokenInput, passwordInput, passwordConfirmationInput) where

import Control.Lens (makeFields)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Relude (Eq, Generic, Show, Text, (<$>), (<*>))
import Types.PasswordInput (PasswordInput)

data ResetPasswordForm = ResetPasswordForm
  { _resetPasswordFormTokenInput :: !Text,
    _resetPasswordFormPasswordInput :: !PasswordInput,
    _resetPasswordFormPasswordConfirmationInput :: !PasswordInput
  }
  deriving (Show, Eq, Generic)

makeFields ''ResetPasswordForm

instance FromJSON ResetPasswordForm where
  parseJSON (Object v) =
    ResetPasswordForm
      <$> v .: "token"
      <*> v .: "password"
      <*> v .: "password_confirmation"
  parseJSON invalid =
    prependFailure
      "parsing ResetPasswordForm failed, "
      (typeMismatch "Object" invalid)