module Types.ActivationForm (ActivationForm, tokenInput, passwordInput, passwordConfirmationInput, handleInput) where

import Control.Lens (makeFields)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import Relude (Eq, Generic, Show, Text, mzero, (<$>), (<*>))
import Types.HandleInput (HandleInput)
import Types.PasswordInput (PasswordInput)

data ActivationForm = ActivationForm
  { _activationFormTokenInput :: !Text,
    _activationFormHandleInput :: !HandleInput,
    _activationFormPasswordInput :: !PasswordInput,
    _activationFormPasswordConfirmationInput :: !PasswordInput
  }
  deriving (Show, Eq, Generic)

makeFields ''ActivationForm

instance FromJSON ActivationForm where
  parseJSON (Object v) =
    ActivationForm
      <$> v .: "token"
      <*> v .: "handle"
      <*> v .: "password"
      <*> v .: "password_confirmation"
  parseJSON _ = mzero
