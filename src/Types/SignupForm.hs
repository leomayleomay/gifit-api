module Types.SignupForm (SignupForm, emailInput, handleInput, passwordInput) where

import Control.Lens (makeFields)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import Relude (Eq, Generic, Show, mzero, (<$>), (<*>))
import Types.EmailInput (EmailInput)
import Types.HandleInput (HandleInput)
import Types.PasswordInput (PasswordInput)

data SignupForm = SignupForm
  { _signupFormEmailInput :: !EmailInput,
    _signupFormHandleInput :: !HandleInput,
    _signupFormPasswordInput :: !PasswordInput
  }
  deriving (Show, Eq, Generic)

makeFields ''SignupForm

instance FromJSON SignupForm where
  parseJSON (Object v) = SignupForm <$> v .: "email" <*> v .: "handle" <*> v .: "password"
  parseJSON _ = mzero