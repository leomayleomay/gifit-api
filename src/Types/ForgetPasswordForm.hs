module Types.ForgetPasswordForm (ForgetPasswordForm, emailInput) where

import Control.Lens (makeFields)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:))
import Relude (Eq, Generic, Show, mzero, (<$>))
import Types.EmailInput (EmailInput)

data ForgetPasswordForm = ForgetPasswordForm
  { _forgetPasswordFormEmailInput :: !EmailInput
  }
  deriving (Show, Eq, Generic)

makeFields ''ForgetPasswordForm

instance FromJSON ForgetPasswordForm where
  parseJSON (Object v) = ForgetPasswordForm <$> v .: "email"
  parseJSON _ = mzero