module Types.EmailInput (EmailInput, ValidEmail) where

import Data.Typeable (typeRep)
import Refined
  ( Predicate (..),
    Refined,
    throwRefineOtherException,
  )
import Relude (Maybe (..), Text, encodeUtf8, ($))
import Text.Email.Validate (isValid)

data ValidEmail

instance Predicate ValidEmail Text where
  validate p value =
    if isValid $ encodeUtf8 value
      then Nothing
      else throwRefineOtherException (typeRep p) "Invalid email"

type EmailInput = Refined ValidEmail Text
