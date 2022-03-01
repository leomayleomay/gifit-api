module Types.GiftForm (GiftForm (..), amount, receiver, notes) where

import Control.Lens (makeFields)
import Data.Aeson (FromJSON, Value (..), parseJSON, (.:), (.:?))
import Relude (Eq, Generic, Maybe, Show, Text, mzero, (<$>), (<*>))
import Types.AmountInput (AmountInput)
import Types.Receiver (Receiver)

data GiftForm = GiftForm
  { _giftFormAmount :: !AmountInput,
    _giftFormReceiver :: !Receiver,
    _giftFormNotes :: !(Maybe Text)
  }
  deriving (Show, Eq, Generic)

makeFields ''GiftForm

instance FromJSON GiftForm where
  parseJSON (Object v) = GiftForm <$> v .: "amount" <*> v .: "receiver" <*> v .:? "notes"
  parseJSON _ = mzero
