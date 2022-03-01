module Types.CreateGiftResponse (CreateGiftResponse (..), id, callbackUrl) where

import Control.Lens (makeFields, (^.))
import Data.Aeson (FromJSON, ToJSON, Value (..), object, parseJSON, toJSON, (.:), (.=))
import Relude (Eq, Generic, Int, Show, Text, mzero, (<$>), (<*>))

data CreateGiftResponse = CreateGiftResponse
  { _createGiftResponseId :: Int,
    _createGiftResponseAmount :: Int,
    _createGiftResponseCallbackUrl :: Text
  }
  deriving (Eq, Show, Generic)

makeFields ''CreateGiftResponse

instance FromJSON CreateGiftResponse where
  parseJSON (Object o) = CreateGiftResponse <$> o .: "id" <*> o .: "amount" <*> o .: "callback_url"
  parseJSON _ = mzero

instance ToJSON CreateGiftResponse where
  toJSON g = object ["id" .= (g ^. id), "amount" .= (g ^. amount), "callback_url" .= (g ^. callbackUrl)]