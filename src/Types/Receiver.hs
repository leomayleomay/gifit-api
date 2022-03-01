module Types.Receiver (Receiver (..)) where

import Data.Aeson (FromJSON, Value (..), parseJSON)
import qualified Data.Scientific as Scientific
import Relude (Eq, Generic, Int, Maybe (..), Show, Text, encodeUtf8, mzero, return, ($))
import qualified Text.Email.Validate as Email

data Receiver
  = ReceiverId Int
  | ReceiverEmail Text
  deriving (Show, Eq, Generic)

instance FromJSON Receiver where
  parseJSON (String v)
    | Email.isValid $ encodeUtf8 v =
      return $ ReceiverEmail v
  parseJSON (Number v)
    | Scientific.isInteger v =
      case Scientific.toBoundedInteger v of
        Just v' -> return $ ReceiverId v'
        Nothing -> mzero
  parseJSON _ = mzero
