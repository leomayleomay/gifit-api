module Types.GiftState where

import Data.Aeson (ToJSON)
import Data.ByteString.Builder (byteString)
import qualified Data.ByteString.Char8 as B
import Database.PostgreSQL.Simple.FromField (FromField (..), ResultError (..), returnError)
import Database.PostgreSQL.Simple.ToField (Action (..), ToField (..))
import Relude
  ( Eq,
    Generic,
    Maybe (..),
    Show,
    return,
    (<$>),
  )

data GiftState
  = Created
  | Paid
  deriving (Show, Eq, Generic)

instance ToJSON GiftState

instance FromField GiftState where
  fromField f mdata =
    case B.unpack <$> mdata of
      Just "created" -> return Created
      Just "paid" -> return Paid
      Just str -> returnError ConversionFailed f str
      Nothing -> returnError UnexpectedNull f ""

instance ToField GiftState where
  toField Created = Plain (byteString "created")
  toField Paid = Plain (byteString "paid")