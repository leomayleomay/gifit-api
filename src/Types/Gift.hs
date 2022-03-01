module Types.Gift (Gift (..), id, sender, receiver, amount, state, notes, createdAt, updatedAt) where

import Control.Lens (makeFields)
import Data.Time.Clock (UTCTime)
import Database.PostgreSQL.Simple.FromRow (FromRow)
import Relude
  ( Eq,
    Generic,
    Int,
    Maybe,
    Show,
    Text,
  )
import Types.GiftState

data Gift = Gift
  { _giftId :: Int,
    _giftSender :: Int,
    _giftReceiver :: Int,
    _giftAmount :: Int,
    _giftState :: GiftState,
    _giftNotes :: Maybe Text,
    _giftCreatedAt :: UTCTime,
    _giftUpdatedAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

makeFields ''Gift

instance FromRow Gift