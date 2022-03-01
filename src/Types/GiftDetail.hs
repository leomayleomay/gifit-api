module Types.GiftDetail where

import Control.Lens (makeFields, (^.))
import Data.Aeson (ToJSON, object, toJSON, (.=))
import Data.Time (UTCTime)
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

data GiftDetail = GiftDetail
  { _giftDetailId :: Int,
    _giftDetailSenderHandle :: Text,
    _giftDetailReceiverHandle :: Text,
    _giftDetailAmount :: Int,
    _giftDetailState :: GiftState,
    _giftDetailNotes :: Maybe Text,
    _giftDetailSentAt :: UTCTime
  }
  deriving (Show, Eq, Generic)

makeFields ''GiftDetail

instance ToJSON GiftDetail where
  toJSON g =
    object
      [ "state" .= (g ^. state),
        "notes" .= (g ^. notes),
        "amount" .= (g ^. amount),
        "sent_at" .= (g ^. sentAt),
        "sender" .= (g ^. senderHandle),
        "receiver" .= (g ^. receiverHandle)
      ]

instance FromRow GiftDetail