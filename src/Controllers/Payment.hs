module Controllers.Payment where

import qualified Models.Gift as GiftRepo
import Polysemy (Members, Sem, interpret, makeSem)
import Relude
  ( Maybe (..),
    Text,
    decodeUtf8,
    encodeUtf8,
    return,
    ($),
    (.),
    (<$>),
    (>>),
  )
import Relude.Unsafe (read)
import Servant
  ( NoContent (..),
    PlainText,
    Post,
    QueryParam',
    Required,
    ServerT,
    Strict,
    (:>),
  )
import qualified Services.Signature as SignatureService

data Algebra m a where
  Create :: Text -> Algebra m NoContent

makeSem ''Algebra

run ::
  Members '[GiftRepo.Algebra, SignatureService.Algebra] r =>
  Sem (Algebra ': r) a ->
  Sem r a
run =
  interpret $ \case
    Create sig -> do
      giftId <- read . decodeUtf8 <$> SignatureService.verify (encodeUtf8 sig)
      gift <- GiftRepo.findCreatedById giftId
      case gift of
        Nothing -> return NoContent
        Just gift' -> do
          GiftRepo.markAsPaid gift'
            >> return NoContent
{-# INLINE run #-}

type CreatePaymentR =
  "payments"
    :> QueryParam' '[Strict, Required] "s" Text
    :> Post '[PlainText] NoContent

type API =
  CreatePaymentR

api ::
  Members '[Algebra] r =>
  ServerT API (Sem r)
api = create