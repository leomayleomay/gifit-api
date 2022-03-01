module Controllers.Gift where

import Control.Lens ((^.))
import Data.String.Interpolate (i)
import qualified Models.Gift as GiftRepo
import qualified Models.User as UserRepo
import Polysemy (Members, Sem, interpret, makeSem)
import Polysemy.Error (Error (..), throw)
import Polysemy.Input (Input, input)
import Relude
  ( Maybe (..),
    return,
    show,
    ($),
    (>>),
    (>>=),
  )
import Servant
  ( Get,
    JSON,
    Post,
    QueryParam',
    ReqBody,
    Required,
    ServerT,
    Strict,
    (:<|>) ((:<|>)),
    (:>),
  )
import Servant.Auth.Server (Auth, AuthResult (..), JWT)
import qualified Services.Email as EmailService
import qualified Services.Signature as SignatureService
import Types.AppError (AppError (BadRequest, Unauthorized))
import Types.CreateGiftResponse (CreateGiftResponse (..))
import Types.CurrentUser (CurrentUser (..))
import Types.Direction (Direction (..))
import qualified Types.Gift as Gift
import Types.GiftForm (GiftForm)
import qualified Types.GiftForm as GiftForm
import Types.IndexGiftsResponse
import Types.Receiver (Receiver (..))
import Types.ServerHost
import qualified Types.User as User

data Algebra m a where
  Index :: AuthResult CurrentUser -> Direction -> Algebra m IndexGiftsResponse
  Create :: AuthResult CurrentUser -> GiftForm -> Algebra m CreateGiftResponse

makeSem ''Algebra

run ::
  Members '[SignatureService.Algebra, EmailService.Algebra, UserRepo.Algebra, GiftRepo.Algebra, Input ServerHost, Error AppError] r =>
  Sem (Algebra ': r) a ->
  Sem r a
run =
  interpret $ \case
    Index (Authenticated (CurrentUser user)) In ->
      GiftRepo.findAllByReceiver user
    Index (Authenticated (CurrentUser user)) Out ->
      GiftRepo.findAllBySender user
    Index _ _ -> throw Unauthorized
    Create (Authenticated (CurrentUser sender)) form -> do
      (ServerHost serverHost) <- input
      case form ^. GiftForm.receiver of
        ReceiverId id' ->
          UserRepo.findById id' >>= \case
            Nothing ->
              throw $ BadRequest "Failed to create gift because receiver doesn't exist"
            Just _ ->
              GiftRepo.create sender (form ^. GiftForm.amount) (form ^. GiftForm.notes) id'
                >>= \case
                  Nothing -> throw $ BadRequest "Failed to create gift"
                  Just g -> do
                    signature <- SignatureService.sign (show $ g ^. Gift.id)
                    return $ CreateGiftResponse (g ^. Gift.id) (g ^. Gift.amount) [i|#{serverHost}/payments?s=#{signature}|]
        ReceiverEmail email ->
          UserRepo.findOrCreateByEmail email >>= \case
            Nothing -> throw $ BadRequest "Failed to find or create receiver by email"
            Just tempUser ->
              EmailService.notifyUserToActivateAccount tempUser
                >> GiftRepo.create sender (form ^. GiftForm.amount) (form ^. GiftForm.notes) (tempUser ^. User.id)
                  >>= \case
                    Nothing -> throw $ BadRequest "Failed to create gift"
                    Just g -> do
                      signature <- SignatureService.sign (show $ g ^. Gift.id)
                      return $ CreateGiftResponse (g ^. Gift.id) (g ^. Gift.amount) [i|#{serverHost}/payments?s=#{signature}|]
    Create _ _ -> throw Unauthorized
{-# INLINE run #-}

type IndexGiftsR =
  Auth '[JWT] CurrentUser
    :> "gifts"
    :> QueryParam' '[Strict, Required] "dir" Direction
    :> Get '[JSON] IndexGiftsResponse

type CreateGiftR =
  Auth '[JWT] CurrentUser
    :> "gifts"
    :> ReqBody '[JSON] GiftForm
    :> Post '[JSON] CreateGiftResponse

type API =
  IndexGiftsR :<|> CreateGiftR

api ::
  Members '[Algebra] r =>
  ServerT API (Sem r)
api = index :<|> create