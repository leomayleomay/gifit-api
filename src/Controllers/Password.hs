module Controllers.Password where

import Control.Lens ((^.))
import qualified Effects.Random as Random
import qualified Models.User as UserRepo
import Polysemy (Members, Sem, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Relude
  ( Maybe (Just, Nothing),
    return,
    when,
    ($),
    (/=),
    (>>),
    (>>=),
  )
import Servant
  ( JSON,
    NoContent (..),
    PlainText,
    Post,
    ReqBody,
    ServerT,
    (:<|>) ((:<|>)),
    (:>),
  )
import qualified Services.Email as EmailService
import Types.AppError (AppError (..))
import Types.ForgetPasswordForm (ForgetPasswordForm, emailInput)
import Types.ResetPasswordForm
  ( ResetPasswordForm,
    passwordConfirmationInput,
    passwordInput,
    tokenInput,
  )

data Algebra m a where
  Create :: ForgetPasswordForm -> Algebra m NoContent
  Update :: ResetPasswordForm -> Algebra m NoContent

makeSem ''Algebra

run ::
  Members '[EmailService.Algebra, UserRepo.Algebra, Random.Algebra, Error AppError] r =>
  Sem (Algebra ': r) a ->
  Sem r a
run =
  interpret $ \case
    Create form ->
      UserRepo.findByEmail (form ^. emailInput) >>= \case
        Just user -> do
          token <- Random.nextUUID
          UserRepo.updatePasswordResetToken user token >>= \case
            Just user' -> EmailService.notifyUserOfPasswordReset user' >> return NoContent
            Nothing -> throw $ BadRequest "Invalid token"
        Nothing -> throw $ BadRequest "Invalid Email"
    Update form -> do
      when (form ^. passwordInput /= form ^. passwordConfirmationInput) $ throw $ BadRequest "Password mismatches"
      UserRepo.findByResetPasswordToken (form ^. tokenInput) >>= \case
        Just user ->
          UserRepo.updatePassword user (form ^. passwordInput)
            >> return NoContent
        Nothing -> throw $ BadRequest "Invalid token"
{-# INLINE run #-}

type ForgetPasswordR =
  "forget_password"
    :> ReqBody '[JSON] ForgetPasswordForm
    :> Post '[PlainText] NoContent

type ResetPasswordR =
  "reset_password"
    :> ReqBody '[JSON] ResetPasswordForm
    :> Post '[PlainText] NoContent

type API =
  ForgetPasswordR :<|> ResetPasswordR

api ::
  Members '[Algebra] r =>
  ServerT API (Sem r)
api = create :<|> update