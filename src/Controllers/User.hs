module Controllers.User where

import Control.Lens ((^.))
import qualified Models.User as UserRepo
import Polysemy (Members, Sem, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Relude
  ( Maybe (..),
    maybeToList,
    return,
    when,
    ($),
    (/=),
    (<$>),
    (>>),
    (>>=),
  )
import Servant
  ( Get,
    JSON,
    NoContent (..),
    Optional,
    Post,
    QueryParam',
    ReqBody,
    ServerT,
    (:<|>) ((:<|>)),
    (:>),
  )
import Servant.Auth.Server (Auth, AuthResult (..), JWT)
import Types.ActivationForm
  ( ActivationForm,
    handleInput,
    passwordConfirmationInput,
    passwordInput,
    tokenInput,
  )
import Types.AppError (AppError (BadRequest, Unauthorized))
import Types.CurrentUser (CurrentUser (..))
import Types.User (User)
import Types.UserQuery (UserQuery (..))

data Algebra m a where
  Search :: AuthResult CurrentUser -> Maybe UserQuery -> Algebra m [User]
  Activate :: ActivationForm -> Algebra m NoContent

makeSem ''Algebra

run ::
  Members '[UserRepo.Algebra, Error AppError] r =>
  Sem (Algebra ': r) a ->
  Sem r a
run =
  interpret $ \case
    Search (Authenticated (CurrentUser sender)) query ->
      case query of
        Just query' -> case query' of
          UserEmail email -> maybeToList <$> UserRepo.findByEmail email
          UserHandle handle -> UserRepo.findAllByHandle handle
        Nothing -> UserRepo.findAllBySender sender
    Search _ _ -> throw Unauthorized
    Activate form -> do
      when (form ^. passwordInput /= form ^. passwordConfirmationInput) $ throw $ BadRequest "Password mismatches"
      UserRepo.findByResetPasswordToken (form ^. tokenInput) >>= \case
        Just user ->
          UserRepo.activate user (form ^. handleInput) (form ^. passwordInput)
            >> return NoContent
        Nothing -> throw $ BadRequest "Invalid token"
{-# INLINE run #-}

type SearchUsersR =
  Auth '[JWT] CurrentUser
    :> "users"
    :> QueryParam' '[Optional] "q" UserQuery
    :> Get '[JSON] [User]

type ActivateUserR =
  "activate"
    :> ReqBody '[JSON] ActivationForm
    :> Post '[JSON] NoContent

type API =
  SearchUsersR :<|> ActivateUserR

api ::
  Members '[Algebra] r =>
  ServerT API (Sem r)
api = search :<|> activate