module Controllers.Registration where

import Control.Lens ((^.))
import qualified Effects.Random as Random
import qualified Models.User as UserRepo
import Polysemy (Embed, Members, Sem, embed, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Relude
  ( Either (..),
    IO,
    Maybe (..),
    Text,
    decodeUtf8,
    return,
    ($),
    (>>=),
  )
import Servant
  ( JSON,
    PlainText,
    Post,
    ReqBody,
    ServerT,
    (:>),
  )
import Servant.Auth.Server (JWTSettings, makeJWT)
import qualified Services.Password as PasswordService
import Types.AppError (AppError (..))
import Types.CurrentUser (CurrentUser (..))
import Types.SignupForm (SignupForm, emailInput)
import Types.User (id)

data Algebra m a where
  Create :: SignupForm -> Algebra m Text

makeSem ''Algebra

run ::
  Members '[Random.Algebra, PasswordService.Algebra, UserRepo.Algebra, Error AppError, Embed IO] r =>
  JWTSettings ->
  Sem (Algebra ': r) a ->
  Sem r a
run jwtSettings =
  interpret $ \case
    Create form ->
      UserRepo.findByEmail (form ^. emailInput) >>= \case
        Just _ -> throw $ BadRequest "The user has signed up"
        Nothing -> signupUser jwtSettings form
{-# INLINE run #-}

signupUser ::
  Members '[UserRepo.Algebra, Error AppError, Embed IO] r =>
  JWTSettings ->
  SignupForm ->
  Sem r Text
signupUser jwtSettings form =
  UserRepo.create form >>= \case
    Just user -> do
      token <- embed $ makeJWT (CurrentUser (user ^. id)) jwtSettings Nothing
      case token of
        Left _err -> throw Unauthorized
        Right token' -> return $ decodeUtf8 token'
    Nothing -> throw $ BadRequest "Failed to sign up user"

type SignupR =
  "signup"
    :> ReqBody '[JSON] SignupForm
    :> Post '[PlainText] Text

type API =
  SignupR

api ::
  Members '[Algebra] r =>
  ServerT API (Sem r)
api = create