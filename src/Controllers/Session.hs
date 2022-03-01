module Controllers.Session where

import Control.Lens ((^.))
import qualified Models.User as UserRepo
import Polysemy (Embed, Members, Sem, embed, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Relude
  ( Either (..),
    IO,
    Int,
    Maybe (..),
    Text,
    decodeUtf8,
    return,
    unless,
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
import Types.LoginForm (LoginForm, emailInput, passwordInput)
import Types.User (id, password)

data Algebra m a where
  Create :: LoginForm -> Algebra m Text

makeSem ''Algebra

run ::
  Members '[PasswordService.Algebra, UserRepo.Algebra, Error AppError, Embed IO] r =>
  JWTSettings ->
  Sem (Algebra ': r) a ->
  Sem r a
run jwtSettings =
  interpret $ \case
    Create form ->
      UserRepo.findByEmail (form ^. emailInput) >>= \case
        Just user -> do
          validated <- PasswordService.validatePassword (form ^. passwordInput) (user ^. password)
          unless validated $ throw Unauthorized
          loginUser jwtSettings (user ^. id)
        _ -> throw Unauthorized
{-# INLINE run #-}

loginUser ::
  Members '[Error AppError, Embed IO] r =>
  JWTSettings ->
  Int ->
  Sem r Text
loginUser jwtSettings userId = do
  token <- embed $ makeJWT (CurrentUser userId) jwtSettings Nothing
  case token of
    Left _err -> throw Unauthorized
    Right token' -> return $ decodeUtf8 token'

type LoginR =
  "login"
    :> ReqBody '[JSON] LoginForm
    :> Post '[PlainText] Text

type API =
  LoginR

api ::
  Members '[Algebra] r =>
  ServerT API (Sem r)
api = create