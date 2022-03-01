module Models.User where

import Control.Lens ((^.))
import qualified Effects.Database as Database
import qualified Effects.Random as Random
import Polysemy (Members, Sem, interpret, makeSem)
import Polysemy.Error (Error, throw)
import qualified Refined as R
import Relude (Either (..), Int, Maybe (..), Text, return, show, ($), (>>=))
import qualified Services.Password as PasswordService
import Types.AppError (AppError (BadRequest))
import Types.EmailInput (EmailInput)
import Types.HandleInput (HandleInput)
import Types.PasswordInput (PasswordInput)
import Types.SignupForm (SignupForm, emailInput, handleInput, passwordInput)
import Types.User (User, id)

data Algebra m a where
  Create :: SignupForm -> Algebra m (Maybe User)
  FindById :: Int -> Algebra m (Maybe User)
  FindAllBySender :: Int -> Algebra m [User]
  FindByEmail :: EmailInput -> Algebra m (Maybe User)
  FindOrCreateByEmail :: Text -> Algebra m (Maybe User)
  FindAllByHandle :: HandleInput -> Algebra m [User]
  FindByResetPasswordToken :: Text -> Algebra m (Maybe User)
  UpdatePasswordResetToken :: User -> Text -> Algebra m (Maybe User)
  UpdatePassword :: User -> PasswordInput -> Algebra m (Maybe User)
  Activate :: User -> HandleInput -> PasswordInput -> Algebra m (Maybe User)

makeSem ''Algebra

run ::
  Members '[Database.Algebra, Random.Algebra, PasswordService.Algebra, Error AppError] r =>
  Sem (Algebra ': r) a ->
  Sem r a
run =
  interpret $ \case
    Create form -> do
      hashed <- PasswordService.hashPassword (form ^. passwordInput)
      Database.fetch1
        "insert into users (email, handle, password) values (?, ?, ?) returning *"
        (R.unrefine $ form ^. emailInput, R.unrefine $ form ^. handleInput, hashed)
    FindOrCreateByEmail email ->
      Database.fetch1 "select * from users where email=?" [email] >>= \case
        Just user' -> return $ Just user'
        Nothing -> do
          uuid <- Random.nextUUID
          case R.refine uuid of
            Left err -> throw $ BadRequest (show err)
            Right password -> do
              hashed <- PasswordService.hashPassword password
              Database.fetch1
                "insert into users (email, handle, password, reset_password_token) values (?, ?, ?, ?) returning *"
                (email, email, hashed, uuid)
    FindById id' ->
      Database.fetch1 "select * from users where id=?" [id']
    FindAllBySender sender ->
      Database.fetchN "select * from users inner join gifts on gifts.receiver = users.id where gifts.sender=?" [sender]
    FindByEmail email ->
      Database.fetch1 "select * from users where email=?" [R.unrefine email]
    FindAllByHandle handle ->
      Database.fetchN "select * from users where handle=?" [R.unrefine handle]
    FindByResetPasswordToken token ->
      Database.fetch1 "select * from users where reset_password_token=?" [token]
    UpdatePasswordResetToken user token ->
      Database.fetch1
        "update users set reset_password_token=? where id=? returning *"
        (token, user ^. id)
    UpdatePassword user newPassword -> do
      hashed <- PasswordService.hashPassword newPassword
      Database.fetch1
        "update users set password=?, reset_password_token=null where id=? returning *"
        (hashed, user ^. id)
    Activate user newHandle newPassword -> do
      hashed <- PasswordService.hashPassword newPassword
      Database.fetch1
        "update users set handle=?, password=?, reset_password_token=null where id=? returning *"
        (R.unrefine newHandle, hashed, user ^. id)
{-# INLINE run #-}