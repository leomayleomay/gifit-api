module Services.Password where

import qualified Crypto.KDF.BCrypt as CKB
import qualified Data.ByteString.Char8 as B
import Data.Text (unpack)
import Polysemy (Embed, Members, Sem, embed, interpret, makeSem)
import qualified Refined as R
import Relude (Bool, IO, return, ($))
import Types.HashedPassword (HashedPassword (..))
import Types.PasswordInput (PasswordInput)

data Algebra m a where
  ValidatePassword :: PasswordInput -> HashedPassword -> Algebra m Bool
  HashPassword :: PasswordInput -> Algebra m HashedPassword

makeSem ''Algebra

run ::
  Members '[Embed IO] r =>
  Sem (Algebra ': r) a ->
  Sem r a
run = interpret $ \case
  ValidatePassword p (HashedPassword h) ->
    return $ CKB.validatePassword (B.pack $ unpack (R.unrefine p)) h
  HashPassword p -> do
    hash <- embed $ CKB.hashPassword 12 (B.pack $ unpack (R.unrefine p))
    return $ HashedPassword hash
{-# INLINE run #-}