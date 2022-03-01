{-# LANGUAGE AllowAmbiguousTypes #-}

module Services.Signature where

import Crypto.Hash.Algorithms (SHA1)
import Crypto.MAC.HMAC (HMAC (hmacGetDigest), hmac)
import Data.ByteArray (convert)
import qualified Data.ByteString.Base64 as Base64
import Data.Text (splitOn)
import Network.URI.Encode (encodeByteString)
import Polysemy (Members, Sem, interpret, makeSem)
import Polysemy.Error (Error, throw)
import Polysemy.Input (Input, input)
import Polysemy.Trace (Trace)
import Relude (ByteString, Either (..), decodeUtf8, encodeUtf8, fmap, return, show, ($), (<>), (==))
import Types.AppError (AppError (BadRequest))
import Types.SecretKey

data Algebra m a where
  Sign :: ByteString -> Algebra m ByteString
  Verify :: ByteString -> Algebra m ByteString

makeSem ''Algebra

run ::
  Members '[Input SecretKey, Error AppError, Trace] r =>
  Sem (Algebra ': r) a ->
  Sem r a
run = interpret $ \case
  Sign msg -> do
    secretKey <- input
    let encodedMsg = Base64.encode msg
        digest = getDigest secretKey encodedMsg
    return $
      encodeByteString $ encodedMsg <> "--" <> digest
  Verify signature -> do
    secretKey <- input
    case fmap encodeUtf8 $ splitOn "--" $ decodeUtf8 signature of
      [encodedMsg, digest] -> do
        if getDigest secretKey encodedMsg == digest
          then case Base64.decode encodedMsg of
            Left err -> throw $ BadRequest (show err)
            Right msg -> return msg
          else throw $ BadRequest "Invalid Signature"
      _ -> throw $ BadRequest "Invalid Signature"
{-# INLINE run #-}

getDigest :: SecretKey -> ByteString -> ByteString
getDigest (SecretKey key) msg =
  Base64.encode $
    convert $
      hmacGetDigest $
        hmac @_ @_ @SHA1 key msg