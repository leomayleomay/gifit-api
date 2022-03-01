module Services.Email where

import Control.Concurrent (forkIO)
import Control.Lens ((^.))
import qualified Data.Pool as P
import Data.String.Interpolate (i)
import Network.Mail.Mime (Mail, Part, htmlPart)
import Network.Mail.SMTP (Address (Address), SMTPConnection, renderAndSend, simpleMail)
import Polysemy (Embed, Members, Sem, embed, interpret, makeSem)
import Polysemy.Input (Input, input)
import Relude (IO, Maybe (..), Text, return, void, ($))
import Types.ClientHost
import Types.Email (Email (..))
import Types.User (User, email, resetPasswordToken)

data Algebra m a where
  NotifyUserOfPasswordReset :: User -> Algebra m ()
  NotifyUserToActivateAccount :: User -> Algebra m ()

makeSem ''Algebra

composeMail :: Text -> Email -> [Part] -> Mail
composeMail subject (Email recipient) =
  simpleMail from to [] [] subject
  where
    from = Address (Just "Gifit") "support@gifit.com"
    to = [Address Nothing recipient]

runToIO ::
  Members '[Input (P.Pool SMTPConnection), Input ClientHost, Embed IO] r =>
  Sem (Algebra ': r) a ->
  Sem r a
runToIO =
  interpret $ \case
    NotifyUserOfPasswordReset user -> do
      pool <- input @(P.Pool SMTPConnection)
      clientHost <- input @ClientHost
      embed $ void $ forkIO $ P.withResource pool $ \conn -> renderAndSend conn $ mail clientHost
      where
        html (ClientHost clientHost) = htmlPart [i|Please click the <a href='#{clientHost}/reset_password?token=#{user ^. resetPasswordToken}'>link</a> to reset your password|]
        mail clientHost = composeMail "Password Reset" (user ^. email) [html clientHost]
    NotifyUserToActivateAccount user -> do
      pool <- input @(P.Pool SMTPConnection)
      clientHost <- input @ClientHost
      embed $ void $ forkIO $ P.withResource pool $ \conn -> renderAndSend conn $ mail clientHost
      where
        html (ClientHost clientHost) = htmlPart [i|Please click the <a href='#{clientHost}/activate?token=#{user ^. resetPasswordToken}'>link</a> to activate your account|]
        mail clientHost = composeMail "[Gifit] Please activate your account" (user ^. email) [html clientHost]
{-# INLINE runToIO #-}

runToTest ::
  Sem (Algebra ': r) a ->
  Sem r a
runToTest =
  interpret $ \case
    NotifyUserOfPasswordReset _user -> return ()
    NotifyUserToActivateAccount _user -> return ()
{-# INLINE runToTest #-}