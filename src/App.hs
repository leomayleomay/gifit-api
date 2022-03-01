module App
  ( app,
    testApp,
    startApp,
  )
where

import qualified Controllers.Gift as GiftsController
import qualified Controllers.Password as PasswordsController
import qualified Controllers.Payment as PaymentsController
import qualified Controllers.Registration as RegistrationsController
import qualified Controllers.Session as SessionsController
import qualified Controllers.User as UsersController
import qualified Data.ByteString.Char8 as B
import qualified Data.Pool as P
import qualified Database.PostgreSQL.Simple as PG
import qualified Database.PostgreSQL.Simple.Migration as PG
import qualified Effects.Database as Database
import qualified Effects.Random as Random
import qualified Models.Gift as GiftRepo
import qualified Models.User as UserRepo
import Network.HTTP.Types (hContentType, renderStdMethod)
import qualified Network.Mail.SMTP as SMTP
import Network.Wai (Application)
import qualified Network.Wai.Handler.Warp as W
import Network.Wai.Middleware.Cors
  ( CorsResourcePolicy (corsMethods, corsOrigins, corsRequestHeaders),
    cors,
    simpleCorsResourcePolicy,
  )
import Polysemy
  ( Embed (Embed),
    Member,
    Members,
    Sem,
    embed,
    interpret,
    runM,
  )
import Polysemy.Error (Error, runError)
import Polysemy.Input (Input, runInputConst)
import Polysemy.Trace (Trace, traceToIO)
import Relude
  ( Bool (..),
    ConvertUtf8 (encodeUtf8),
    IO,
    Maybe (..),
    Proxy (..),
    const,
    either,
    liftIO,
    map,
    mapM_,
    maxBound,
    maybe,
    minBound,
    return,
    void,
    ($),
    (.),
    (<$>),
  )
import Relude.Unsafe (read)
import Servant
  ( Context (EmptyContext, (:.)),
    Handler,
    ServerT,
    err400,
    err401,
    err404,
    err500,
    errBody,
    hoistServerWithContext,
    serveWithContext,
    throwError,
    (:<|>) ((:<|>)),
  )
import Servant.Auth.Server
  ( CookieSettings,
    JWTSettings,
    defaultCookieSettings,
    defaultJWTSettings,
    generateKey,
  )
import qualified Services.Email as EmailService
import qualified Services.Password as PasswordService
import qualified Services.Signature as SignatureService
import System.Environment (getEnv, lookupEnv)
import Types.AppError (AppError (..))
import Types.ClientHost (ClientHost (ClientHost))
import Types.SecretKey (SecretKey (SecretKey))
import Types.ServerHost (ServerHost (ServerHost))

type API =
  SessionsController.API
    :<|> RegistrationsController.API
    :<|> PasswordsController.API
    :<|> UsersController.API
    :<|> GiftsController.API
    :<|> PaymentsController.API

api ::
  Members
    '[ SessionsController.Algebra,
       RegistrationsController.Algebra,
       PasswordsController.Algebra,
       UsersController.Algebra,
       GiftsController.Algebra,
       PaymentsController.Algebra,
       Error AppError,
       Trace
     ]
    r =>
  ServerT API (Sem r)
api =
  SessionsController.api
    :<|> RegistrationsController.api
    :<|> PasswordsController.api
    :<|> UsersController.api
    :<|> GiftsController.api
    :<|> PaymentsController.api

createDbPool :: IO (P.Pool PG.Connection)
createDbPool = do
  databaseUrl <- getEnv "DATABASE_URL"
  let open = PG.connectPostgreSQL $ B.pack databaseUrl
  P.createPool open PG.close 1 10 10

initializeDb :: PG.Connection -> IO ()
initializeDb conn =
  PG.withTransaction conn $
    mapM_ migrate [PG.MigrationInitialization, PG.MigrationDirectory "schema/migrations"]
  where
    migrate cmd = PG.runMigration $ PG.MigrationContext cmd True conn

createSmtpPool :: IO (P.Pool SMTP.SMTPConnection)
createSmtpPool = do
  host <- getEnv "SENDGRID_HOST"
  P.createPool (SMTP.connectSMTP host) SMTP.closeSMTP 1 10 10

initializeSmtp :: SMTP.SMTPConnection -> IO ()
initializeSmtp conn = do
  username <- getEnv "SENDGRID_USERNAME"
  password <- getEnv "SENDGRID_PASSWORD"
  void $ SMTP.login conn username password

mkCorsResourcePolicy :: ClientHost -> IO CorsResourcePolicy
mkCorsResourcePolicy (ClientHost clientHost) =
  return
    simpleCorsResourcePolicy
      { corsOrigins = Just ([B.pack clientHost], False),
        corsMethods = map renderStdMethod [minBound .. maxBound],
        corsRequestHeaders = [hContentType, "Origin"]
      }

testApp :: IO Application
testApp = do
  dbPool <- createDbPool

  P.withResource dbPool initializeDb

  jwtSettings <- defaultJWTSettings <$> generateKey

  secretKey <- getEnv "SECRET_KEY"

  serverHost <- getEnv "SERVER_HOST"

  clientHost <- getEnv "CLIENT_HOST"

  let server =
        hoistServerWithContext
          (Proxy :: Proxy API)
          (Proxy :: Proxy '[CookieSettings, JWTSettings])
          ( interpretTestServer
              (SecretKey $ encodeUtf8 secretKey)
              (ServerHost serverHost)
              (ClientHost clientHost)
              jwtSettings
              dbPool
          )
          api

  corsResourcePolicy <- mkCorsResourcePolicy $ ClientHost clientHost

  return $
    cors (const $ Just corsResourcePolicy) $
      serveWithContext
        (Proxy :: Proxy API)
        (defaultCookieSettings :. jwtSettings :. EmptyContext)
        server

interpretTestServer ::
  SecretKey ->
  ServerHost ->
  ClientHost ->
  JWTSettings ->
  P.Pool PG.Connection ->
  Sem
    '[ SessionsController.Algebra,
       RegistrationsController.Algebra,
       PasswordsController.Algebra,
       UsersController.Algebra,
       GiftsController.Algebra,
       PaymentsController.Algebra,
       UserRepo.Algebra,
       GiftRepo.Algebra,
       EmailService.Algebra,
       PasswordService.Algebra,
       SignatureService.Algebra,
       Database.Algebra,
       Random.Algebra,
       Input (P.Pool PG.Connection),
       Input ClientHost,
       Input ServerHost,
       Input SecretKey,
       Error AppError,
       Trace,
       Embed IO,
       Embed Handler
     ]
    a ->
  Handler a
interpretTestServer secretKey serverHost clientHost jwt dbPool eff = do
  res <- runEffects
  either handleError return res
  where
    runEffects =
      runM
        . ioToHandler
        . traceToIO
        . runError @AppError
        . runInputConst @SecretKey secretKey
        . runInputConst @ServerHost serverHost
        . runInputConst @ClientHost clientHost
        . runInputConst @(P.Pool PG.Connection) dbPool
        . Random.run
        . Database.run
        . SignatureService.run
        . PasswordService.run
        . EmailService.runToTest
        . GiftRepo.run
        . UserRepo.run
        . PaymentsController.run
        . GiftsController.run
        . UsersController.run
        . PasswordsController.run
        . RegistrationsController.run jwt
        . SessionsController.run jwt
        $ eff

app :: IO Application
app = do
  dbPool <- createDbPool

  P.withResource dbPool initializeDb

  smtpPool <- createSmtpPool

  P.withResource smtpPool initializeSmtp

  jwtSettings <- defaultJWTSettings <$> generateKey

  secretKey <- getEnv "SECRET_KEY"

  serverHost <- getEnv "SERVER_HOST"

  clientHost <- getEnv "CLIENT_HOST"

  let server =
        hoistServerWithContext
          (Proxy :: Proxy API)
          (Proxy :: Proxy '[CookieSettings, JWTSettings])
          ( interpretServer
              (SecretKey $ encodeUtf8 secretKey)
              (ServerHost serverHost)
              (ClientHost clientHost)
              jwtSettings
              dbPool
              smtpPool
          )
          api

  corsResourcePolicy <- mkCorsResourcePolicy $ ClientHost clientHost

  return $
    cors (const $ Just corsResourcePolicy) $
      serveWithContext
        (Proxy :: Proxy API)
        (defaultCookieSettings :. jwtSettings :. EmptyContext)
        server

startApp :: IO ()
startApp = do
  app' <- app

  port <- lookupEnv "PORT"

  W.run (maybe 4000 read port) app'

ioToHandler :: Member (Embed Handler) r => Sem (Embed IO ': r) a -> Sem r a
ioToHandler = interpret \(Embed io) -> embed $ liftIO io

interpretServer ::
  SecretKey ->
  ServerHost ->
  ClientHost ->
  JWTSettings ->
  P.Pool PG.Connection ->
  P.Pool SMTP.SMTPConnection ->
  Sem
    '[ SessionsController.Algebra,
       RegistrationsController.Algebra,
       PasswordsController.Algebra,
       UsersController.Algebra,
       GiftsController.Algebra,
       PaymentsController.Algebra,
       UserRepo.Algebra,
       GiftRepo.Algebra,
       EmailService.Algebra,
       PasswordService.Algebra,
       SignatureService.Algebra,
       Database.Algebra,
       Random.Algebra,
       Input (P.Pool SMTP.SMTPConnection),
       Input (P.Pool PG.Connection),
       Input ClientHost,
       Input ServerHost,
       Input SecretKey,
       Error AppError,
       Trace,
       Embed IO,
       Embed Handler
     ]
    a ->
  Handler a
interpretServer secretKey serverHost clientHost jwt dbPool smtpPool eff = do
  res <- runEffects
  either handleError return res
  where
    runEffects =
      runM
        . ioToHandler
        . traceToIO
        . runError @AppError
        . runInputConst @SecretKey secretKey
        . runInputConst @ServerHost serverHost
        . runInputConst @ClientHost clientHost
        . runInputConst @(P.Pool PG.Connection) dbPool
        . runInputConst @(P.Pool SMTP.SMTPConnection) smtpPool
        . Random.run
        . Database.run
        . SignatureService.run
        . PasswordService.run
        . EmailService.runToIO
        . GiftRepo.run
        . UserRepo.run
        . PaymentsController.run
        . GiftsController.run
        . UsersController.run
        . PasswordsController.run
        . RegistrationsController.run jwt
        . SessionsController.run jwt
        $ eff

handleError :: AppError -> Handler a
handleError =
  throwError . \case
    BadRequest body -> err400 {errBody = encodeUtf8 body}
    Unauthorized -> err401
    NotFound -> err404
    InternalError -> err500
