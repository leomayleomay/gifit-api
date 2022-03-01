{-# LANGUAGE QuasiQuotes #-}

import App (testApp)
import Control.Lens ((^.))
import Data.Aeson (decode)
import Data.Text (replace)
import qualified Database.PostgreSQL.Simple as PG
import Network.HTTP.Types (methodGet, methodPost)
import Network.Wai.Test (simpleBody)
import Relude
  ( IO,
    Int,
    Maybe (..),
    String,
    encodeUtf8,
    listToMaybe,
    not,
    null,
    toStrict,
    void,
    ($),
    (.),
    (<$>),
    (<>),
    (>>),
  )
import Test.Hspec (Spec, after_, describe, hspec, it, shouldBe, shouldSatisfy)
import Test.Hspec.Wai (liftIO, matchStatus, request, shouldRespondWith, with)
import Test.Hspec.Wai.JSON (json)
import Types.CreateGiftResponse (CreateGiftResponse)
import qualified Types.CreateGiftResponse as CreateGiftResponse
import Types.Gift (Gift)
import qualified Types.Gift as Gift
import Types.GiftState (GiftState (..))
import Types.User (User)
import qualified Types.User as User

get path = request methodGet path headers ""
  where
    headers = [("Content-Type", "application/json")]

getWithToken path token = request methodGet path headers ""
  where
    headers =
      [ ("Content-Type", "application/json"),
        ("Authorization", "Bearer " <> token)
      ]

post path = request methodPost path headers
  where
    headers = [("Content-Type", "application/json")]

postWithToken path token = request methodPost path headers
  where
    headers =
      [ ("Content-Type", "application/json"),
        ("Authorization", "Bearer " <> token)
      ]

main :: IO ()
main = do
  conn <- PG.connectPostgreSQL "dbname=gifit"
  hspec $ spec conn

truncateTables :: PG.Connection -> IO ()
truncateTables conn =
  void $
    PG.execute_ conn "truncate users cascade"
      >> PG.execute_ conn "truncate gifts cascade"

findUserByEmail :: PG.Connection -> String -> IO (Maybe User)
findUserByEmail conn e =
  listToMaybe <$> PG.query conn "select * from users where email=?" [e]

findGiftById :: PG.Connection -> Int -> IO (Maybe Gift)
findGiftById conn id' =
  listToMaybe <$> PG.query conn "select * from gifts where id=?" [id']

findGiftByAmount :: PG.Connection -> Int -> IO (Maybe Gift)
findGiftByAmount conn amount =
  listToMaybe <$> PG.query conn "select * from gifts where amount=?" [amount]

spec :: PG.Connection -> Spec
spec conn = with testApp $
  after_ (truncateTables conn) $ do
    describe "send gift to a new user" $ do
      it "is successful" $ do
        -- sender signup
        _ <- post "/signup" [json|{email: "sender@test.com", handle: "sender", password: "12345678"}|]

        -- sender login
        _ <- post "/login" [json|{email: "sender@test.com", password: "12345678"}|]

        -- sender forgets password
        post "/forget_password" [json|{email: "sender@test.com"}|] `shouldRespondWith` ""
        Just sender <- liftIO $ findUserByEmail conn "sender@test.com"
        liftIO $ (sender ^. User.resetPasswordToken) `shouldSatisfy` (not . null)

        -- sender resets password
        post "/reset_password" [json|{token: #{sender ^. User.resetPasswordToken}, password: "87654321", password_confirmation: "87654321"}|] `shouldRespondWith` ""
        Just sender' <- liftIO $ findUserByEmail conn "sender@test.com"
        liftIO $ (sender' ^. User.resetPasswordToken) `shouldSatisfy` null

        -- sender login with new password
        senderLoginResult' <- post "/login" [json|{email: "sender@test.com", password: "87654321"}|]
        let senderToken = simpleBody senderLoginResult'

        -- sender sends gift to receiver via receiver's email
        createGiftResult <- postWithToken "/gifts" (toStrict senderToken) [json|{receiver: "receiver@test.com", amount: 100}|]

        -- sender sends gift to receiver via receiver's id
        Just receiver <- liftIO (findUserByEmail conn "receiver@test.com")
        postWithToken "/gifts" (toStrict senderToken) [json|{receiver: #{receiver ^. User.id}, amount: 300}|] `shouldRespondWith` 200

        -- payment
        let (Just body) = decode $ simpleBody createGiftResult :: Maybe CreateGiftResponse
        let path = replace "http://localhost:4000" "" $ body ^. CreateGiftResponse.callbackUrl
        post (encodeUtf8 path) "" `shouldRespondWith` ""
        Just gift <- liftIO $ findGiftById conn (body ^. CreateGiftResponse.id)
        liftIO $ (gift ^. Gift.state) `shouldBe` Paid

        -- receiver activates his/her account
        Just receiver <- liftIO (findUserByEmail conn "receiver@test.com")
        post "/activate" [json|{token: #{receiver ^. User.resetPasswordToken}, handle: "receiver", password: "12345678", password_confirmation: "12345678"}|] `shouldRespondWith` ""

        -- unauthorised lookup user with email
        get "/users?q=receiver@test.com" `shouldRespondWith` "" {matchStatus = 401}

        -- authorised
        Just receiver' <- liftIO (findUserByEmail conn "receiver@test.com")
        -- lookup user with handle
        getWithToken "/users?q=receiver" (toStrict senderToken) `shouldRespondWith` [json|[{id: #{receiver' ^. User.id}, email: "receiver@test.com", handle: "receiver"}]|]
        -- lookup user with email
        getWithToken "/users?q=receiver@test.com" (toStrict senderToken) `shouldRespondWith` [json|[{id: #{receiver' ^. User.id}, email: "receiver@test.com", handle: "receiver"}]|]

        -- lookup gifts going out
        Just gift100 <- liftIO $ findGiftByAmount conn 100
        Just gift300 <- liftIO $ findGiftByAmount conn 300
        getWithToken "/gifts?dir=out" (toStrict senderToken)
          `shouldRespondWith` [json|[{
            "amount":100,"state":"Paid","receiver":"receiver","sender":"sender","notes":null,"sent_at":#{gift100 ^. Gift.createdAt}
          },{
            "amount":300,"state":"Created","receiver":"receiver","sender":"sender","notes":null,"sent_at":#{gift300 ^. Gift.createdAt}
          }]|]

        -- lookup gifts coming in
        getWithToken "/gifts?dir=in" (toStrict senderToken) `shouldRespondWith` [json|[]|]

    describe "send gift to an existing user" $ do
      it "is successful" $ do
        -- receiver signup
        _ <- post "/signup" [json|{email: "receiver@test.com", handle: "receiver", password: "12345678"}|]
        (Just receiver) <- liftIO $ findUserByEmail conn "receiver@test.com"

        -- sender signup
        _ <- post "/signup" [json|{email: "sender@test.com", handle: "sender", password: "12345678"}|]

        -- sender login
        senderLoginResult <- post "/login" [json|{email: "sender@test.com", password: "12345678"}|]
        let senderToken = simpleBody senderLoginResult

        -- send gift
        _ <- postWithToken "/gifts" (toStrict senderToken) [json|{receiver: #{receiver ^. User.id}, amount: 100}|]

        -- sender looks up gifts going out
        Just gift100' <- liftIO $ findGiftByAmount conn 100
        _ <-
          getWithToken "/gifts?dir=out" (toStrict senderToken)
            `shouldRespondWith` [json|
              [{"amount":100,"state":"Created","receiver":"receiver","sender":"sender","notes":null,"sent_at":#{gift100' ^. Gift.createdAt}}]
            |]

        -- sender looks up gifts coming in
        _ <- getWithToken "/gifts?dir=in" (toStrict senderToken) `shouldRespondWith` [json|[]|]

        -- receiver login
        receiverLoginResult <- post "/login" [json|{email: "receiver@test.com", password: "12345678"}|]
        let receiverToken = simpleBody receiverLoginResult

        -- receiver looks up gifts coming in
        Just gift100 <- liftIO $ findGiftByAmount conn 100
        getWithToken "/gifts?dir=in" (toStrict receiverToken)
          `shouldRespondWith` [json|
            [{"amount":100,"state":"Created","receiver":"receiver","sender":"sender","notes":null,"sent_at":#{gift100 ^. Gift.createdAt}}]
          |]

        -- receiver looks up gifts going out
        getWithToken "/gifts?dir=out" (toStrict receiverToken) `shouldRespondWith` [json|[]|]
