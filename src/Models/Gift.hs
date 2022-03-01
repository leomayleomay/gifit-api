module Models.Gift where

import Control.Lens ((^.))
import qualified Effects.Database as Database
import Polysemy (Members, Sem, interpret, makeSem)
import qualified Refined as R
import Relude (Int, Maybe, Text, ($))
import Types.AmountInput (AmountInput)
import Types.Gift (Gift, id)
import Types.GiftDetail (GiftDetail)

data Algebra m a where
  Create :: Int -> AmountInput -> Maybe Text -> Int -> Algebra m (Maybe Gift)
  FindCreatedById :: Int -> Algebra m (Maybe Gift)
  FindAllBySender :: Int -> Algebra m [GiftDetail]
  FindAllByReceiver :: Int -> Algebra m [GiftDetail]
  MarkAsPaid :: Gift -> Algebra m (Maybe Gift)

makeSem ''Algebra

run ::
  Members '[Database.Algebra] r =>
  Sem (Algebra ': r) a ->
  Sem r a
run =
  interpret $ \case
    Create sender amount notes receiver ->
      Database.fetch1
        "insert into gifts (sender, amount, notes, receiver, state) values (?, ?, ?, ?, 'created') returning *"
        (sender, R.unrefine amount, notes, receiver)
    FindCreatedById id' ->
      Database.fetch1 "select * from gifts where id=? and state='created'" [id']
    FindAllBySender sender ->
      Database.fetchN "select g.id, s.handle, r.handle, g.amount, g.state, g.notes, g.created_at from gifts g inner join users s on g.sender=s.id inner join users r on g.receiver=r.id where g.sender=?" [sender]
    FindAllByReceiver receiver ->
      Database.fetchN "select g.id, s.handle, r.handle, g.amount, g.state, g.notes, g.created_at from gifts g inner join users s on g.sender=s.id inner join users r on g.receiver=r.id where g.receiver=?" [receiver]
    MarkAsPaid gift ->
      Database.fetch1
        "update gifts set state='paid' where id=? returning *"
        [gift ^. id]
{-# INLINE run #-}