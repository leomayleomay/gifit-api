module Effects.Database where

import qualified Data.Pool as P
import qualified Database.PostgreSQL.Simple as PG
import Polysemy (Embed, Members, Sem, embed, interpret, makeSem)
import Polysemy.Input (Input, input)
import Relude
  ( IO,
    Maybe,
    listToMaybe,
    ($),
    (<$>),
  )

data Algebra m a where
  Fetch1_ :: PG.FromRow b => PG.Query -> Algebra m (Maybe b)
  Fetch1 :: (PG.FromRow b, PG.ToRow c) => PG.Query -> c -> Algebra m (Maybe b)
  FetchN_ :: PG.FromRow b => PG.Query -> Algebra m [b]
  FetchN :: (PG.FromRow b, PG.ToRow c) => PG.Query -> c -> Algebra m [b]

makeSem ''Algebra

run ::
  Members '[Input (P.Pool PG.Connection), Embed IO] r =>
  Sem (Algebra ': r) a ->
  Sem r a
run =
  interpret $ \case
    Fetch1_ q -> do
      pool <- input
      embed $ P.withResource pool $ \conn -> listToMaybe <$> PG.query_ conn q
    Fetch1 q args -> do
      pool <- input
      embed $ P.withResource pool $ \conn -> listToMaybe <$> PG.query conn q args
    FetchN_ q -> do
      pool <- input
      embed $ P.withResource pool $ \conn -> PG.query_ conn q
    FetchN q args -> do
      pool <- input
      embed $ P.withResource pool $ \conn -> PG.query conn q args
{-# INLINE run #-}
