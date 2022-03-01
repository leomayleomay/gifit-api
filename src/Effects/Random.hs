module Effects.Random where

import Data.UUID (toText)
import Data.UUID.V4 (nextRandom)
import Polysemy (Embed, Members, Sem, embed, interpret, makeSem)
import Relude (IO, Text, ($), (<$>))

data Algebra m a where
  NextUUID :: Algebra m Text

makeSem ''Algebra

run :: Members '[Embed IO] r => Sem (Algebra ': r) a -> Sem r a
run = interpret $ \case NextUUID -> embed $ toText <$> nextRandom
{-# INLINE run #-}