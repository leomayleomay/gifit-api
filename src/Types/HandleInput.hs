module Types.HandleInput (HandleInput) where

import Refined
  ( And,
    Refined,
    SizeGreaterThan,
    SizeLessThan,
  )
import Relude (Text)

type HandleInput = Refined (And (SizeGreaterThan 2) (SizeLessThan 32)) Text
