module Types.AmountInput (AmountInput) where

import Refined (GreaterThan, Refined)
import Relude (Int)

type AmountInput = Refined (GreaterThan 10) Int