module Types.PasswordInput (PasswordInput) where

import Refined (Refined, SizeGreaterThan)
import Relude (Text)

type PasswordInput = Refined (SizeGreaterThan 6) Text