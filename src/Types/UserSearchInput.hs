module Types.UserSearchInput (UserSearchInput) where

import Refined (Refined, SizeGreaterThan)
import Relude (Text)

type UserSearchInput = Refined (SizeGreaterThan 3) Text