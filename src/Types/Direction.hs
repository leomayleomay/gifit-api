module Types.Direction (Direction (..)) where

import Data.Text (toLower)
import Relude (Either (..), Eq, Show, otherwise, (==))
import Web.HttpApiData (FromHttpApiData (..))

data Direction
  = In
  | Out
  deriving (Show, Eq)

instance FromHttpApiData Direction where
  parseQueryParam input
    | toLower input == "in" = Right In
    | toLower input == "out" = Right Out
    | otherwise = Left "Invalid direction"