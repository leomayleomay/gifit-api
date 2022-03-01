module Types.AppError (AppError (..)) where

import Relude (Eq, LText, Show)

data AppError
  = NotFound
  | Unauthorized
  | InternalError
  | BadRequest LText
  deriving (Eq, Show)
