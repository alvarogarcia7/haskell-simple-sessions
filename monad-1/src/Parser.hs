module Parser where

data Operation a = And a a | Not a
  deriving (Show, Eq)
