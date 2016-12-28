module Parser where

data Operation a = And a a | Not a
  deriving (Show, Eq)

apply :: Operation Bool -> Bool
apply (And a b) = a && b
apply (Not a) = not a
