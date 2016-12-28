module Parser where

class Oper a where
  apply' :: Operation a -> a
  new :: a -> Operation a

instance Oper Bool where
  new b = Expression b
  apply' (And a b) = (apply' a) && (apply' b)
  apply' (Not a) = not (apply' a)
  apply' (Expression a) = a

data Operation a = And (Operation a) (Operation a) 
                 | Not (Operation a)
                 | Expression a
  deriving (Show, Eq)

apply b = apply' b

