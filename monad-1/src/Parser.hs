module Parser where

class Oper a where
  apply' :: a -> a

data Operation a = And (Operation a) (Operation a) 
                 | Not (Operation a)
                 | Expression a
  deriving (Show, Eq)



apply :: Operation Bool -> Bool
apply (And a b) = (apply a) && (apply b)
apply (Not a) = not (apply a)
apply (Expression a) = a
