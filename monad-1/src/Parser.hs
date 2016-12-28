module Parser where

class Operation a where
  apply :: Expression a -> a
  new :: a -> Expression a

instance Operation Bool where
  new b = Literal b
  apply (And a b) = (apply a) && (apply b)
  apply (Not a) = not (apply a)
  apply (Literal a) = a

data Expression a = And (Expression a) (Expression a) 
                 | Not (Expression a)
                 | Literal a
  deriving (Show, Eq)

