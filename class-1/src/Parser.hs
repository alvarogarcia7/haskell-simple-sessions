module Parser where

class Operation a where
  apply :: Expression a -> a
  new :: a -> Expression a

instance Operation Bool where
  new b = Literal b
  apply (Op2 a b) = (apply a) && (apply b)
  apply (Op1 a) = not (apply a)
  apply (Literal a) = a

data Expression a = Op2 (Expression a) (Expression a) 
                  | Op1 (Expression a)
                  | Literal a
  deriving (Show, Eq)

