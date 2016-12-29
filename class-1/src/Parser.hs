module Parser where

class Operation a where
  apply :: Expression a -> a
  new :: a -> Expression a
  parse :: String -> Expression a

instance Operation Bool where
  new b = Literal b
  apply (Op2 "AND" a b) = (apply a) && (apply b)
  apply (Op2 "OR" a b) = (apply a) || (apply b)
  apply (Op1 _ a) = not (apply a)
  apply (Literal a) = a
  parse s = Op2 "AND" (Literal True) (Literal False)

data Expression a = Op2 String (Expression a) (Expression a)
                  | Op1 String (Expression a)
                  | Literal a
  deriving (Show, Eq)
