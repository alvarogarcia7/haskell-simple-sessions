{-# LANGUAGE DataKinds #-}

module Parser where

import Data.Text as T (pack, splitOn, unpack)

calculate "T" = True
calculate "F" = False
calculate ('N':'O':'T':' ':rest) = not $ calculate rest
calculate expression = do
  let delimiters = map T.pack [" AND ", " OR "]
  let expressionParts =  map T.unpack $ foldl (\acc ele -> concat $ map (T.splitOn ele) acc) [T.pack expression] delimiters
  foldl1 (&&) $ map calculate expressionParts

data AST op typ = Operation op [AST op typ]
                | Literal typ
                deriving (Eq)

instance Show (AST a b) where
  show (Operation _ children) = "function [" ++ (concat $ map show children) ++ "]"
  show (Literal b) = "Value"

apply :: AST (typ -> typ ->typ) typ -> typ
apply (Operation fn children) = fn (apply (children !! 0)) (apply (children !! 1))
apply (Literal l) = l

-- See example-ast.txt

parse expression = do
  let delimiters = map T.pack [" "]
  let expressionParts =  map T.unpack $ foldl (\acc ele -> concat $ map (T.splitOn ele) acc) [T.pack expression] delimiters
  let isNot = (head expressionParts) == "NOT"
  if isNot then 
    do
      let (_:restExpressions) = expressionParts
      parseOp (expressionParts !! 0) [(parse' (expressionParts !! 1))]
    else
      do
        let operand1 = parse' (expressionParts !! 0)
        let (_:restExpressions) = expressionParts
        parse2 operand1 restExpressions

parse2 tree1 raw = 
  if (null raw) then
      tree1
    else do
        let operator = raw !! 0
        let operand = parse' $ raw !! 1
        let rawRest = consume2 raw
        let currentTree = parseOp operator [tree1, operand]
        parse2 currentTree rawRest

consume2 (_:_:xs) = xs
consume3 (_:_:_:xs) = xs

parse' :: String -> AST (Bool -> Bool -> Bool) Bool
parse' b =
  case b of 
   ("T") -> Literal True
   ("F") -> Literal False
   ("_") -> Operation (&&) []

parseOp :: String -> [AST (Bool -> Bool -> Bool) Bool]-> AST (Bool -> Bool -> Bool) Bool
parseOp expr otherOperands = 
  case expr of
    ("AND") -> Operation (&&) otherOperands
    ("OR") -> Operation (||) otherOperands
    ("NOT") -> Operation (\a _ -> not a) (otherOperands++[parse' "T"])
