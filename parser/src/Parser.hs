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
  let parts = map parse' expressionParts
  let (operand1:_:operand2:restParts) = parts
  let (_:_:_:restExpressions) = expressionParts
  let operandRepresentation = expressionParts !! 1
  let tree1 = parseOp operandRepresentation  [operand1, operand2]
  parse2 tree1 restParts restExpressions


parse2 tree1 parsed raw = 
  if (null raw) then
      tree1
    else do
        let operator = raw !! 0
        let operand = parsed !! 1
        let (_:_:parsedRest) = parsed
        let (_:_:rawRest) = raw
        let currentTree = parseOp operator [tree1, parsed !! 1]
        parse2 currentTree parsedRest rawRest

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
