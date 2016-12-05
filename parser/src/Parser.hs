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

apply :: AST (typ -> typ ->typ) typ -> typ
apply (Operation fn children) = fn (apply (children !! 0)) (apply (children !! 1))
apply (Literal l) = l

-- See example-ast.txt


--calculate' "T" = "True"
--calculate' "F" = "False"
--calculate' "NOT" = "not"
--calculate' "AND" = "&&"
--calculate' "OR" = "||"
calculate' expression = do
  let delimiters = map T.pack [" "]
  let expressionParts =  map T.unpack $ foldl (\acc ele -> concat $ map (T.splitOn ele) acc) [T.pack expression] delimiters
  let parts = map parse expressionParts
  parseOp (expressionParts !! 1)  [parts !! 0, parts !! 2]

parse :: String -> AST (Bool -> Bool -> Bool) Bool
parse b =
  case b of 
   ("T") -> Literal True
   ("F") -> Literal False
   ("AND") -> Operation (&&) []

parseOp :: String -> [AST (Bool -> Bool -> Bool) Bool]-> AST (Bool -> Bool -> Bool) Bool
parseOp expr otherOperands = 
  case expr of
    ("AND") -> Operation (&&) otherOperands
