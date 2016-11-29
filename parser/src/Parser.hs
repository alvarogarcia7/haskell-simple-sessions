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
                | Literale typ
                deriving (Eq)

instance Show (AST a b) where
    show (Operation _ children) = "function [" ++ (concat $ map show children) ++ "]"
    show (Literale l) = "literal"

apply :: AST (typ -> typ ->typ) typ -> typ
apply (Operation fn children) = fn (apply (children !! 0)) (apply (children !! 1))
apply (Literale l) = l

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
  let fn = parseFn (parts !! 1)
  Operation fn [Literale (parts !! 0), Literale (parts !! 2)]

myAnd :: Literale -> Literale -> Literale
myAnd _ BFalse = BFalse
myAnd BFalse _ = BFalse
myAnd BTrue BTrue = BTrue

data Literale = BTrue
        | BFalse 
        | AND

parseFn :: Literale -> (Literale -> Literale -> Literale)
parseFn AND = myAnd

instance Show Literale where
  show BTrue = "True"
  show BFalse = "False"
  show AND = "&&"

parse :: [Char] -> Literale
parse b =
  case b of 
   ("T") -> BTrue
   ("F") -> BFalse
   ("AND") -> AND
