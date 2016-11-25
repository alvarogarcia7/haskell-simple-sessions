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
    show (Literal l) = "literal"

apply :: AST (typ -> typ ->typ) typ -> typ
apply (Operation fn children) = fn (apply (children !! 0)) (apply (children !! 1))
apply (Literal l) = l
