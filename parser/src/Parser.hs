module Parser where

import Data.Text as T (pack, splitOn, unpack)

calculate "T" = True
calculate "F" = False
calculate ('N':'O':'T':' ':rest) = not $ calculate rest
calculate expression = do
    calculate $ calculateWith "AND" (&&) expression

calculateWith str op expression = do
  let parts = T.splitOn (T.pack " ") $ T.pack expression
  let whichIndexesAreTheOperator = filter (\i -> not (-1 ==i)) $ zipWith (\text index -> if (text == (T.pack str)) then index else -1) parts [0..]
  let operand1 = calculate $ T.unpack $ parts !! ((whichIndexesAreTheOperator!!0) -1)
  let operand2 = calculate $ T.unpack $ parts !! ((whichIndexesAreTheOperator!!0) +1)
  let rest = snd (splitAt ((whichIndexesAreTheOperator!!0) +2) expression)
  if (op operand1 operand2) then "T "++rest else "F "++rest

