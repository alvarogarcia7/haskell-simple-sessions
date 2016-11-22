module Parser where

import Data.Text as T (pack, splitOn, unpack)

calculate "T" = True
calculate "F" = False
calculate ('N':'O':'T':' ':rest) = not $ calculate rest
calculate expression = do
    let andDelimiter = T.pack " AND "
    let expressionParts =  map T.unpack $ T.splitOn andDelimiter (T.pack expression)
    foldl1 (&&) $ map calculate expressionParts
