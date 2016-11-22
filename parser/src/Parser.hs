module Parser where

import Data.Text as T (pack, splitOn, unpack)

calculate "T" = True
calculate "F" = False
calculate ('N':'O':'T':' ':rest) = not $ calculate rest
calculate expression = foldl1 (&&) $ map calculate $ map T.unpack $ T.splitOn (T.pack " AND ") (T.pack expression)
