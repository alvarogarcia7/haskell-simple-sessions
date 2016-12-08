module Syntax where

letNotation =
  let a = 1
  in a

-- error: parse error on input ‘a’
-- pointing to the single usage of 'a'
--letNotation =
--  let a = 1
--  a

letNotation' = do
  let a = 1
  a + 1

