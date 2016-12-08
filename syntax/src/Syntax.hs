module Syntax where

letNotation :: Integer
letNotation =
  let a = 1
  in a

-- error: parse error on input ‘a’
-- pointing to the single usage of 'a'
-- notice that you need a `do` block -- see example below
--letNotation =
--  let a = 1
--  a

letNotation' :: Integer
letNotation' = do
  let a = 1
  a + 1

