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

lazyNames =
  let
    a = 4
    x = undefined -- undefined is a name that cannot be defined. fails at runtime
  in a

