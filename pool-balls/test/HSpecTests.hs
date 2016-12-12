import PoolBalls

import Debug.Trace
import Test.Hspec
import Text.Printf (printf)

import Test.QuickCheck
import System.Random
import Data.Map
 

prop_halfWrongBalls :: String -> String -> Bool
prop_halfWrongBalls balls b= False
--  let shuffled = fisherYates (mkStdGen 0) balls
--  let numberSwaps = length $ reorder balls (fst shuffled)
--  let prop = ceiling $ toRational numberSwaps / 2
--  let prop2 = fitnessFn balls
  

fisherYatesStep :: RandomGen g => (Map Int a, g) -> (Int, a) -> (Map Int a, g)
fisherYatesStep (m, gen) (i, x) = ((insert j x . insert i (m ! j)) m, gen')
  where
    (j, gen') = randomR (0, i) gen
 
fisherYates :: RandomGen g => g -> [a] -> ([a], g)
fisherYates gen [] = ([], gen)
fisherYates gen l = 
  toElems $ Prelude.foldl fisherYatesStep (initial (head l) gen) (numerate (tail l))
  where
    toElems (x, y) = (elems x, y)
    numerate = zip [1..]
    initial x gen = (singleton 0 x, gen)

main = quickCheck prop_halfWrongBalls

