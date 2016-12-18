import PoolBalls

import Debug.Trace
import Text.Printf (printf)

import Test.QuickCheck
import System.Random
import Data.Map
 

prop_halfWrongBalls :: String -> String -> Bool
prop_halfWrongBalls balls b= do
  let shuffled = fst $ fisherYates (mkStdGen 0) balls
  let numberOfSwaps = length $ reorder balls shuffled
  let wrongBalls = - (fitnessFn balls shuffled) :: Int
  let halfWrongBalls = ceiling ((toRational wrongBalls) / (toRational 2))
  let satisfiesProperty =  numberOfSwaps <= halfWrongBalls
  let info = pprint [("original", balls), ("shuffled", shuffled), ("halfWrongBalls", show halfWrongBalls), ("swaps", show numberOfSwaps), ("applies", show satisfiesProperty)] 
    in trace info satisfiesProperty

pprint xs = unlines $ Prelude.map (\(a,b) -> a++(show b)) xs
  

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
