module PoolBalls where

import Data.Sequence (fromList, update)
import Data.Foldable (toList)

reorder current desired = reorder' current [] where
  reorder' current cumulatedSwaps = 
    if (fit current desired) == 0 then
      reverse $ cumulatedSwaps
      else do
          let bestSwap = findBest current desired
          reorder' (apply bestSwap current) (bestSwap:cumulatedSwaps)

findBest :: (Eq a, Ord a) => [a] -> [a] -> [Int]
findBest current desired = do
  let allPairs = pairs $ length current
  let y = (map (\swap -> (swap, fit desired $ apply swap current)) allPairs :: [([Int], Int)])
  let best = foldl1 (\(swap,fitness) (swap',fitness') -> if (fitness' > fitness) then (swap',fitness') else (swap,fitness)) y
  fst best

fit:: Eq a => [a] -> [a] -> Int
fit current desired = do
  let mix = zip current desired
  let different = filter (\(a,b) -> a/=b) mix
  let fitness = -(length different)
  fitness
    

pairs :: Int -> [[Int]]
pairs n = [[x,y] | y<-[0..n-1], x<-[0..y-1]]

apply swap@[from, to] balls = do
  let balls' = fromList balls
  let tmp = balls !! from
  let firstReplaced = update from (balls !! to) $ balls'
  let secondReplaced = update to tmp firstReplaced
  toList secondReplaced
  
