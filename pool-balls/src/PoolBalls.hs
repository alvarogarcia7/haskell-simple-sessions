module PoolBalls where

import Data.Sequence (fromList, update)
import Data.Foldable (toList)

reorder current desired = reorder' current [] where
  reorder' current cumulatedSwaps = 
    if current == desired then
      reverse $ cumulatedSwaps
      else do
          let bestSwap = findBest current desired
          reorder' (apply bestSwap current) (bestSwap:cumulatedSwaps)

findBest :: (Eq a, Ord a) => [a] -> [a] -> [Int]
findBest current desired = do
  let allPossibleSwaps = possibleSwaps $ length current
  let y = map (\swap -> (swap, fitnessFn desired $ apply swap current)) allPossibleSwaps :: [([Int], Int)]
  let best = foldl1 (\p1@(swap, fitness) p2@(swap', fitness') -> if (fitness' > fitness) then p2 else p1) y
  fst best

fitnessFn :: Eq a => [a] -> [a] -> Int
fitnessFn  current desired = do
  let mix = zip current desired
  let different = filter (\(a,b) -> a/=b) mix
  let fitness = -(length different)
  fitness
    

possibleSwaps :: Int -> [[Int]]
possibleSwaps n = [[x,y] | y<-[0..n-1], x<-[0..y-1]]

apply swap@[from, to] balls = do
  let balls' = fromList balls
  let tmp = balls !! from
  let firstReplaced = update from (balls !! to) $ balls'
  let secondReplaced = update to tmp firstReplaced
  toList secondReplaced
  
