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

reorderMax current desired = reorder' current [] [] where
  reorder' current cumulatedSwaps pastStates = 
    if current == desired then
      reverse $ cumulatedSwaps
      else do
          let visitedStates = current:pastStates
          let bestSwap = findWorst current desired visitedStates
          reorder' (apply bestSwap current) (bestSwap:cumulatedSwaps) visitedStates

findWorst :: (Eq a, Ord a) => [a] -> [a] -> [[a]]-> [Int]
findWorst current desired pastStates = do
  let allPossibleSwaps = possibleSwaps $ length current
  let swapAndItsResult = map (\swap -> (swap, apply swap current)) allPossibleSwaps
  let notIn needle haystack = not $ any (==needle) haystack
  let notVisitedStates = filter (\p@(swap, result) -> notIn result pastStates) swapAndItsResult
  let swapAndItsFitness = map (\(swap, newState) -> (swap, fitnessFn desired newState)) notVisitedStates :: [([Int], Int)]
  let minFitnessLevel = fitnessFn desired current
  let acceptableSwaps = filter (\(_, fitness) -> fitness >= minFitnessLevel) swapAndItsFitness
  let minByFitness = (\first@(swap, fitness) second@(swap', fitness') -> if (fitness <= fitness') then first else second)
  let best = foldl1 minByFitness acceptableSwaps
  fst best


findBest :: (Eq a, Ord a) => [a] -> [a] -> [Int]
findBest current desired = do
  let allPossibleSwaps = possibleSwaps $ length current
  let swapAndItsFitness = map (\swap -> (swap, fitnessFn desired $ apply swap current)) allPossibleSwaps :: [([Int], Int)]
  let maxByFitness = (\first@(swap, fitness) second@(swap', fitness') -> if (fitness > fitness') then first else second)
  let best = foldl1 maxByFitness swapAndItsFitness
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
  
