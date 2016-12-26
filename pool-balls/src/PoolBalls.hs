module PoolBalls where

import Data.Sequence (fromList, update)
import Data.Foldable (toList)

reorder :: Ord a => [a] -> [a] -> [[Int]]
reorder current desired = reorder' current [] where
  reorder' currentState cumulatedSwaps =
    if currentState == desired then
      reverse $ cumulatedSwaps
      else do
          let bestSwap = findBest currentState desired
          reorder' (apply bestSwap currentState) (bestSwap:cumulatedSwaps)

reorderMax :: Ord a => [a] -> [a] -> [[Int]]
reorderMax current desired = reorder' current [] [] where
  reorder' currentState cumulatedSwaps pastStates =
    if currentState == desired then
      reverse $ cumulatedSwaps
      else do
          let visitedStates = currentState:pastStates
          let bestSwap = findWorst currentState desired visitedStates
          reorder' (apply bestSwap currentState) (bestSwap:cumulatedSwaps) visitedStates

findWorst :: (Ord a) => [a] -> [a] -> [[a]]-> [Int]
findWorst current desired pastStates = do
  let allPossibleSwaps = possibleSwaps $ length current
  let swapAndItsResult = map (\swap -> (swap, apply swap current)) allPossibleSwaps
  let notIn needle haystack = not $ any (==needle) haystack
  let notVisitedStates = filter (\(_, result) -> notIn result pastStates) swapAndItsResult
  let swapAndItsFitness = map (\(swap, newState) -> (swap, fitnessFn desired newState)) notVisitedStates :: [([Int], Int)]
  let minFitnessLevel = fitnessFn desired current
  let atLeast level = (level >= minFitnessLevel)
  let acceptableSwaps = filter (\(_, fitness) -> atLeast fitness) swapAndItsFitness
  let minByFitness = (\first@(_, fitness) second@(_, fitness') -> if (fitness <= fitness') then first else second)
  let worst = foldl1 minByFitness acceptableSwaps
  fst worst

findBest :: (Ord a) => [a] -> [a] -> [Int]
findBest current desired = do
  let allPossibleSwaps = possibleSwaps $ length current
  let swapAndItsFitness = map (\swap -> (swap, fitnessFn desired $ apply swap current)) allPossibleSwaps :: [([Int], Int)]
  let maxByFitness = (\first@(_, fitness) second@(_, fitness') -> if (fitness > fitness') then first else second)
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

apply :: [Int] -> [a] -> [a]
apply [from, to] balls = do
  let balls' = fromList balls
  let tmp = balls !! from
  let firstReplaced = update from (balls !! to) $ balls'
  let secondReplaced = update to tmp firstReplaced
  toList secondReplaced
apply _ _ = error "wrong ball swap"
  
