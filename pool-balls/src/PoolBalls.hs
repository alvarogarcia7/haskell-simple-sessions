module PoolBalls where

import Data.Sequence (fromList, update)
import Data.Foldable (toList)

reorder current desired = [[0,1]]

pairs :: Int -> [[Int]]
pairs n = [[x,y] | y<-[0..n], x<-[0..y-1]]

apply swap@[from, to] balls = do
  let balls' = fromList balls
  let tmp = balls !! from
  let firstReplaced = update from (balls !! to) $ balls'
  let secondReplaced = update to tmp firstReplaced
  toList secondReplaced
  
