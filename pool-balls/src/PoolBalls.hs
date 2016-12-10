module PoolBalls where

pairs :: Int -> [[Int]]
pairs n = [[x,y] | y<-[0..n], x<-[0..y-1]]

