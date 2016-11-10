import Test.Hspec
import Test.QuickCheck

main :: IO ()
main = do
  hspec $ do
    describe "Canary Test" $ do
        it "should be green" $ do
          True `shouldBe` True

    describe "sorting stacks" $ do
        it "should sort one empty stack" $ do
	      merg [1] [] `shouldBe` [1]
	      merg [] [1] `shouldBe` [1]

        describe "should sort stacks with only one element" $ do
          it "the order does not matter" $ do
            merg [1] [2] `shouldBe` [1,2]
            merg [2] [1] `shouldBe` [1,2]

        describe "should sort stacks with more than one element" $ do
          it "the order does not matter" $ do
            merg [1,2] [2] `shouldBe` [1,2,2]
            merg [2,3] [1] `shouldBe` [1,2,3]
            merg [2,3] [1,3,3,3,4] `shouldBe` [1,2,3,3,3,3,4]

        describe "acceptance test" $ do
          it "should sort stacks" $ do
            merg [0,2..8] [1,3..9] `shouldBe` [0..9]

merg :: Ord a => [a] -> [a] -> [a]
merg [] [] = []
merg [] (x:xs) = x : merg [] xs
merg y [] = merg [] y
merg (x:xs) (x':xs') = case x `compare` x' of
    GT -> x': (x  : (merg xs xs'))
    _  -> x : (x' : (merg xs xs'))
