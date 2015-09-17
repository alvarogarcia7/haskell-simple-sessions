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


st1 = [0,2,4,6,8]
st2 = [1,3,5,7,9]

ste = [0,1,2,3,4,5,6,7,8,9]

merg :: Ord a => [a] -> [a] -> [a]
merg [] [] = []
merg [] (x:xs) = x : merg [] xs
merg y [] = merg [] y
merg (x:xs) (x':xs') = case x `compare` x' of
    GT -> x': (x  : (merg xs xs'))
    _  -> x : (x' : (merg xs xs'))
