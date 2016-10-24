import Test.Hspec
import Test.QuickCheck
import Data.List

main = hspec $ do
    describe "Canary Test" $ do
      it "should be green" $ do
          True `shouldBe` True

    describe "permutation" $ do
      it "of an empty array should be empty" $ do
        null (permutate []) `shouldBe` True

      it "of a 1-element array should be unique" $ do
        (permutate [1]) `shouldBe` ([[1]] :: [[Int]])

      describe "of a 2-element array" $ do
        it "no repeated elements" $ do
          (permutate [1,2]) `shouldBe` ([[1,2], [2,1]] :: [[Int]])

        it "with repeated elements" $ do
          (permutate [3,3]) `shouldBe` ([[3,3], [3,3]] :: [[Int]]) --because permutations are performed on sets, therefore the two threes (3) are different


      describe "of a 3-element array" $ do
        it "no repeated elements" $ do
          (permutate [1,2,3]) `shouldBeInAnyOrder` ([[1,2,3], [1,3,2], [2,1,3], [2,3,1], [3,1,2], [3,2,1]] :: [[Int]])


shouldBeInAnyOrder actual expected = 
   (all (==True) $ (map (containedIn expected) actual ++ map (containedIn actual) expected)) `shouldBe` True where
        containedIn expecteds actual = any (==actual) expecteds


permutate :: [a] -> [[a]]
permutate [] = []
permutate x = permutate' x [[]] where
  permutate' elements accumulated = case length elements of
    1 -> [elements]
    2 -> [elements, reverse elements]
    n -> foldl (++) [] $ map (\comb -> addInAllPositions (head x) comb ) $ permutate' (tail x) accumulated


addInAllPositions element array = map (\position -> setAt array position element) (reverse [0..(length array)])

-- TODO AGB Replace function with Data.List.Tools::setAt
setAt :: [a] -> Int -> a -> [a]
setAt [] 0 ele = [ele]
setAt [] _ _ = []
setAt coll 0 ele = ele:coll
setAt (x:xs) n ele = x:(setAt xs (n-1) ele)

