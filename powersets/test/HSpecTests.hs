import PowerSets
import Test.Hspec

main = hspec $ do
        describe "canary" $ do
            it "truth" $ do
                True `shouldBe` True;

        describe "helper functions" $ do
            describe "pieces" $ do
                it "generates " $ do
                    pieces [1,2,3] `shouldBe` [[1,2,3],[2,3],[3],[]]

        describe "powersets" $ do
            it "base case" $ do
                ((powersets []) :: [[Int]]) `shouldBe` [[]];

            it "input of one" $ do
                ((powersets [1]) :: [[Int]]) `shouldBe` [[],[1]];

            it "input of two" $ do
                ((powersets [1,2]) :: [[Int]]) `shouldBe` [[],[1],[2],[1,2]];

            it "input of three" $ do
                ((powersets [1,2,3]) :: [[Int]]) `shouldBe` [[],[1],[2],[3],[1,2],[1,3],[2,3],[1,2,3]];

            it "input of four, by size only" $ do
                toRational (length (powersets [1,2,3,4])) `shouldBe` (toRational $ 2 ** 4) 

            it "by definition, the size of a powerset is 2^n where n is the size of the initial array" $ do
                toRational (length (powersets [1..10])) `shouldBe` (toRational $ 2 ** 10) 

            it "supports repeated elements" $ do
                ((powersets [1,1]) :: [[Int]]) `shouldBe` [[],[1],[1],[1,1]];


