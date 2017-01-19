import BookGeneration
import Test.Hspec
import Debug.Trace

main = hspec $ do
    describe "generates carts" $ do
        it "no books turns into a single cart" $
            (cartsFor [] :: [[[Int]]]) `shouldBe` ([[]] :: [[[Int]]])

        it "counts carts" $ do
            length (cartsFor [1]) `shouldBe` 1;
            length (cartsFor [1,2]) `shouldBe` 2;

        describe "does not matter which elements are present" $ do
            it "all different" $ do
                length (cartsFor [1,2,3]) `shouldBe` 5;
            it "some repetition" $ do
                length (cartsFor [1,2,1]) `shouldBe` 5;
            it "all the same" $ do
                length (cartsFor [1,1,1]) `shouldBe` 5;

