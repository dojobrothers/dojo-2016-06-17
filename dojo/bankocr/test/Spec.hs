import Test.Hspec
import Test.QuickCheck

import Input
import Output

main :: IO ()
main = hspec $ do
  describe "Prelude.head" $ do
    it "returns the first element of a list" $ --do
      head [23 ..] `shouldBe` (23 :: Int)

    it "returns the first element of an *arbitrary* list" $
      property $ \x xs -> head (x:xs) == (x :: Int)

  describe "outputNumber" $ do
    it "works for 1234567890" $
      outputNumber 1234567890 `shouldBe`
        "    _  _     _  _  _  _  _  _ \n\
        \  | _| _||_||_ |_   ||_||_|| |\n\
        \  ||_  _|  | _||_|  ||_| _||_|\n"

    it "works for 490067715" $
      outputNumber 490067715 `shouldBe`
        "    _  _  _  _  _  _     _ \n\
        \|_||_|| || ||_   |  |  ||_ \n\
        \  | _||_||_||_|  |  |  | _|\n"
