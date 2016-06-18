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

  describe "inputDigit" $ do
    it "works for 1 only!" $
      inputDigit
        "   "
        "  |"
        "  |"

        `shouldBe`
        1

    it "works for 2 only!" $
      inputDigit
        " _ "
        " _|"
        "|_ "
        `shouldBe`
        2

  describe "inputTwoDigits" $ do
    it "works for 12" $
      inputTwoDigits
        "    _ \n\
        \  | _|\n\
        \  ||_ \n"
        `shouldBe`
        12
    it "works for 21" $
      inputTwoDigits
        " _    \n\
        \ _|  |\n\
        \|_   |\n"
        `shouldBe`
        21

 describe "split digits " $ do
   it "splits the motherfucker 12 into 1 and 2" $
    splitDigits
           "    _    _   \n\
           \  | _| | _| |\n\
           \  ||_  ||_  |\n"
           `shouldBe`


  -- describe "inputNumber" $ do
  --   it "works for 12121" $
  --     inputNumber
  --       "    _    _   \n\
  --       \  | _| | _| |\n\
  --       \  ||_  ||_  |\n"
  --       `shouldBe`
  --       12121
