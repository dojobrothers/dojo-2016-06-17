module Output (outputNumber) where

import Data.List (transpose)

-- | A digit from the scanned document.
data Digit = Digit String String String

-- | Smart construtor for 'Digit'.
digit :: String -> String -> String -> Digit
digit top middle bottom
  | all ((== 3) . length) [top, middle, bottom] = Digit top middle bottom
  | otherwise = error "Digits should have strings of the same length."

-- | List of all digits.
digits :: [Digit]
digits = process input
  where
    process = map (\[top, middle, bottom] -> digit top middle bottom) . transpose
    input = [ [" _ ","   "," _ "," _ ","   "," _ "," _ "," _ "," _ "," _ "]
            , ["| |","  |"," _|"," _|","|_|","|_ ","|_ ","  |","|_|","|_|"]
            , ["|_|","  |","|_ "," _|","  |"," _|","|_|","  |","|_|"," _|"] ]

-- | Print a number as the scanner would scan it.
outputNumber :: Int -> String
outputNumber = glue . map toDigit . reverse . separate
  where
    separate :: Int -> [Int]
    separate n | n < 0     = error "Can't handle negative numbers."
               | n < 10    = [n]
               | otherwise = n `mod` 10 : separate (n `div` 10)

    toDigit :: Int -> Digit
    toDigit n | n >= 0 && n <= 9 = digits !! n
              | otherwise        = error $ show n ++ " can't be a digit."

    glue :: [Digit] -> String
    glue = unlines . map concat . transpose . map (\(Digit top middle bottom) -> [top, middle, bottom])
