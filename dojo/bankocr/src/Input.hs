module Input where

import Data.List (transpose)

someFunc :: IO ()
someFunc = putStrLn "someFunc"

inputDigit :: String -> String -> String -> Int
inputDigit "   "
           "  |"
           "  |" = 1
inputDigit " _ "
           " _|"
           "|_ " = 2
inputDigit " _ "
           " _|"
           " _|" = 3
inputDigit "   "
           "|_|"
           "  |" = 4
inputDigit " _ "
           "|_ "
           " _|" = 5
inputDigit " _ "
           "|_ "
           "|_|" = 6
inputDigit " _ "
           "  |"
           "  |" = 7
inputDigit " _ "
           "|_|"
           "|_|" = 8
inputDigit " _ "
           "|_|"
           " _|" = 9
inputDigit " _ "
           "| |"
           "|_|" = 0
inputDigit t m b = error $ "inputDigit: cant handle this digit " ++ show (t, m, b)

inputTwoDigits :: String -> Int
inputTwoDigits input =
  inputDigit (take 3 t) (take 3 m) (take 3 b) * 10 +
  inputDigit (drop 3 t) (drop 3 m) (drop 3 b)
  where
    [t, m, b] = lines input

inputNumber :: String -> Int
inputNumber = glue . reverse . map fromDigit . splitDigits
  where
    fromDigit :: [String] -> Int
    fromDigit [t, m, b] = inputDigit t m b
    glue = foldr (\x r -> x + 10 * r) 0

splitDigits :: String -> [[String]]
splitDigits = transpose . map splitBy3 . lines
  where
    splitBy3 [] = []
    splitBy3 xs = take 3 xs : splitBy3 (drop 3 xs)
