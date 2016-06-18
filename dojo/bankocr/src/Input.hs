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
inputDigit _ _ _ = error "inputDigit: cant handle this digit"

inputTwoDigits :: String -> Int
inputTwoDigits input =
  inputDigit (take 3 t) (take 3 m) (take 3 b) * 10 +
  inputDigit (drop 3 t) (drop 3 m) (drop 3 b)
  where
    [t, m, b] = lines input

inputNumber :: String -> Int
inputNumber = glue . reverse . map fromDigit . splitDigits
  where
    fromDigit [t, m, b] = inputDigit t m b
    glue []     = 0
    glue (x:xs) = x + 10 * glue xs

splitDigits :: String -> [[String]]
splitDigits = transpose . map splitBy3 . lines
  where
    splitBy3 [] = []
    splitBy3 xs = take 3 xs : splitBy3 (drop 3 xs)
