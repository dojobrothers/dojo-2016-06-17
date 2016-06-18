module Input where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
inputDigit :: String->String->String->Int
inputDigit t m b
  | t=="   " && m=="  |" && b=="  |" = 1
  | t==" _ " && m==" _|" && b=="|_ " = 2
  | otherwise = error "inputDigit: cant handle this digit"

inputNumberTwelve t m b
  | inputDigit (take 3 t) (take 3 m) (take 3 b) * 10 +
    inputDigit (drop 3 t) (drop 3 m) (drop 3 b) == 12 = 12
  | otherwise = error "inputNumberTwelve: not twelve"
