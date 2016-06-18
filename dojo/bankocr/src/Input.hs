module Input where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
inputNumberOne :: String->String->String->Int
inputNumberOne t m b
  | t=="   " && m=="  |" && b=="  |" = 1
  | otherwise = error "inputNumberOne: cant handle this digit"

inputNumberTwo t m b
  | t==" _ " && m==" _|" && b=="|_ " = 2
  | otherwise = error "inputNumberTwo: cant handle this digit"

inputNumberTwelve t m b
  | inputNumberOne (take 3 t) (take 3 m) (take 3 b) * 10 +
    inputNumberTwo (drop 3 t) (drop 3 m) (drop 3 b) == 12 = 12
  | otherwise = error "inputNumberTwelve: not twelve"
