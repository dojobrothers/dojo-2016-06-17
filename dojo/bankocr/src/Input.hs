module Input where

someFunc :: IO ()
someFunc = putStrLn "someFunc"
inputNumberOne :: String->String->String->Int
inputNumberOne t m b
  | t=="   " && m=="  |" && b=="  |" = 1
  | otherwise = error "inputNumberOne: cant handle this digit"

inputNumberTwo = t m b
  | t==" _ " && m=="  |" && b=="  |" = 1
  | otherwise = error "inputNumberOne: cant handle this digit"
