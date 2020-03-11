import Data.List

input = "1113222113"

lookAndSay :: String -> String
lookAndSay = concatMap f . group
  where f x = show (length x) ++ [head x]

main_part1 = length $ (iterate lookAndSay input) !! 40