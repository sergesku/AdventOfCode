import Data.List (group)

input = "1113222113"

lookAndSay :: String -> String
lookAndSay = concatMap f . group
  where f x = show (length x) ++ [head x]

lengthAfterN :: String -> Int -> Int
lengthAfterN str n = length $ (iterate lookAndSay str) !! n

main_part1 = lengthAfterN input 40
main_part2 = lengthAfterN input 50
