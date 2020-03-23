import Data.List     (sortOn, groupBy)
import Data.Function (on)
import Control.Monad (filterM)

input :: FilePath
input = "day17_input.txt"

totalEggnog :: Int
totalEggnog = 150

combinations :: Int -> [Int] -> [[Int]]
combinations i = filter ((== i) . sum) . filterM (\_ -> [True, False]) 

minCombinations :: [[Int]] -> [[Int]]
minCombinations = head . groupBy ((==) `on` length) . sortOn length

solveWith :: FilePath -> ([Int] -> [[Int]]) -> IO ()
solveWith file f = readFile file >>= print . length . f . fmap read . words

main_part1 = input `solveWith` (combinations totalEggnog)
main_part2 = input `solveWith` (minCombinations . combinations totalEggnog)