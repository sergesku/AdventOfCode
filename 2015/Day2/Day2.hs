import Data.List.Split (splitOn)
import Data.List (sort)

input = "../input/day2_input.txt"

type Box = (Int,Int,Int)

parseBox :: String -> Box
parseBox str = (a,b,c)
  where [a,b,c] = sort . map read $ splitOn "x" str

paperForBox :: Box -> Int
paperForBox (a,b,c) = 2 * (side_1 + side_2 + side_3) + side_1
  where side_1   = a * b
        side_2   = b * c
        side_3   = a * c

totalPaper :: String -> Int
totalPaper = sum . map (paperForBox . parseBox) . words

ribbonForBox :: Box -> Int
ribbonForBox (a,b,c) = 2 * (a + b) + a*b*c

totalRibbon :: String -> Int
totalRibbon = sum . map (ribbonForBox . parseBox) . words
        
solveWith :: Show a => (String -> a) -> FilePath -> IO ()
solveWith f i = readFile i >>= (print . f)

main_part1 = solveWith totalPaper input
main_part2 = solveWith totalRibbon input
