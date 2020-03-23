import Control.Monad (filterM)

input = "day17_input.txt"

totalEggnog :: Int
totalEggnog = 150

main_part1 = readFile input >>= print . length . filter ((== totalEggnog) . sum) . filterM (\_ -> [True, False]) . fmap read . words