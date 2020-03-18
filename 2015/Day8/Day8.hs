
input = "day8_input.txt"

countDifference :: Int -> String -> Int
countDifference n [] = n
countDifference n ('\\' : '\\' : rest)        = countDifference (n + 1) rest
countDifference n ('\\' : '"' : rest)         = countDifference (n + 1) rest
countDifference n ('\\' : 'x' : _ : _ : rest) = countDifference (n + 3) rest
countDifference n ('"' : rest )               = countDifference (n + 1) rest
countDifference n (_ : rest)               = countDifference n rest

main = readFile input >>= print . sum . map (countDifference 0) . lines