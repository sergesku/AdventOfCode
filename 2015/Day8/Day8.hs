input = "day8_input.txt"

differenceDecoded :: Int -> String -> Int
differenceDecoded n [] = n
differenceDecoded n ('\\' : '\\' : rest)        = differenceDecoded (n + 1) rest
differenceDecoded n ('\\' : '"' : rest)         = differenceDecoded (n + 1) rest
differenceDecoded n ('\\' : 'x' : _ : _ : rest) = differenceDecoded (n + 3) rest
differenceDecoded n ('"'  : rest )              = differenceDecoded (n + 1) rest
differenceDecoded n ( _  : rest)                = differenceDecoded n rest

differenceEncoded :: Int -> String -> Int
differenceEncoded n [] = n
differenceEncoded n ('"' : rest)  = differenceEncoded (n + 1) rest
differenceEncoded n ('\\' : rest) = differenceEncoded (n + 1) rest
differenceEncoded n ( _ : rest )  = differenceEncoded n rest

solveWith :: FilePath -> (String -> Int) -> IO ()
solveWith file f = readFile file >>= print . sum . map f . lines

main_part1 = input `solveWith` (differenceDecoded 0)
main_part2 = input `solveWith` (differenceEncoded 2)
