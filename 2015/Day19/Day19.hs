import Text.Parsec
import Data.List
import Data.List.Split
import Data.Function (on)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS (readFile)

type Parser = Parsec ByteString ()
type StateParser = Parsec String Int
type Rule = (String, String)
type Input = ([Rule], String)

file :: FilePath
file = "c:\\Documents\\WorkDir\\AdventOfCode\\2015\\Day19\\day19_input.txt"

rule :: Parser Rule
rule = do
        a <- anyChar `manyTill` (string " => ")
        b <- anyChar `manyTill` endOfLine
        return (a,b)

rules :: Parser [Rule]
rules = rule `manyTill` endOfLine

puzzleInput :: Parser Input
puzzleInput = (,) <$> rules <*> (many1 anyChar)

variants :: String -> Rule -> [String]
variants str (k,v) = map fn [1..n-1]
    where lst  = splitOn k str
          n    = length lst
          fn x = let (l,r) = splitAt x lst
                  in (intercalate k l ++ v ++ intercalate k r)

                  
nextVariants :: Input -> Int
nextVariants (lst,str) = length . group . sort . concatMap (variants str)  $ lst

{-- 
Part 2 solution has taken from Reddit:
https://www.reddit.com/r/adventofcode/comments/3xflz8/day_19_solutions/cy4h7ji
--}

atom :: StateParser ()
atom = upper *> (many lower) *> modifyState (+1)

atomRnAr :: StateParser ()
atomRnAr = (string "Rn" <|> string "Ar") *> return ()

atomY :: StateParser ()
atomY = string "Y" *> modifyState (subtract 1)

chainLength :: StateParser Int
chainLength = many (try atomRnAr <|> try atomY <|> atom) *> modifyState (subtract 1) *> getState

solveWith :: Show a => FilePath -> (Input -> a) -> IO ()
solveWith file f = BS.readFile file >>= print . fmap f . parse puzzleInput ""
              
main_part1 = file `solveWith` nextVariants
main_part2 = file `solveWith` (runParser chainLength 0 "" . snd)
