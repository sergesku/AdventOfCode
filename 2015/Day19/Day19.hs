import Text.Parsec
import Data.List
import Data.List.Split
import Data.Function (on)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS (readFile)

type Parser = Parsec ByteString ()
type Replacement = (String, String)
type Input = ([Replacement], String)

file :: FilePath
file = "c:\\Documents\\WorkDir\\AdventOfCode\\2015\\Day19\\day19_input.txt"

replacement :: Parser Replacement
replacement = do
        a <- anyChar `manyTill` (string " => ")
        b <- anyChar `manyTill` endOfLine
        return (a,b)

allReplacements :: Parser [Replacement]
allReplacements = replacement `manyTill` endOfLine

puzzleInput :: Parser Input
puzzleInput = (,) <$> allReplacements <*> (many1 anyChar)

variants :: String -> Replacement -> [String]
variants str (k,v) = map fn [1..n-1]
    where lst  = splitOn k str
          n    = length lst
          fn x = let (l,r) = splitAt x lst
                  in (intercalate k l ++ v ++ intercalate k r)

allVariants :: String -> [Replacement] -> [String]
allVariants str = concatMap (variants str)

main_part1 = BS.readFile file >>= print . fmap fn . parse puzzleInput ""
  where fn = \(lst, str) -> length . group . sort . allVariants str $ lst