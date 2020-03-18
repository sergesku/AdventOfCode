import Text.Parsec
import Data.ByteString.Char8 (ByteString)
import Data.Set (Set)
import Data.Map.Strict (Map)
import Data.List (permutations, maximumBy)
import qualified Data.Set as S (empty, elems, insert)
import qualified Data.Map.Strict as M (empty, findWithDefault, insert)
import qualified Data.ByteString.Char8 as BS (readFile)

type Name = String
type Neighbors = (Name, Name)
type Parser = Parsec ByteString InputData
type InputData = (Map Neighbors Int, Set Name)

input = "day13_input.txt"

number :: Parser Int
number = string "gain " *> digits <|>
         string "lose " *> (negate <$> digits)
   where digits = read <$> (many digit)

name :: Parser String
name = (:) <$> upper <*> (many1 lower)

statement :: Parser ()
statement = do 
            n1 <- name <* string " would "
            num <- number <* string " happiness units by sitting next to "
            n2 <- name <* char '.'
            modifyState $ \(m,s) -> (M.insert (n1,n2) num m, S.insert n1 (S.insert n2 s))

inputData :: Parser InputData
inputData = statement `sepEndBy` endOfLine *> getState

solvePuzzle :: InputData -> Int
solvePuzzle (m,s) = maximum $ (totalHappiness . neighbors) <$> variants
  where (x:xs)                     = S.elems s
        variants                   = (x:) <$> (permutations xs)
        neighbors lst              = zip lst (tail (cycle lst))
        neighborsHappiness (n1,n2) = (M.findWithDefault 0 (n1,n2) m) + (M.findWithDefault 0 (n2,n1) m) 
        totalHappiness             = sum . map neighborsHappiness

solvePuzzleWith :: [Name] -> InputData -> Int
solvePuzzleWith lst (m,s) = solvePuzzle (m,newS)
  where newS = foldr S.insert s lst 

solveWith :: FilePath -> (InputData -> Int) -> IO ()
solveWith file f = BS.readFile file >>= print . fmap f . runParser inputData (M.empty, S.empty) ""

main_part1 = input `solveWith`(solvePuzzle) 
main_part2 = input `solveWith`(solvePuzzleWith ["Me"])