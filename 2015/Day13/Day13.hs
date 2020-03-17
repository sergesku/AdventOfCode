import Text.Parsec
import Data.ByteString.Char8 (ByteString)
import Data.Map (Map)
import Data.Set (Set)
import Data.List (permutations, maximumBy)
import Control.Applicative (liftA2)
import Data.Maybe (fromJust)
import Data.Either (fromRight)
import qualified Data.Map as M
import qualified Data.Set as S
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
solvePuzzle (m,s) = maximum $ (happiness . neighbors) <$> variants
  where (x:xs)    = S.elems s
        variants  = (x:) <$> (permutations xs)
        neighbors = liftA2 zip id (tail . cycle)
        happiness = sum . map fromJust . map (\(n1,n2) -> liftA2 (+) ((n1,n2) `M.lookup` m) ((n2,n1) `M.lookup` m))

main_part1 = BS.readFile input >>= print . solvePuzzle . fromRight (M.empty, S.empty) . runParser inputData (M.empty, S.empty) ""
        