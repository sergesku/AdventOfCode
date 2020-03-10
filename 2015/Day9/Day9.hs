import Text.Parsec                           
import Data.Map                              (Map)
import Data.Set                              (Set)
import Data.List                             (permutations)
import Control.Applicative                   (liftA2)
import Data.Either                           (fromRight)
import Data.ByteString.Char8                 (ByteString)
import qualified Data.ByteString.Char8 as BS (split, readFile)
import qualified Data.Map              as M  (empty, insert, lookup)
import qualified Data.Set              as S  (empty, insert, elems)

type Point   = String
type Edge = (Point,Point)
type Route   = [Point]
type Graph   = (Map Edge Int, Set Point)
type Parser  = Parsec ByteString Graph

input = "day9_input.txt"                                   

emptyGraph :: Graph
emptyGraph = (M.empty, S.empty)
                                   
number :: Parser Int
number = read <$> many1 digit
                                   
point :: Parser Point
point = (:) <$> upper <*> (manyTill anyChar space)

edge :: Parser Edge
edge = (,) <$> point <*> (string "to " *> point)

entry :: Parser ()
entry = do
        e@(p1,p2) <- edge <* string "= "
        n         <- number
        (m,s)     <- getState
        let newSet = S.insert p1 . S.insert p2 $ s
            newMap = M.insert e n . M.insert (p2,p1) n $ m
        putState (newMap, newSet)

graph :: Parser Graph
graph = sepEndBy1 entry endOfLine >> getState

getDistances :: Graph -> [Int]
getDistances (m,s) = do
        route <- permutations . S.elems $ s
        let routeToEdges = liftA2 zip id tail
            Just ints    = traverse (`M.lookup` m) . routeToEdges $ route 
        return $ sum ints

solveWith :: FilePath -> ([Int] -> Int) -> IO ()
solveWith i f = BS.readFile i >>= print . f . getDistances . fromRight emptyGraph . runParser graph emptyGraph ""

main_part1 = input `solveWith` minimum
main_part2 = input `solveWith` maximum