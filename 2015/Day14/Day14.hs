import Text.Parsec
import Data.List
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS (readFile)

input = "day14_input.txt"
time = 2503

type Parser = Parsec ByteString ()

data Reindeer = Reindeer { name      :: String
                         , rideSpeed :: Int
                         , rideTime  :: Int
                         , restTime  :: Int
                         } deriving (Eq, Show)
                        
getNumber :: Parser Int
getNumber = read <$> many1 digit

getName :: Parser String
getName = (:) <$> upper <*> many1 lower

getReindeer :: Parser Reindeer
getReindeer =  Reindeer
           <$> getName   <* string " can fly "
           <*> getNumber <* string " km/s for "
           <*> getNumber <* string " seconds, but then must rest for "
           <*> getNumber <* string " seconds."
    
getAllReindeers :: Parser [Reindeer]
getAllReindeers = getReindeer `sepEndBy` endOfLine

distance :: Int -> Reindeer -> Int
distance t (Reindeer _ rideS rideT restT) = totalRideTime * rideS
  where roundTime           = rideT + restT
        (rounds, remainder) = t `divMod` roundTime
        totalRideTime       = rounds * rideT + (min rideT remainder)

roundWinners :: [Reindeer] -> Int -> [String]
roundWinners lst t = snd $ foldr f (0,[]) lst
  where f d l@(n,ds) | dist == n = (n,(name d):ds)
                     | dist <  n = l
                     | dist >  n = (dist,[name d])
                     where dist = distance t d

maxDistance :: [Reindeer] -> Int -> Int
maxDistance lst t = maximum $ map (distance t) lst                     
                     
maxPoints :: [Reindeer] -> Int -> Int
maxPoints lst t = head . map length . group . sort $ winners
  where winners = concatMap (roundWinners lst) [1..t]

                     
solveWith :: FilePath -> ([Reindeer] -> Int) -> IO ()
solveWith file f = BS.readFile file >>= print . fmap f . parse getAllReindeers ""

main_part1 = input `solveWith` (`maxDistance` time)
main_part2 = input `solveWith` (`maxPoints` time)