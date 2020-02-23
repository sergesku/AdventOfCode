import Text.Parsec
import Data.Bifunctor (first, second)
import Data.Set (Set)
import Data.ByteString (ByteString)
import Control.Monad.Reader
import qualified Data.ByteString as BS
import qualified Data.Set as S (singleton, size, insert)

input = "./day3_input.txt"

type Pars = ParsecT ByteString DeliveryState (Reader Bool) 

type Location = (Int,Int)

type DeliveryState = ( Turn         -- who moves next
                     , Location     -- Santa Location
                     , Location     -- Robot Location
                     , Set Location -- Path
                     )

data Turn = Santa | Robot deriving (Show, Eq)

modifyStateWith :: (Location -> Location) -> Pars ()
modifyStateWith f = do
                    withRobot <- ask 
                    (turn, santa, robot, set) <- getState
                    let nextSt = case (withRobot, turn) of
                                      (True, Santa) -> let next = f santa
                                                       in (Robot, next, robot, S.insert next set)
                                      (True, Robot) -> let next = f robot
                                                       in (Santa, santa, next, S.insert next set)
                                      _             -> let next = f santa
                                                       in (Santa, next, robot, S.insert next set)
                    putState nextSt

up :: Pars ()
up = char '^' >> modifyStateWith (second (+1))

down :: Pars()
down = char 'v' >> modifyStateWith (second (subtract 1))

left :: Pars ()
left = char '<' >> modifyStateWith (first (subtract 1))

right :: Pars()
right = char '>' >> modifyStateWith (first (+1))

direction :: Pars ()
direction = up <|> down <|> left <|> right

houses :: Bool -> ByteString -> Either ParseError Int
houses b str = runReader (runParserT solver startState "" str) b
  where solver = direction `manyTill` eof >> fmap getSize getState
        getSize (_,_,_,set) = S.size set 
        startState = (Santa, (0,0), (0,0), S.singleton (0,0))

solveWith :: Show a => (ByteString -> a) -> FilePath -> IO ()
solveWith f i = BS.readFile i >>= (print . f)

main_part1 = solveWith (houses False) input
main_part2 = solveWith (houses True)  input
