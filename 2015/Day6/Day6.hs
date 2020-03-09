{-# LANGUAGE ScopedTypeVariables #-}

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS (readFile)
import Data.Vector.Unboxed (Unbox, Vector)
import qualified Data.Vector.Unboxed as V (filter, length, sum, unsafeFreeze)
import Data.Vector.Unboxed.Mutable as V (unsafeModify, replicate)
import Control.Monad (forM, forM_)
import Control.Monad.ST (ST, runST)
import Text.Parsec (Parsec, parse, char, string, digit, try, many, many1, spaces, (<|>))
import Data.Either (fromRight)

type Point = (Int,Int)
type Parser = Parsec ByteString ()

data Action = TurnOn | TurnOff | Toggle deriving Show
data ActionRange = AR Action Point Point deriving Show

input :: FilePath
input = "./day6_input.txt"

ledRange :: (Point,Point)
ledRange = ((0,0),(999,999))
                        
lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

turnOn :: Parser Action
turnOn = lexeme $ TurnOn <$ string "turn on"

turnOff :: Parser Action
turnOff = lexeme $ TurnOff <$ string "turn off"

toggle :: Parser Action
toggle = lexeme $ Toggle <$ string "toggle"

action :: Parser Action
action = try turnOn <|> try turnOff <|> try toggle

number :: Parser Int
number = read <$> (many1 digit)

point :: Parser Point
point = lexeme $ (,) <$> (number <* char ',') <*> number

actionRanges :: Parser [ActionRange]
actionRanges = many actionRange
    where actionRange = AR <$> action <*> (point <* through) <*> point
          through = lexeme $ string "through"

pointToInt :: Point -> Int
pointToInt (x,y) = 1000 * x + y

rangeToList :: (Point, Point) -> [Int]
rangeToList ((x1,y1),(x2,y2))= [pointToInt (x,y) | x <- [x1..x2], y <- [y1..y2]]

dictBool :: Action -> (Bool -> Bool)
dictBool TurnOn  = const True
dictBool TurnOff = const False
dictBool _       = not

dictInt :: Action -> (Int -> Int)
dictInt TurnOn  = (+1)
dictInt TurnOff = max 0 . subtract 1
dictInt _       = (+2)

countBool :: Vector Bool -> Int
countBool = V.length . V.filter id

setUpLights :: Unbox a 
            => a                        -- function that convert elvish action to appropriate action
            -> (Action -> (a -> a))     -- initial state of leds
            -> (Vector a -> Int)        -- function that count brightness of lights
            -> [ActionRange]            -- just a list of elvish actions
            -> Int
            
setUpLights ini dict counter lst = runST $ do
            vec <- V.replicate 1000000 ini
            forM_ lst $ \ (AR action start stop) -> do
                let f = dict action
                forM_ (rangeToList (start,stop)) (V.unsafeModify vec f)
            v <- V.unsafeFreeze vec
            return $ counter v  
        
        
solveWith :: Show a => FilePath -> ([ActionRange] -> a) -> IO ()
solveWith i f = BS.readFile i >>= print . f . fromRight [] . parse actionRanges ""

main_part1 = input `solveWith` (setUpLights False dictBool countBool)
main_part2 = input `solveWith` (setUpLights 0 dictInt V.sum)