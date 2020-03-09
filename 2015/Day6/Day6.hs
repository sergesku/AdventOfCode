{-# LANGUAGE ScopedTypeVariables #-}

import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS (readFile)
import Data.Ix (range)
import Data.Array.ST (MArray, STUArray, readArray, writeArray, newArray)
import Control.Monad (forM, forM_)
import Control.Monad.ST (ST, runST)
import Text.Parsec (Parsec, parse, char, string, digit, try, many, many1, spaces, (<|>))
import Data.Either (fromRight)
import Criterion.Main

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

dictBool :: Action -> (Bool -> Bool)
dictBool TurnOn  = const True
dictBool TurnOff = const False
dictBool _       = not

dictInt :: Action -> (Int -> Int)
dictInt TurnOn  = (+1)
dictInt TurnOff = max 0 . subtract 1
dictInt _       = (+2)

setUpLightsBool ::  [ActionRange] -> Int
setUpLightsBool lst = runST $ do
        arr <- newArray ledRange False
        forM_ lst $ apply arr
        result <- forM (range ledRange) $ readArray arr
        return . length . filter id $ result
  where apply :: STUArray s Point Bool -> ActionRange -> ST s ()
        apply arr (AR action start end) = let f = dictBool action
                                           in forM_ (range (start, end)) $ \p -> readArray arr p >>= writeArray arr p . f

setUpLightsInt ::  [ActionRange] -> Int
setUpLightsInt lst = runST $ do
        arr <- newArray ledRange 0
        forM_ lst $ apply arr
        result <- forM (range ledRange) $ readArray arr
        return . sum $ result
  where apply :: STUArray s Point Int -> ActionRange -> ST s ()
        apply arr (AR action start end) = let f = dictInt action
                                           in forM_ (range (start, end)) $ \p -> readArray arr p >>= writeArray arr p . f            
        
solveWith :: Show a => FilePath -> ([ActionRange] -> a) -> IO ()
solveWith i f = BS.readFile i >>= print . f . fromRight [] . parse actionRanges ""

main_part1 = input `solveWith` setUpLightsBool
main_part2 = input `solveWith` setUpLightsInt

main = defaultMain [ bench "part1" $ nfIO main_part1
                   , bench "part2" $ nfIO main_part2
                   ]