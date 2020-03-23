{-# LANGUAGE OverloadedStrings #-}

import Text.Parsec
import Data.Function                         ((&))
import Data.Map                              (Map)
import Data.ByteString.Char8                 (ByteString)
import qualified Data.Map              as M  (fromList, insert, lookup, keys)
import qualified Data.ByteString.Char8 as BS (readFile)

type Entry = (String, Int)
type Parser = Parsec ByteString ()

data Sue = Sue { number :: Int
               , things :: [Entry]
               } deriving (Show)

input = "day16_input.txt"
               
outputMFCSAM_1 :: Map String (Int -> Bool)
outputMFCSAM_1 = M.fromList [ ("children", (== 3))
                            , ("cats", (== 7))
                            , ("samoyeds", (== 2))
                            , ("pomeranians", (== 3))
                            , ("akitas", (== 0))
                            , ("vizslas", (== 0))
                            , ("goldfish", (== 5))
                            , ("trees", (== 3))
                            , ("cars", (== 2))
                            , ("perfumes", (== 1))
                            ]

outputMFCSAM_2 :: Map String (Int -> Bool)
outputMFCSAM_2 = M.fromList [ ("children", (== 3))
                            , ("cats", (> 7))
                            , ("samoyeds", (== 2))
                            , ("pomeranians", (< 3))
                            , ("akitas", (== 0))
                            , ("vizslas", (== 0))
                            , ("goldfish", (< 5))
                            , ("trees", (> 3))
                            , ("cars", (== 2))
                            , ("perfumes", (== 1))
                            ]                       
                                                  
thing :: Parser String
thing = choice $ map (try . string)(M.keys outputMFCSAM_1)

entry :: Parser Entry
entry = (,)
      <$> (thing <* string ": ")
      <*> integer

integer :: Parser Int
integer = read <$> (many1 digit)

sue :: Parser Sue
sue = Sue
    <$> (string "Sue " *> integer <* string ": ")
    <*> (entry `sepEndBy` string ", ")

allSues :: Parser [Sue]
allSues = sue `sepEndBy` endOfLine
                          
checkEntry :: Map String (Int -> Bool) -> (String, Int) -> Bool
checkEntry m (s,i) = case (i &) <$> (M.lookup s m) of
                       (Just True) -> True
                       _           -> False

checkSue :: Map String (Int -> Bool) -> Sue -> Bool
checkSue m sue = and $ map (checkEntry m) (things sue)


findSue :: Map String (Int -> Bool) -> [Sue] -> Sue
findSue m = head . filter (checkSue m)

solveWith :: FilePath -> Map String (Int -> Bool) -> IO ()
solveWith file m = BS.readFile file >>= print . fmap (number . findSue m) . parse allSues ""

main_part1 = input `solveWith` outputMFCSAM_1
main_part2 = input `solveWith` outputMFCSAM_2