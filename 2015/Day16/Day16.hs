{-# LANGUAGE OverloadedStrings #-}

{--
Sue 102: pomeranians: 6, trees: 1, samoyeds: 4
Sue 103: cars: 2, perfumes: 1, goldfish: 5
Sue 104: goldfish: 2, cars: 8, pomeranians: 2
Sue 105: goldfish: 6, vizslas: 0, trees: 10
Sue 106: trees: 10, akitas: 10, pomeranians: 0
--}
import Text.Parsec
import Data.Map (Map)
import Data.ByteString.Char8 (ByteString)
import qualified Data.Map              as M
import qualified Data.ByteString.Char8 as BS

type Entry = (String, Int)
type Parser = Parsec ByteString ()

data Sue = Sue { number :: Int
               , things :: [Entry]
               } deriving (Show)

input = "day16_input.txt"
               
outputMFCSAM :: Map String Int
outputMFCSAM = M.fromList [ ("children", 3)
                          , ("cats", 7)
                          , ("samoyeds", 2)
                          , ("pomeranians", 3)
                          , ("akitas", 0)
                          , ("vizslas", 0)
                          , ("goldfish", 5)
                          , ("trees", 3)
                          , ("cars", 2)
                          , ("perfumes", 1)
                          ]

thing :: Parser String
thing = choice $ map (try . string)(M.keys outputMFCSAM)

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
                          
checkEntry :: Map String Int -> (String, Int) -> Bool
checkEntry m (s,i) = case (==i) <$> (M.lookup s m) of
                       (Just True) -> True
                       _           -> False

checkSue :: Map String Int -> Sue -> Bool
checkSue m sue = and $ map (checkEntry m) (things sue)


findSue :: Map String Int -> [Sue] -> Sue
findSue m = head . filter (checkSue m)

main_part1 = BS.readFile input >>= print . fmap (number . findSue outputMFCSAM) . parse allSues ""