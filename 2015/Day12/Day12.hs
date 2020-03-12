import Text.Parsec
import Data.ByteString.Char8 (ByteString)
import Data.Either (fromRight)

import qualified Data.ByteString.Char8 as BS (readFile)

input = "day12_input.txt"

type Parser = Parsec ByteString ()

posNumber :: Parser Int
posNumber = read <$> (many1 digit)

negNumber :: Parser Int
negNumber = char '-' >> (negate <$> posNumber)

number :: Parser Int
number = negNumber <|> posNumber


allNumbers :: Parser [Int]
allNumbers = many1 ((try number)
                  <|> try (anyChar `manyTill` (try (lookAhead number)) >> number) <|> (eof >> return 0))


solution :: ByteString -> Int
solution = sum . fromRight [] . parse allNumbers ""

main_part1 :: IO ()
main_part1 = BS.readFile input >>= print . solution
