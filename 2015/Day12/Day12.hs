import Text.Parsec
import Data.ByteString.Char8 (ByteString)
import Data.Either (fromRight)

import qualified Data.ByteString.Char8 as BS (readFile)

input = "day12_input.txt"

type Parser = Parsec ByteString ()
type ObjectEntry = (String, Value)

data Value = NumValue Int
           | StrValue String
           | LstValue [Value]
           | ObjValue [ObjectEntry]
           deriving (Show, Eq)

posNumber :: Parser Int
posNumber = read <$> (many1 digit)

negNumber :: Parser Int
negNumber = char '-' >> (negate <$> posNumber)

numValue :: Parser Value
numValue = NumValue <$> (negNumber <|> posNumber)

strValue :: Parser Value
strValue = StrValue <$> (between (char '"') (char '"') (many1 letter))

objEntry :: Parser (String,Value)
objEntry = do
        (StrValue str) <- strValue
        char ':'
        val <- value
        return (str, val)

objValue :: Parser Value
objValue = ObjValue <$>
           between (char '{') (char '}') (objEntry `sepBy` (char ','))

lstValue :: Parser Value
lstValue = LstValue <$>
           between (char '[') (char ']') (value `sepBy` (char ','))

value :: Parser Value
value = numValue <|> strValue <|> objValue <|> lstValue
           
evalWithCheck :: Bool     -- is any check need
              -> String   -- forbiden value
              -> Value    -- JSON value to calculate
              -> Int      -- calculated value
evalWithCheck _ _ (NumValue x) = x
evalWithCheck _ _ (StrValue _) = 0
evalWithCheck b str (LstValue lst) = sum $ map (evalWithCheck b str) lst
evalWithCheck b str (ObjValue lst) = checkObject 0 lst
  where checkObject acc [] = acc
        checkObject acc ((s,v):rest) | b, (StrValue s) <- v, s == str = 0
                                     | otherwise = checkObject (acc + evalWithCheck b str v) rest

solveWith :: FilePath -> (Value -> Int) -> IO ()
solveWith file f = BS.readFile file >>= print . f . fromRight (NumValue 0) . parse value ""

main_part1 = input `solveWith` (evalWithCheck False "")
main_part2 = input `solveWith` (evalWithCheck True "red")