import Data.Word
import Data.Bits
import qualified Data.Map.Strict as M
import Control.Monad.State
import Text.Parsec hiding (State)
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (asum)
import Data.Either (fromRight)

input = "day7_input.txt"

type Connection = (String, Signal)
type ConnectionMap = M.Map String Signal
type Parser = Parsec ByteString ()

data Signal = Value Word16
            | Wire String
            | And Signal Signal
            | Or  Signal Signal
            | Lshift Signal Int
            | Rshift Signal Int
            | Not Signal
            deriving (Eq, Show)

evalSignal :: Signal -> State ConnectionMap Word16
evalSignal (Value i)  = return i
evalSignal (And s1 s2)  = (.&.) <$> (evalSignal s1) <*> (evalSignal s2)
evalSignal (Or s1 s2)   = (.|.) <$> (evalSignal s1) <*> (evalSignal s2)
evalSignal (Lshift s i) = (`shiftL` i) <$> (evalSignal s)
evalSignal (Rshift s i) = (`shiftR` i) <$> (evalSignal s)
evalSignal (Not s)      = complement   <$> (evalSignal s)
evalSignal (Wire str) = do
                        mp  <- get
                        let (Just x) = M.lookup str mp
                        signal <- evalSignal x
                        modify (M.insert str (Value signal))
                        return signal

lexeme :: Parser a -> Parser a
lexeme p = p <* spaces

integer :: Parser Int
integer = lexeme $ read <$> (many1 digit)
                        
arrow :: Parser ()
arrow = lexeme (string "->") >> return ()

wire :: Parser Signal
wire = Wire <$> (anyChar `manyTill` space)

value :: Parser Signal
value = (Value . fromIntegral) <$> integer

simpleSignal :: Parser Signal
simpleSignal = value <|> wire

andSignal :: Parser Signal
andSignal = And <$> simpleSignal <*> (string "AND " *> simpleSignal)

orSignal :: Parser Signal
orSignal = Or <$> simpleSignal <*> (string "OR " *> simpleSignal)

lShiftSignal :: Parser Signal
lShiftSignal = Lshift <$> simpleSignal <*> (string "LSHIFT " *> integer)

rShiftSignal :: Parser Signal
rShiftSignal = Rshift <$> simpleSignal <*> (string "RSHIFT " *> integer)

notSignal :: Parser Signal
notSignal = Not <$> (string "NOT " >> wire)

signal :: Parser Signal
signal = asum $ try <$> [notSignal, andSignal, orSignal, lShiftSignal, rShiftSignal, simpleSignal]

connection :: Parser Connection
connection = do s <- signal
                arrow
                w <- anyChar `manyTill` (() <$ endOfLine <|> eof)
                return (w,s)

connectionMap :: Parser ConnectionMap
connectionMap = M.fromList <$> many connection

solveWire :: String -> ByteString -> Word16 
solveWire str = evalState (evalSignal (Wire str)) . fromRight M.empty . parse connectionMap ""

solveWith :: Show a => FilePath -> (ByteString -> a) -> IO ()
solveWith i f = BS.readFile i >>= print . f

main_part1 = input `solveWith` (solveWire "a")