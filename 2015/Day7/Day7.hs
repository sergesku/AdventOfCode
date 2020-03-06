import Data.Word
import Data.Bits
import qualified Data.Map as M
import Control.Monad.State
import Text.Parsec hiding (State)
import Data.ByteString.Char8 (ByteString)
import Data.Foldable (asum)

input = "day7_input.txt"

type Instruction = (String, Signal)
type WireMap = M.Map String Signal
type Parser = Parsec ByteString ()

data Signal = Value Word16
            | Wire String
            | And Signal Signal
            | Or  Signal Signal
            | Lshift Signal Int
            | Rshift Signal Int
            | Not Signal
            deriving (Eq, Show)

evalSignal :: Signal -> State WireMap Word16
evalSignal (Value i)  = return i
evalSignal (And s1 s2)  = (.&.) <$> (evalSignal s1) <*> (evalSignal s2)
evalSignal (Or s1 s2)   = (.|.) <$> (evalSignal s1) <*> (evalSignal s2)
evalSignal (Lshift s i) = (`shiftL` i) <$> (evalSignal s)
evalSignal (Rshift s i) = (`shiftR` i) <$> (evalSignal s)
evalSignal (Not s)      = complement   <$> (evalSignal s)
evalSignal (Wire str) = do
                        mp       <- get
                        let (Just x) = M.lookup str mp
                        signal   <- evalSignal x
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

instruction :: Parser Instruction
instruction = do s <- signal
                 arrow
                 w <- many1 anyChar
                 return (w,s)
