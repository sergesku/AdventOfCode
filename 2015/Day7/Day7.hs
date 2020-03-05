import Data.Word
import Data.Bits
import qualified Data.Map as M
import Control.Monad.State

type Wire = String
type WireMap = M.Map Wire Signal

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
                         
                         