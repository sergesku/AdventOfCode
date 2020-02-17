import Control.Monad.Trans.State.Strict
import Control.Monad.Except
import Control.Monad.Identity

input = "../input/day1_input.txt"
basement = (-1)

type Floor = Int
type Step  = Int

goUp :: StateT (Floor,Step) (Either Int) ()
goUp = get >>= \(f,s) -> put (f+1,s+1)

goDown :: StateT (Floor,Step) (Either Int) ()
goDown = get >>= \(f,s) -> put (f-1,s+1)

oneStep :: Char -> StateT (Floor,Step) (Either Int) ()
oneStep '(' = goUp
oneStep ')' = goDown
oneStep _   = return ()

checkFloor :: Int -> StateT (Floor,Step) (Either Int) ()
checkFloor n = get >>= \(f,s) -> when (f == n) $ throwError s

getFloor :: String -> Floor
getFloor str = either id fst $ execStateT (mapM_ oneStep str) (0,0)

getStepToFloor :: Int -> String -> Floor
getStepToFloor n str = either id fst $ execStateT (mapM_ (\ch -> oneStep ch >> checkFloor n) str) (0,0)

solveWith :: Show a => (String -> a) -> FilePath -> IO ()
solveWith f i = readFile i >>= (print . f)

main_part1 = solveWith getFloor input
main_part2 = solveWith (getStepToFloor basement) input

                