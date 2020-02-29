import Text.Parsec
import Data.Text (Text)
import qualified Data.Text as T (lines)
import qualified Data.Text.IO as TIO
import Text.Parsec.Char (endOfLine)
import Data.Bifunctor (first, second)

input = "./day5_input.txt"

type StringState = (Int, Bool) -- (number of vowels, is there a double letter)

type Pars = Parsec Text StringState

deprecatedStrings :: [String]
deprecatedStrings = [ "ab"
                    , "cd"
                    , "pq"
                    , "xy"
                    ]
                   
deprecate :: String -> Pars Bool
deprecate str = try $ string str >> return True

deprecated :: Pars Bool
deprecated = choice $ map (deprecate) deprecatedStrings

vowel :: Pars ()
vowel = oneOf "aouie" >> modifyState (first (+1))

doubleLetter :: Pars ()
doubleLetter = do 
            lookAhead $ letter >>= char
            modifyState (fmap (const True))

rules :: Pars ()
rules = do
            isBad <- deprecated <|> return False
            if isBad
              then unexpected "Deprecated string"
              else do 
                    try doubleLetter <|> return ()
                    vowel <|> (() <$ anyChar)

niceString :: Pars Bool
niceString = do 
            (i, b) <- many rules >> getState
            return $ and [i > 2, b]
                       

               
getCount :: Text -> Int
getCount = length . filter (== valid) . map isValidString . T.lines
  where valid         = Right True  
        initState     = (0,False)
        isValidString = runParser niceString initState ""

solveWith :: Show a => (Text -> a) -> FilePath -> IO ()
solveWith f i = TIO.readFile i >>= (print . f)

main_part1 = solveWith getCount input