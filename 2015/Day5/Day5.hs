{-# LANGUAGE FlexibleContexts #-}

import Text.Parsec
import Data.ByteString.Char8 (ByteString)
import qualified Data.ByteString.Char8 as BS (split, readFile)
import Text.Parsec.Char (endOfLine)
import Data.Bifunctor (first, second)

input = "./day5_input.txt"

type State1 = (Int, Bool)  -- (number of vowels; is there a double letters)
type State2 = (Bool, Bool) -- (is there a pair that appear twice; is there a letter between identical letters)

type Parser1 = Parsec ByteString State1
type Parser2 = Parsec ByteString State2

deprecatedStrings :: [String]
deprecatedStrings = [ "ab"
                    , "cd"
                    , "pq"
                    , "xy"
                    ]

<<<<<<< HEAD
                    
=======
>>>>>>> day5
deprecated :: Parser1 Bool
deprecated = choice $ map (deprecate) deprecatedStrings
  where deprecate str = try (string str) >> return True

vowel :: Parser1 ()
vowel = oneOf "aouie" >> modifyState (first (+1))

doubleLetter :: Parser1 ()
doubleLetter = do lookAhead $ letter >>= char
                  modifyState $ second (const True)

pairTwice :: Char -> Parser2 ()
pairTwice c = do lookAhead $ anyChar >>= \c' -> anyChar `manyTill` (try (string [c,c']))
                 modifyState $ first (const True)

betweenChar :: Char -> Parser2 ()
betweenChar c = do lookAhead $ anyChar >> char c
                   modifyState $ second (const True)
                    
rules1 :: Parser1 ()
rules1 = do
        isBad <- deprecated <|> return False
        if isBad
          then unexpected "Deprecated string"
          else do 
               try doubleLetter <|> return ()
               vowel <|> (() <$ anyChar)

rules2 :: Parser2 ()
rules2 = do
        c <- anyChar
        (try (pairTwice c))   <|> return ()
        (try (betweenChar c)) <|> return ()
        
                   
niceString1 :: Parser1 Bool
niceString1 = do
            (i, b) <- many rules1 >> getState
            return $ and [i > 2, b]

niceString2 :: Parser2 Bool
niceString2 = do
            (x,y) <- many rules2 >> getState
            return $ x && y
               
getCount :: Parsec ByteString st Bool -> st -> ByteString -> Int
getCount p st = length . filter (== valid) . map isValidString . BS.split '\n'
  where valid         = Right True  
        isValidString = runParser p st ""
        
solveWith :: Show a => FilePath -> (ByteString -> a) -> IO ()
solveWith i f = BS.readFile i >>= (print . f)

main_part1 = input `solveWith` (getCount niceString1 (0,False))
main_part2 = input `solveWith` (getCount niceString2 (False,False))