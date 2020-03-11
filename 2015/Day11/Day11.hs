import Data.Char

input = "hxbxwxba"

succString :: String -> String
succString ""       = ""
succString ('z':xs) = 'a' : (succString xs)
succString (x:xs)   = (succ x) : xs 

hasStraight :: String -> Bool
hasStraight (z:rest@(y:x:_)) = ((z == succ y) && (y == succ x)) || hasStraight rest
hasStraight _ = False

hasNoDeprecated :: String -> Bool
hasNoDeprecated = not . any (`elem` "iol")

hasDifferentPair :: Char -> String -> Bool
hasDifferentPair c (x:y:rest) | x == c    = hasDifferentPair c (y:rest)
                              | x == y    = True
                              | otherwise = hasDifferentPair c (y:rest)
hasDifferentPair _ _  = False
                     
hasTwoPair :: String -> Bool
hasTwoPair (x:y:rest) = ((x == y) && (hasDifferentPair x rest)) || (hasTwoPair (y:rest))
hasTwoPair _ = False

correctPassword :: String -> Bool
correctPassword = and . sequenceA [hasStraight, hasNoDeprecated, hasTwoPair]

main_part1 :: String
main_part1 = reverse . head . filter (correctPassword) . drop 1 . iterate succString $ reverse input