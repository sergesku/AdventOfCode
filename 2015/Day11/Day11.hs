import Control.Applicative (liftA2)

input = "hxbxwxba"

succString :: String -> String
succString ""       = ""
succString ('h':xs) = 'j' : (succString xs)
succString ('k':xs) = 'm' : (succString xs)
succString ('n':xs) = 'p' : (succString xs)
succString ('z':xs) = 'a' : (succString xs)
succString (x:xs)   = (succ x) : xs 

hasStraight :: String -> Bool
hasStraight (z:rest@(y:x:_)) = ((z == succ y) && (y == succ x)) || hasStraight rest
hasStraight _ = False

hasDifferentPair :: Char -> String -> Bool
hasDifferentPair c (x:y:rest) | x == c    = hasDifferentPair c (y:rest)
                              | x == y    = True
                              | otherwise = hasDifferentPair c (y:rest)
hasDifferentPair _ _  = False
                     
hasTwoPair :: String -> Bool
hasTwoPair (x:y:rest) = ((x == y) && (hasDifferentPair x rest)) || (hasTwoPair (y:rest))
hasTwoPair _ = False

correctPassword :: String -> Bool
correctPassword = liftA2 (&&) hasStraight hasTwoPair

nextPassword :: String -> String
nextPassword = reverse . head . filter (correctPassword) . drop 1 . iterate succString . reverse

main_part1 = nextPassword input
main_part2 = nextPassword . nextPassword $ input
