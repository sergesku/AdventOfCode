import Text.Parsec
import Control.Applicative (liftA2)
import Data.List (transpose)

type Parser = Parsec String ()
type Variant = [Int]            
type Ingredient = ([Int],Int)  -- (properties, calories)

input    = "./day15_input.txt"
spoons   = 100
calories = 500


number :: Parser Int
number = negNumber <|> posNumber
  where posNumber = read <$> (many1 digit)
        negNumber = char '-' *> (negate <$> posNumber)

        
ingredient :: Parser Ingredient
ingredient = do
             a <- anyChar `manyTill` (lookAhead number) *> number
             b <- anyChar `manyTill` (lookAhead number) *> number
             c <- anyChar `manyTill` (lookAhead number) *> number
             d <- anyChar `manyTill` (lookAhead number) *> number
             e <- anyChar `manyTill` (lookAhead number) *> number
             return ([a,b,c,d],e)

             
allIngredients :: Parser [Ingredient]
allIngredients = ingredient `sepEndBy` endOfLine


variants :: Int         -- number of ingridients
         -> Int         -- number of spoons
         -> [Variant]   -- all possible combinations of ingridients
variants i n = filter ((==n) . sum) $ sequenceA $ map (const [1..n]) [1..i]           
             
             
variantResult :: [Ingredient] 
              -> Variant        
              -> (Int, Int)
variantResult ingr var = (calories, totalScore)
    where lst          = zipWith (\n i -> map (*n) i) var (map fst ingr)
          totalScore   = product $ map (max 0 . sum) $ transpose lst
          calories     = sum $ zipWith (\(_,k) n -> k * n) ingr var

findBestScores :: [Ingredient]
               -> Bool       -- ignore calories
               -> Int        -- number of spoons
               -> Int        -- number of calories
               -> Int        -- best scores
findBestScores ingr ignore sp cal = maximum checkedScores  
    where nIngr           = length ingr
          results         = map (variantResult ingr) $ variants nIngr sp
          caloriesFilter  = liftA2 (||) (const ignore) ((==cal) . fst)
          checkedCalories = filter caloriesFilter results
          checkedScores   = filter (> 0) . map snd $ checkedCalories
   
  
solveWith :: FilePath   
          -> Bool       -- ignore calories
          -> Int        -- number of spoons
          -> Int        -- number of calories
          -> IO ()
solveWith file ignore sp cal = do
            file <- readFile file
            let (Right ingredients) = parse allIngredients "" file
                answer = findBestScores ingredients ignore sp cal
            print answer
          
          
main_part1 = solveWith input True  spoons calories
main_part2 = solveWith input False spoons calories
