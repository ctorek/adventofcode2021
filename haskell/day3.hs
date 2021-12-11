import Data.List (genericLength, transpose)
import Data.Char (intToDigit)

-- find most common out of a string of 0 or 1 and a default if equal
mostCommon :: String -> Char -> Char
mostCommon l d
    | ones l == zeros l = d
    | ones l > zeros l  = '1'
    | otherwise         = '0'
    where zeros x = count x '0'
          ones x = count x '1'

-- gets opposite of most common
leastCommon :: String -> Char -> Char
leastCommon l d =
    if (mostCommon l '1' == '1')
      then '0' else '1'

-- count appearances of element in list
count :: (Eq a) => [a] -> a -> Int
count l val = length $ filter (== val) l

-- find most common bit for each column in inputs
gammaRate :: [String] -> String
gammaRate l = map (\x -> mostCommon x '0') l

-- opposite of gamma rate
epsilonRate :: String -> String
epsilonRate l = map replace l
    where replace x = if x == '0' then '1' else '0'

-- takes in string of 0s and 1s and converts to binary
toDecimal :: String -> Int
toDecimal l = sum $ zipWith (\b ind -> (read b) * (2 ^ ind)) (reverse $ map (:[]) l) [0,1..]

-- find most common bit in each position or default 1 if equal
oxygenRate :: [String] -> String
oxygenRate l = mostCommon (map head l) '1' : oxygenRate (map tail l)

-- find least common bit in each position or default 0 if equal
co2Rate :: [String] -> String
co2Rate l = leastCommon (map head l) '0' : co2Rate (map tail l)

main :: IO()
main = do
    input <- getContents
    let i = transpose $ lines input
    putStrLn "Part 1:"
    let gamma = toDecimal $ gammaRate i
    let epsilon = toDecimal $ epsilonRate $ gammaRate i
    print (gamma * epsilon)

    putStrLn "Part 2:"
    let oxygen = toDecimal $ oxygenRate i
    print oxygen
