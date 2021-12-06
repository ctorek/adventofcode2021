import Data.List (genericLength, transpose)
import Data.Char (intToDigit)

-- find most common out of a string of 0 or 1
mostCommon :: String -> Char
mostCommon l
    | ones l > zeros l = '1'
    | otherwise        = '0'
    where zeros x = count x '0'
          ones x = count x '1'

-- count appearances of element in list
count :: (Eq a) => [a] -> a -> Int
count l val = length $ filter (== val) l

-- find most common bit for each column in inputs
gammaRate :: [String] -> String
gammaRate l = map (mostCommon) l

-- opposite of gamma rate
epsilonRate :: String -> String
epsilonRate l = map replace l
    where replace x = if x == '0' then '1' else '0'

-- takes in string of 0s and 1s and converts to binary
toDecimal :: String -> Int
toDecimal l = sum $ zipWith (\b ind -> (read b) * (2 ^ ind)) (map (:[]) l) [0,1..]

main :: IO()
main = do
    input <- getContents
    putStrLn "Part 1:"
    let gamma = toDecimal $ gammaRate $ transpose $ lines input
    print gamma
    let epsilon = toDecimal $ epsilonRate $ gammaRate $ transpose $ lines input
    print epsilon
    print (gamma * epsilon)