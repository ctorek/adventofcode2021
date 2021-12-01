-- generate a list of sums of each 3 elements
sumOfThree :: [Integer] -> [Integer]
sumOfThree l
  | length l < 3 = []
  | otherwise    = sum (take 3 l) : sumOfThree (tail l)

-- compare each element of list to previous element
compareEach :: (Ord a) => [a] -> [Ordering]
compareEach l
  | length l < 2 = []
  | otherwise    = compare (head (tail l)) (head l) : compareEach (tail l)

-- count occurences of GT in the compared list
count :: [Ordering] -> Int
count l = length (filter (\x -> x == GT) l)

-- read lines from a string into a list of integers
readLines :: String -> [Integer]
readLines l = [read x | x <- lines l] 

-- count the number of groups of 3 that are greater than the previous group of 3
main :: IO()
main = do
  input <- getContents
  let total = count $ compareEach $ sumOfThree $ readLines input
  print total
