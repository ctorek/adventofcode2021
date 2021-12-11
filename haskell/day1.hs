-- compare each index to the previous index
comparePrevious :: [Integer] -> [Ordering]
comparePrevious l
  -- need more than one index to compare to the previous index
  | length l < 2 = []
  | otherwise =
    -- compare elements 0 and 1, append to rest of comparisons
    (compare (head (tail l)) (head l)) : comparePrevious (tail l)

-- count the elements that are equal to GT
countGreater :: [Ordering] -> Int
countGreater l =
  length (filter (\x -> x == GT) l)

-- generate a list of sums of each 3 elements
sumOfThree :: [Integer] -> [Integer]
sumOfThree l
  | length l < 3 = []
  | otherwise    = sum (take 3 l) : sumOfThree (tail l)

-- read strings into integers from file line by line
readLines :: String -> [Integer]
readLines l = [read x | x <- lines l]

main = do
  -- read input from stdin
  input <- getContents
  putStrLn "Part 1"
  print $ countGreater $ comparePrevious $ readLines input

  putStrLn "Part 2"
  print $ countGreater $ comparePrevious $ sumOfThree $ readLines input