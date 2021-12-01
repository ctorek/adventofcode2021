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

-- read strings into integers from file line by line
readLines :: String -> [Integer]
readLines l = [read x | x <- lines l]

main = do
  -- read input from stdin
  input <- getContents
  let count = countGreater $ comparePrevious $ readLines input
  print count
