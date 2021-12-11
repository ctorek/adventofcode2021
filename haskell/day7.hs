import Data.List.Split (splitOn)
import Data.List (sort)

-- find sum distance of each crab from a position
distance :: [Int] -> Int -> Int
distance l pos = sum $ map (\x -> abs (x - pos)) l

-- find median of a list
-- i cant write a type signature that works so i did not write one
findMedian l
  | l == [] = 0
  | (length l `mod` 2) == 0 = truncate $ avg (l !! mid) (l !! mid + 1)
  | otherwise = (sort l) !! mid
  where mid = round ((fromIntegral $ length l) / 2.0)

-- find average
avg :: (Integral a, RealFrac b) => a -> a -> b
avg x y = realToFrac (x + y) / 2.0

-- convert input file into list of numbers
parseInput :: String -> [Int]
parseInput s = map read (splitOn "," s) 

main :: IO()
main = do
  input <- getContents
  let i = parseInput input

  putStrLn "Part 1:"
  print $ distance i (findMedian i)


  
