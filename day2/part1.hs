import Data.Char (digitToInt)
import Data.List (isInfixOf)

-- read lines of input and return horizontal distance
countHorizontal :: [String] -> Int
countHorizontal l
  | l == []   = 0
  | otherwise = sum (map (\x -> digitToInt $ last x) (horizontalInputs l))
  where horizontalInputs x = filter (isInfixOf "forward") x

-- read lines of input and return depth traveled
countDepth :: [String] -> Int
countDepth l
  | l == []   = 0
  | otherwise = sum (map (\x -> parseUpDown x) (depthInputs l))
  where depthInputs x = filter (\y -> (isInfixOf "up" y) || (isInfixOf "down" y)) x
        parseUpDown x = if isInfixOf "up" x
                          then negate $ digitToInt $ last x
                          else digitToInt $ last x

part1 :: [String] -> Int
part1 l = (countHorizontal l) * (countDepth l)

main :: IO()
main = do
  input <- getContents
  putStrLn "Part 1:"
  print $ part1 $ lines input
  
