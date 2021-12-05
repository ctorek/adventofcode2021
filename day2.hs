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

-- mutliply horizontal distance by depth
part1 :: [String] -> Int
part1 l = (countHorizontal l) * (countDepth l)

-- count depth with aim parameter from part 2
aimDepth :: [String] -> Int -> Int
aimDepth l aim 
  | l == []                       = 0
  | isInfixOf "forward" (head l)  = (aim * digit (head l)) + aimDepth (tail l) aim
  | isInfixOf "up" (head l)       = aimDepth (tail l) (aim - (digit $ head l))
  | isInfixOf "down" (head l)     = aimDepth (tail l) (aim + (digit $ head l))
  where digit s = digitToInt $ last s

-- mutliply horizontal distance by aimed depth
part2 :: [String] -> Int
part2 l = (countHorizontal l) * (aimDepth l 0)

main :: IO()
main = do
  -- read input
  input <- getContents

  putStrLn "Part 1:"
  print $ part1 $ lines input

  putStrLn "Part 2:"
  print $ part2 $ lines input