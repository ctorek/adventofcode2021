-- grid is stored as a 1d array where (x,y) is accessible as x + (y * width)
data Grid = Grid [Int] Int Int

-- checks whether a point is within the boundaries of a grid
inBounds :: (Int, Int) -> (Int, Int) -> Bool
inBounds (w, h) (x, y) = if x < 0 || y < 0 || x > w || y > h
                                  then False
                                  else True

-- returns a list of adjacent points, excluding diagonals
adjacent :: (Int, Int) -> [(Int, Int)]
adjacent (x, y) = [right, left, up, down]
  where right = (x+1, y)
        left = (x-1, y)
        up = (x, y+1)
        down = (x, y-1)

-- find points with values lower than adjacent values
lowest :: Grid -> [Int]
lowest (Grid values w h) = [a | (a, i) <- zip values [0..], isLowest $ fromIndex i w]
  where neighbors (x, y) = filter (inBounds (w, h)) (adjacent (x, y))
        isLowest (x, y) = all (\(x1, y1) -> val (x1, y1) > val (x, y)) (neighbors (x, y))
        val (x, y) = values !! toIndex (x, y) w

-- convert (x, y) point on grid into list index
toIndex :: (Int, Int) -> Int -> Int
toIndex (x, y) w = x + (y * w)

-- convert list index into (x, y) point on grid
fromIndex :: Int -> Int -> (Int, Int)
fromIndex index w = (index `mod` w, index `div` w)

main :: IO()
main = do
  putStrLn "Part 1:"
