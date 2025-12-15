-- Done by copilot
import qualified Data.Set as Set

type Pos = (Int, Int)

-- Find the start position 'S' in the grid
findStart :: [[Char]] -> Pos
findStart grid = head [(r, c) | (r, row) <- zip [0..] grid, (c, ch) <- zip [0..] row, ch == 'S']

-- Simulate the beam splitting process
simulate :: [[Char]] -> Int
simulate grid = go (Set.singleton (startRow+1, startCol)) Set.empty 0
  where
    (startRow, startCol) = findStart grid
    numRows = length grid
    numCols = length (head grid)
    -- beams: set of (row, col) positions where beams are currently active
    -- visited: set of (row, col) positions already processed to avoid loops
    go beams visited splits
      | Set.null beams = splits
      | otherwise =
          let (row, col) = Set.findMin beams
              rest = Set.delete (row, col) beams
          in if row >= numRows || col < 0 || col >= numCols || Set.member (row, col) visited
                then go rest visited splits
                else case grid !! row !! col of
                  '.' -> go (Set.insert (row+1, col) rest) (Set.insert (row, col) visited) splits
                  '^' -> -- Split: add left and right beams, count split
                      go (Set.insert (row, col-1) (Set.insert (row, col+1) rest)) (Set.insert (row, col) visited) (splits+1)
                  _   -> go rest (Set.insert (row, col) visited) splits

parseGrid :: String -> [[Char]]
parseGrid = lines

main :: IO ()
main = do
  content <- readFile "input"
  let grid = parseGrid content
  let splits = simulate grid
  print splits
