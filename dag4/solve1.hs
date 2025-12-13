import qualified Data.Map.Strict as Map

neighbours (x,y) =
    [(x-1,y-1), (x, y-1), (x+1, y-1),
     (x-1,  y),           (x+1,   y),
     (x-1,y+1), (x, y+1), (x+1, y+1)]

main = do
    input <- lines <$> readFile "input"
    let charMap = Map.fromList [ ((x,y),char) | (row,y) <- zip input [1..],
                                                (char,x) <- zip row [1..]]
    let isPaper (x,y) = '@' == Map.findWithDefault '.' (x,y) charMap
    let coords = [(x,y) | y <- [1..nrows], x <- [1..ncols]] where
            nrows = length input
            ncols = length (head input)
    let countPaperNeighbours c = length (filter isPaper (neighbours c))
    let movable = filter (\c -> isPaper c && countPaperNeighbours c < 4) coords

    -- putStrLn (show movable)
    -- putStrLn ""
    -- putStrLn (show charMap)
    putStrLn ("Count: " ++ show (length movable))