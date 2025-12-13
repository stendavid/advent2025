import qualified Data.Map.Strict as Map
import Data.List (intersperse)

neighbours (x,y) =
    [(x-1,y-1), (x, y-1), (x+1, y-1),
     (x-1,  y),           (x+1,   y),
     (x-1,y+1), (x, y+1), (x+1, y+1)]

showMap m = concat (intersperse "\n" rows)
    where
        (ncols,nrows) = maximum (Map.keys m)
        row y = [ m Map.! (x,y) | x <- [1..ncols] ]
        rows = [ row y | y <- [1..nrows] ]

replaceWith newValue keys m = Map.union replacements m
    where replacements = Map.fromList (zip keys (repeat newValue))

recursivelyMovable charMap =
    let
        isPaper (x,y) = '@' == Map.findWithDefault '.' (x,y) charMap
        countPaperNeighbours c = length (filter isPaper (neighbours c))
        movable = filter (\c -> isPaper c && countPaperNeighbours c < 4) (Map.keys charMap)
    in
        if movable == []
            then [] 
            else movable ++ recursivelyMovable (replaceWith '.' movable charMap)

main = do
    input <- lines <$> readFile "input"
    let charMap = Map.fromList [ ((x,y),char) | (row,y) <- zip input [1..],
                                                (char,x) <- zip row [1..]]
    let movable = recursivelyMovable charMap

    --putStrLn (showMap (replaceWith 'x' movable charMap))
    putStrLn ("Count: " ++ show (length movable))