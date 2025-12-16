import qualified Data.List as List
import qualified Data.Map.Strict as Map

split c s = case break ((==) c) s of
            (w, [])     -> [w]
            (w, _:rest) -> w : split c rest

toInt s = read s :: Int

parseBox s = (x,y,z) where [x,y,z] = map toInt $ split ',' s

distance (x1,y1,z1) (x2,y2,z2) = (abs (x1-x2)) ^ 2 + (abs (y1-y2)) ^ 2 + (abs (z1-z2)) ^ 2

addPair :: Map.Map Int Int -> (Int,Int,Int) -> Map.Map Int Int
addPair circuits (_,b1,b2) = circuits'
    where
        c1 = Map.findWithDefault 0 b1 circuits
        c2 = Map.findWithDefault 0 b2 circuits
        circuits' =
            case (c1,c2) of
                (0,0) -> addNewCircuit circuits b1 b2 -- neither b1 nor b2 are in a circuit
                (0,_) -> addToCircuit circuits c2 b1 -- b2 is in a cirucuit, add b1 to it 
                (_,0) -> addToCircuit circuits c1 b2 -- b1 is in a cirucuit, add b2 to it
                (_,_) -> mergeCircuits circuits c1 c2 -- both b1 and b2 are in circuits, merge them

addNewCircuit circuits b1 b2 = Map.insert b1 c $ Map.insert b2 c circuits
    where
        c = if circuits == Map.empty then 1
                                     else (maximum circuits) + 1

addToCircuit circuits c b = Map.insert b c circuits

mergeCircuits circuits c1 c2 = Map.union movec2_c1 circuits
    where
        c2keys = Map.keys $ Map.filter ((==)c2) circuits
        movec2_c1 = Map.fromList (zip c2keys (repeat c1))

main = do
    input <- lines <$> readFile "input"
    let n = 1000
    let boxes = map parseBox input
    let distances = [(distance b1 b2, i1, i2) |
                        (b1,i1) <- zip boxes [1..],
                        (b2,i2) <- drop i1 (zip boxes [1..])]
    let nshortest = take n $ List.sort distances
    -- Create a map of box index -> circuit number
    let boxCircuitMap = foldl addPair Map.empty nshortest
    -- Count occurences of each circuit number
    let circuitSizes = Map.fromListWith (+) (zip (Map.elems boxCircuitMap) (repeat 1))
    let sortedSizes = List.reverse $ List.sort $ Map.elems circuitSizes

    --putStrLn (show nshortest)
    --putStrLn ""
    --putStrLn (show boxCircuitMap)
    --putStrLn ""
    putStrLn ("Product: " ++ (show $ product $ take 3 sortedSizes))
    --putStrLn (show sortedSizes)