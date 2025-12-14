toInt s = read s :: Int

-- Parse "12-34" -> (12,34)
parseRange s = (toInt a, toInt b) where (a,_:b) = break ((==)'-') s

parseInput input = (map parseRange freshRanges, map toInt availableIDs)
    where
        (freshRanges,_:availableIDs) = break ((==)"") input

main = do
    input <- lines <$> readFile "input"
    let (freshRanges, availableIDs) = parseInput input
    let isInRange i (a,b) = i >= a && i <= b 
    let isFresh id = any (isInRange id) freshRanges
    let fresh = filter isFresh availableIDs

    putStrLn (show (length fresh) ++ " fresh IDs")
    --putStrLn (show fresh)