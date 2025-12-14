import qualified Data.List as List

toInt s = read s :: Int

-- Parse "12-34" -> (12,34)
parseRange s = (toInt a, toInt b) where (a,_:b) = break ((==)'-') s

parseInput input = map parseRange (takeWhile ((/=)"") input)

merge (a1,a2) (b1,b2) = (min a1 b1, max a2 b2)

isOverlap (a1,a2) (b1,b2) = b1 <= a2 && b2 >= a1

mergeRanges [] = []
mergeRanges [a] = [a]
mergeRanges (a:b:rest)
    | isOverlap a b = mergeRanges ((merge a b):rest)
    | otherwise = [a] ++ mergeRanges (b:rest)

main = do
    input <- lines <$> readFile "input"
    let freshRanges = List.sort $ parseInput input
    let mergedRanges = mergeRanges freshRanges
    let totalIDs = sum [b-a+1 | (a,b) <- mergedRanges]

    --putStrLn (show (length freshRanges) ++ " ranges")
    --putStrLn (show (length mergedRanges) ++ " ranges after merging")
    putStrLn (show (totalIDs) ++ " fresh IDs")