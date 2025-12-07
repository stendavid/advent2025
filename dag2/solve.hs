split :: Char -> String -> [String]
split c s = case dropWhile ((==) c) s of
            "" -> []
            s' -> w : split c s'' where (w, s'') = break ((==) c) s'

parseRange s =
    let (a:b:_) = split '-' s
    in
        (read a :: Int, read b :: Int)

parseRanges :: String -> [(Int,Int)]
parseRanges s = map parseRange (split ',' s)

numDigits n = if n < 10 then 1 else 1 + numDigits (div n 10)

isInvalidID i =
    let n = numDigits i
    in
        if mod n 2 /= 0 then False
        else
            let mask = 10^(div n 2)
                high = div i mask
                low = mod i mask
            in
                high == low

invalidIDsInRange (low,high) = filter isInvalidID [low .. high]

main = do
    input <- readFile "input"
    let ranges = parseRanges input
    let invalidIDs = ranges >>= invalidIDsInRange

    --putStrLn ("Invalid IDs: " ++ show invalidIDs)
    putStrLn ("Sum: " ++ (show (sum invalidIDs)))
