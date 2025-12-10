split :: Char -> String -> [String]
split c s = case dropWhile ((==) c) s of
            "" -> []
            s' -> w : split c s'' where (w, s'') = break ((==) c) s'

-- Parse "12-34" -> (12,34)
parseRange :: String -> (Int,Int)
parseRange s =
    let (a:b:_) = split '-' s
    in
        (read a :: Int, read b :: Int)

-- Parse "12-34,45-67,..." -> [(12,34), (45,67), ...]
parseRanges :: String -> [(Int,Int)]
parseRanges s = map parseRange (split ',' s)

numDigits n = if n < 10 then 1 else 1 + numDigits (div n 10)

-- Generate invalid IDs.
-- Invalid IDs are numbers with two sets of repeated digits, like 12341234.
-- Generat them using the formula id = h * (10^m) + h = h * (10^m + 1), where
-- m is half the number of digits in id.
invalidIDsOfSize size
    | mod size 2 /= 0 = []
    | otherwise =
        let m = div size 2
            startH = 10 ^ (m-1)     -- smallest m-digit number
            endH   = (10 ^ m) - 1   -- largest m-digit number
            base = 10 ^ m + 1
        in [ h * base | h <- [startH .. endH] ]

invalidIDsInRange (low,high) =
    let
        digitsMin = numDigits low
        digitsMax = numDigits high
        filterRange l h = filter (\x -> x >= l && x <= h)
    in concat [ filterRange low high (invalidIDsOfSize d) | d <- [digitsMin .. digitsMax] ]

main = do
    input <- readFile "input"
    let ranges = parseRanges input
    let invalidIDs = ranges >>= invalidIDsInRange
    putStrLn ("Sum: " ++ (show (sum invalidIDs)))
