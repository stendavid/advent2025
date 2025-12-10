import qualified Data.Set as Set

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

-- Invalid IDs are numbers with repeated digits, like 121212, or 12341234, or 111.
-- Generat them using the formula
---
--     id = h + h * 10^m + h * 10^(2m) + ... + h * 10^((n-1)*m)
--        = h * ( 1 + 10^m + 10^(2m) + ... + 10^((n-1)*m) )
--
-- where m is the number of repeated digits, and n = size/m is the number of
-- times they repeat.
--
-- Generate invalid IDs of length 'size' with sets of 'm' repeated digits.
invalidIDsOfSize' size m
    | mod size m /= 0 = []
    | otherwise =
        let n = div size m        -- number of times the digits repeat
            startH = 10 ^ (m-1)   -- smallest m-digit number
            endH = (10 ^ m) - 1   -- largest m-digit number
            base = sum [ 10^(i*m) | i <- [0 .. (n-1)] ]
        in [ h * base | h <- [startH .. endH] ]

unique = Set.toList . Set.fromList

invalidIDsOfSize size =
    let minSetLength = 1
        maxSetLength = div size 2
    in unique $ concat [ invalidIDsOfSize' size l | l <- [minSetLength..maxSetLength]]

invalidIDsInRange (low,high) =
    let
        digitsMin = numDigits low
        digitsMax = numDigits high
        filterRange = filter (\x -> x >= low && x <= high)
    in concat [ filterRange (invalidIDsOfSize d) | d <- [digitsMin .. digitsMax] ]

main = do
    input <- readFile "input"
    let ranges = parseRanges input
    let invalidIDs = ranges >>= invalidIDsInRange
    --putStrLn ("IDs: " ++ (show invalidIDs))
    putStrLn ("Sum: " ++ (show (sum invalidIDs)))
