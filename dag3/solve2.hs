toInt s = read s :: Int

-- All except the 'n' last elements of 'list'
initN n list = take (length list - n) list

maxJoltage n str
    | n == 0 = []
    | otherwise = (m : maxJoltage (n-1) rest)
        where
            m = maximum (initN (n-1) str)
            (_:rest) = dropWhile ((/=)m) str

main = do
    input <- readFile "input"
    let joltages = map (maxJoltage 12) (lines input)
    --putStrLn (show joltages)
    putStrLn ("Sum: " ++ show (sum (map toInt joltages)))