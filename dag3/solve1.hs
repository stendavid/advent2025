toInt s = read s :: Int

maxJoltage str = [m, maximum rest]
    where
        m = maximum (init str)
        (_:rest) = dropWhile ((/=)m) str

main = do
    input <- readFile "input"
    let joltages = map maxJoltage (lines input)
    --putStrLn (show joltages)
    putStrLn ("Sum: " ++ show (sum (map toInt joltages)))