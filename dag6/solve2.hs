import Data.List (transpose)

toInt s = read s :: Int
isEmpty s = all ((==)' ') s

calc [] [] [] = 0
calc (num:numbers) (op:ops) numbersAcc
    | isEmpty num = calc numbers ops []
    | op == '*' = (toInt num) * (product numbersAcc) + (calc numbers ops [])
    | op == '+' = (toInt num) + (sum numbersAcc) + (calc numbers ops [])
    | op == ' ' = calc numbers ops ((toInt num):numbersAcc)

main = do
    input <- lines <$> readFile "input"
    let ops = reverse (last input)
    let numberLines = init input
    let numbers = reverse (transpose numberLines)
    let result = calc numbers ops []

    --putStrLn (show problems)
    putStrLn ("Sum: " ++ show result)