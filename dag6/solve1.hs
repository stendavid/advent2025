toInt a = read a :: Int

ltrim = dropWhile ((==)' ')
word = break ((==)' ')

nextWord s =
    let (value, s') = word (ltrim s)
    in (value, ltrim s')

parseInput ["","","","",""] = []
parseInput [a,b,c,d,e] = [(toInt ai, toInt bi, toInt ci, toInt di, ei)] ++ parseInput [a',b',c',d',e']
    where
        (ai,a') = nextWord a
        (bi,b') = nextWord b
        (ci,c') = nextWord c
        (di,d') = nextWord d
        (ei,e') = nextWord e

calc (a,b,c,d,"+") = a+b+c+d
calc (a,b,c,d,"*") = a*b*c*d

main = do
    input <- lines <$> readFile "input"
    let problems = parseInput input
    
    --putStrLn (show problems)
    putStrLn ("Sum: " ++ show (sum (map calc problems)))