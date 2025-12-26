split :: Char -> String -> [String]
split c s = case dropWhile ((==) c) s of
            "" -> []
            s' -> w : split c s'' where (w, s'') = break ((==) c) s'

toInt s = read s :: Int

parsePoint s = (toInt x, toInt y) where [x,y] = split ',' s

area (x1,y1) (x2,y2) = (abs (x1-x2) + 1) * (abs (y1-y2) + 1)

main = do
    input <- lines <$> readFile "input"
    let points = map parsePoint input
    let areas = [(area p1 p2) | (p1,i) <- zip points [0..], (p2,j) <- zip points [0..], j < i]
    putStrLn ("Maximum area: " ++ show (maximum areas))
