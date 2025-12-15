window3 (a:b:c:rest) = (a,b,c) : window3 (b:c:rest)
window3 _ = []

calcRayCount :: [Int] -> [Char] -> [Int]
calcRayCount prevCounts chars = map calcOnCell (zip windowedCounts windowedChars)
  where
    windowedCounts = window3 ([0] ++ prevCounts ++ [0])
    windowedChars = window3 (['.'] ++ chars ++ ['.'])
    calcOnCell ((n1, n2, n3), (ch1, ch2, ch3)) =
      (if ch1 == '^' then n1 else 0) +  -- rays from cell to the left if split
      (if ch2 /= '^' then n2 else 0) +  -- rays from above if not split
      (if ch3 == '^' then n3 else 0)    -- rays from cell to the right if split

main = do
  grid <- lines <$> readFile "input"
  let rayCountFirstRow = [if ch == 'S' then 1 else 0 | ch <- head grid]
  let rayCountLastRow = foldl calcRayCount rayCountFirstRow (tail grid)

  putStrLn ("Sum: " ++ show (sum rayCountLastRow))
  --putStrLn (show rayCountLastRow)
