wheelSize = 100

turn :: (Int,Int) -> (Char,Int) -> (Int,Int)
turn (pos, zero_count) (dir, steps) =
    let new_pos = case dir of
            'L' -> mod (pos - steps) wheelSize
            'R' -> mod (pos + steps) wheelSize
            _   -> error "Direction must be L or R"
        new_zero_count = case new_pos of
            0 -> zero_count + 1
            _ -> zero_count
    in (new_pos, new_zero_count)


turn2 :: (Int,Int) -> (Char, Int) -> (Int,Int)
turn2 (pos, zero_count) (dir, steps) =
    let turns = div steps wheelSize
        new_pos = case dir of
            'L' -> mod (pos - steps) wheelSize
            'R' -> mod (pos + steps) wheelSize
            _   -> error "Direction must be L or R"
        passes_zero = pos /= 0 && (new_pos == 0 || (dir == 'L' && new_pos > pos) || (dir == 'R' && new_pos < pos))
        new_zero_count = zero_count + turns + (if passes_zero then 1 else 0)
    in (new_pos, new_zero_count)

parseInstruction (direction : steps) = (direction, read steps :: Int)

main = do
    input <- readFile "input"
    let instructions = map parseInstruction (lines (input))

    let (_,zero_count) = foldl turn (50,0) instructions
    print ("Found " ++ show zero_count ++ " zeros")

    let (_,pass_count) = foldl turn2 (50,0) instructions
    print ("Passed zero " ++ show pass_count ++ " times")
