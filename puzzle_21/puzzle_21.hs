module Main where
    import Debug.Trace

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input

    parseNumeric :: String -> String
    parseNumeric = parseNumericAcc 'A'
        where
            parseNumericAcc :: Char -> String -> String
            parseNumericAcc _ [] = []
            parseNumericAcc s (x:xs) = advanceNumeric s x ++ parseNumericAcc x xs

    advanceNumeric :: Char -> Char -> String
    advanceNumeric '0' '0' = "A"
    advanceNumeric '0' '1' = "^<A"
    advanceNumeric '0' '2' = "^A"
    advanceNumeric '0' '3' = ">^A"
    advanceNumeric '0' '4' = "^^<A"
    advanceNumeric '0' '5' = "^^A"
    advanceNumeric '0' '6' = "^^>A"
    advanceNumeric '0' '7' = "^^^<A"
    advanceNumeric '0' '8' = "^^^A"
    advanceNumeric '0' '9' = ">^^^A"
    advanceNumeric '0' 'A' = ">A"

    advanceNumeric '1' '0' = ">vA"
    advanceNumeric '1' '1' = "A"
    advanceNumeric '1' '2' = ">A"
    advanceNumeric '1' '3' = ">>A"
    advanceNumeric '1' '4' = "^A"
    advanceNumeric '1' '5' = ">^A"
    advanceNumeric '1' '6' = "^>>A"
    advanceNumeric '1' '7' = "^^A"
    advanceNumeric '1' '8' = "^^>A"
    advanceNumeric '1' '9' = "^^>>A"
    advanceNumeric '1' 'A' = ">>vA"

    advanceNumeric '2' '0' = "vA"
    advanceNumeric '2' '1' = "<A"
    advanceNumeric '2' '2' = "A"
    advanceNumeric '2' '3' = ">A"
    advanceNumeric '2' '4' = "<^A"
    advanceNumeric '2' '5' = "^A"
    advanceNumeric '2' '6' = "^>A"
    advanceNumeric '2' '7' = "<^^A"
    advanceNumeric '2' '8' = "^^A"
    advanceNumeric '2' '9' = "^^>A"
    advanceNumeric '2' 'A' = "v>A"

    advanceNumeric '3' '0' = "<vA"
    advanceNumeric '3' '1' = "<<A"
    advanceNumeric '3' '2' = "<A"
    advanceNumeric '3' '3' = "A"
    advanceNumeric '3' '4' = "<<^A"
    advanceNumeric '3' '5' = "<^A"
    advanceNumeric '3' '6' = "^A"
    advanceNumeric '3' '7' = "<<^^A"
    advanceNumeric '3' '8' = "<^^A"
    advanceNumeric '3' '9' = "^^A"
    advanceNumeric '3' 'A' = "vA"

    advanceNumeric '4' '0' = ">vvA"
    advanceNumeric '4' '1' = "vA"
    advanceNumeric '4' '2' = ">vA"
    advanceNumeric '4' '3' = ">>vA"
    advanceNumeric '4' '4' = "A"
    advanceNumeric '4' '5' = ">A"
    advanceNumeric '4' '6' = ">>A"
    advanceNumeric '4' '7' = "^A"
    advanceNumeric '4' '8' = ">^A"
    advanceNumeric '4' '9' = ">>^A"
    advanceNumeric '4' 'A' = ">>vvA"

    advanceNumeric '5' '0' = "vvA"
    advanceNumeric '5' '1' = "<vA"
    advanceNumeric '5' '2' = "vA"
    advanceNumeric '5' '3' = ">vA"
    advanceNumeric '5' '4' = "<A"
    advanceNumeric '5' '5' = "A"
    advanceNumeric '5' '6' = ">A"
    advanceNumeric '5' '7' = "<^A"
    advanceNumeric '5' '8' = "^A"
    advanceNumeric '5' '9' = ">^A"
    advanceNumeric '5' 'A' = "vv>A"

    advanceNumeric '6' '0' = "<vvA"
    advanceNumeric '6' '1' = "<<vA"
    advanceNumeric '6' '2' = "<vA"
    advanceNumeric '6' '3' = "vA"
    advanceNumeric '6' '4' = "<<A"
    advanceNumeric '6' '5' = "<A"
    advanceNumeric '6' '6' = "A"
    advanceNumeric '6' '7' = "<<^A"
    advanceNumeric '6' '8' = "<^A"
    advanceNumeric '6' '9' = "^A"
    advanceNumeric '6' 'A' = "vvA"

    advanceNumeric '7' '0' = ">vvvA"
    advanceNumeric '7' '1' = "vvA"
    advanceNumeric '7' '2' = "vv>A"
    advanceNumeric '7' '3' = "vv>>A"
    advanceNumeric '7' '4' = "vA"
    advanceNumeric '7' '5' = "v>A"
    advanceNumeric '7' '6' = "v>>A"
    advanceNumeric '7' '7' = "A"
    advanceNumeric '7' '8' = ">A"
    advanceNumeric '7' '9' = ">>A"
    advanceNumeric '7' 'A' = ">>vvvA"

    advanceNumeric '8' '0' = "vvvA"
    advanceNumeric '8' '1' = "<vvA"
    advanceNumeric '8' '2' = "vvA"
    advanceNumeric '8' '3' = "vv>A"
    advanceNumeric '8' '4' = "<vA"
    advanceNumeric '8' '5' = "vA"
    advanceNumeric '8' '6' = "v>A"
    advanceNumeric '8' '7' = "<A"
    advanceNumeric '8' '8' = "A"
    advanceNumeric '8' '9' = ">A"
    advanceNumeric '8' 'A' = "vvv>A"

    advanceNumeric '9' '0' = "<vvvA"
    advanceNumeric '9' '1' = "<<vvA"
    advanceNumeric '9' '2' = "<vvA"
    advanceNumeric '9' '3' = "vvA"
    advanceNumeric '9' '4' = "<<vA"
    advanceNumeric '9' '5' = "<vA"
    advanceNumeric '9' '6' = "vA"
    advanceNumeric '9' '7' = "<<A"
    advanceNumeric '9' '8' = "<A"
    advanceNumeric '9' '9' = "A"
    advanceNumeric '9' 'A' = "vvvA"

    advanceNumeric 'A' '0' = "<A"
    advanceNumeric 'A' '1' = "^<<A"
    advanceNumeric 'A' '2' = "<^A"
    advanceNumeric 'A' '3' = "^A"
    advanceNumeric 'A' '4' = "^^<<A"
    advanceNumeric 'A' '5' = "<^^A"
    advanceNumeric 'A' '6' = "^^A"
    advanceNumeric 'A' '7' = "^^^<<A"
    advanceNumeric 'A' '8' = "<^^^A"
    advanceNumeric 'A' '9' = "^^^A"
    advanceNumeric 'A' 'A' = "A"

    parseDirectional :: String -> String
    parseDirectional = parseDirectionalAcc 'A'
        where
            parseDirectionalAcc :: Char -> String -> String
            parseDirectionalAcc _ [] = []
            parseDirectionalAcc s (x:xs) = advanceDirectional s x ++ parseDirectionalAcc x xs

    advanceDirectional :: Char -> Char -> String
    advanceDirectional '>' '>' = "A"
    advanceDirectional '>' '<' = "<<A"
    advanceDirectional '>' '^' = "<^A"
    advanceDirectional '>' 'v' = "<A"
    advanceDirectional '>' 'A' = "^A"

    advanceDirectional '<' '>' = ">>A"
    advanceDirectional '<' '<' = "A"
    advanceDirectional '<' '^' = ">^A"
    advanceDirectional '<' 'v' = ">A"
    advanceDirectional '<' 'A' = ">>^A"

    advanceDirectional '^' '>' = "v>A"
    advanceDirectional '^' '<' = "v<A"
    advanceDirectional '^' '^' = "A"
    advanceDirectional '^' 'v' = "vA"
    advanceDirectional '^' 'A' = ">A"

    advanceDirectional 'v' '>' = ">A"
    advanceDirectional 'v' '<' = "<A"
    advanceDirectional 'v' '^' = "^A"
    advanceDirectional 'v' 'v' = "A"
    advanceDirectional 'v' 'A' = "^>A"

    advanceDirectional 'A' '>' = "vA"
    advanceDirectional 'A' '<' = "v<<A"
    advanceDirectional 'A' '^' = "<A"
    advanceDirectional 'A' 'v' = "<vA"
    advanceDirectional 'A' 'A' = "A"

    fullChain :: String -> String
    fullChain = parseDirectional . parseDirectional . parseNumeric

    puzzle1 :: String -> Int
    puzzle1 = sum . map (\s -> let k = read (init s) in k * (length . fullChain $ s)) . lines
