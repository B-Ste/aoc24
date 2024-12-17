module Main where
    import Debug.Trace (trace)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        (print . puzzle1) input
        (print . puzzle2) input

    parseInput :: String -> [Maybe Int]
    parseInput = parseInputAcc 0
        where
            parseInputAcc :: Int -> String -> [Maybe Int]
            parseInputAcc id [x] = replicate (read [x]) (Just id)
            parseInputAcc id (x:y:zs) =
                replicate (read [x]) (Just id)
                ++ replicate (read [y]) Nothing
                ++ parseInputAcc (id + 1) zs

    compress :: [Maybe Int] -> [Int]
    compress s = take (length s - emptySpace s) (compressAcc s (reverse s))
        where
            compressAcc :: [Maybe Int] -> [Maybe Int] -> [Int]
            compressAcc [] _ = []
            compressAcc _ [] = []
            compressAcc a (Nothing:bs) = compressAcc a bs
            compressAcc (Just a:as) b = a : compressAcc as b
            compressAcc (Nothing:as) (Just b:bs) = b : compressAcc as bs

            emptySpace :: [Maybe Int] -> Int
            emptySpace = foldr (\x i -> case x of
                Nothing -> i + 1
                deault -> i) 0

    calculateChecksum :: [Int] -> Int
    calculateChecksum = calculateChecksumAcc 0
        where
            calculateChecksumAcc :: Int -> [Int] -> Int
            calculateChecksumAcc _ [] = 0
            calculateChecksumAcc pos (x:xs) = pos * x + calculateChecksumAcc (pos + 1) xs

    puzzle1 :: String -> Int
    puzzle1 = calculateChecksum . compress . parseInput

    puzzle2 :: String -> Int
    puzzle2 = undefined