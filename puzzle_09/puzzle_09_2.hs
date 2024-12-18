module Main where

    type Section = (Maybe Int, Int)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        (print . puzzle2) input

    parseInput :: String -> [Section]
    parseInput = parseInputAcc 0
        where
            parseInputAcc :: Int -> String -> [Section]
            parseInputAcc id [x] = [(Just id, read [x])]
            parseInputAcc id (x:y:as) = (Just id, read [x]) : (Nothing, read [y]) : parseInputAcc (id + 1) as

    compress :: [Section] -> [Section]
    compress [] = []
    compress s =
        let ls = last s in
        case ls of
            (Nothing, ai) -> compress (init s) ++ [(Nothing, ai)]
            (Just a, ai)
                | checkFit ai s -> compress (insert (Just a, ai) (init s) ++ [(Nothing, ai)])
                | otherwise -> compress (init s) ++ [(Just a, ai)]

    checkFit :: Int -> [Section] -> Bool
    checkFit _ [] = False
    checkFit s ((Just a, _):as) = checkFit s as
    checkFit s ((Nothing, x):as)
        | x >= s = True
        | otherwise = checkFit s as

    insert :: Section -> [Section] -> [Section]
    insert _ [] = error "\ncan't insert"
    insert (Nothing, _) _ = error "\ninserting nothing"
    insert s ((Just b, bi):as) = (Just b, bi) : insert s as
    insert (Just a, ai) ((Nothing, bi):as)
        | ai < bi = (Just a, ai) : (Nothing, bi - ai) : as
        | ai == bi = (Just a, ai) : as
        | otherwise = (Nothing, bi) : insert (Just a, ai) as

    calculateChecksum :: [Section] -> Int
    calculateChecksum = calculateChecksumAcc 0
        where
            calculateChecksumAcc :: Int -> [Section] -> Int
            calculateChecksumAcc _ [] = 0
            calculateChecksumAcc i ((a, ai):as) = calculateSectionChecksum i (a, ai) + calculateChecksumAcc (i + ai) as

            calculateSectionChecksum :: Int -> Section -> Int
            calculateSectionChecksum _ (Nothing, _) = 0
            calculateSectionChecksum i (Just a, ai) = (sum . zipWith (*) (iterate (+1) i)) (replicate ai a)

    puzzle2 :: String -> Int
    puzzle2 = calculateChecksum . compress . parseInput