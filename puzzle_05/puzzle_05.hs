module Main where
    import Data.Map (Map)
    import qualified Data.Map as Map

    main :: IO()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input
        print . puzzle2 $ input

    divide :: String -> ([String], [String])
    divide = (\(a, b) -> (a, tail b)) . break null . lines

    buildMap :: [String] -> Map Int [Int] 
    buildMap = buildMapAcc Map.empty
        where
            buildMapAcc :: Map Int [Int] -> [String] -> Map Int [Int] 
            buildMapAcc acc [] = acc
            buildMapAcc acc ([a, b, '|', c, d]:xs)
                | Map.member (stringToInt [a, b]) acc = let (Just x) = Map.lookup (stringToInt [a, b]) acc 
                    in buildMapAcc (Map.insert (stringToInt [a, b]) (stringToInt [c, d]:x) acc) xs
                | otherwise = buildMapAcc (Map.insert (stringToInt [a, b]) [stringToInt [c, d]] acc) xs

    stringToInt :: String -> Int
    stringToInt = read

    convertInt :: String -> [Int]
    convertInt [a, b] = [stringToInt [a, b]]
    convertInt (a:b:',':xs) = stringToInt [a, b]:convertInt xs

    evaluateUpdate :: [Int] -> Map Int [Int] -> Int
    evaluateUpdate x m
        | validUpdate x m = middle x
        | otherwise = 0

    evaluateUpdateCorrrected :: [Int] -> Map Int [Int] -> Int
    evaluateUpdateCorrrected x m
        | not $ validUpdate x m = middle (topoSort x m)
        | otherwise = 0

    validUpdate :: [Int] -> Map Int [Int] -> Bool
    validUpdate [] _ = True
    validUpdate (x:xs) m
        | not $ containedInMap x xs m = validUpdate xs m
        | otherwise = False

    containedInMap :: Int -> [Int] -> Map Int [Int] -> Bool
    containedInMap x [] _ = False
    containedInMap x (y:ys) m
        | let (Just a) = Map.lookup y m in elem x a = True
        | otherwise = containedInMap x ys m

    middle :: [Int] -> Int
    middle x = x !! max 0 (div (length x) 2)

    topoSort :: [Int] -> Map Int [Int] -> [Int]
    topoSort x m = topoSortAcc x m []
        where
            topoSortAcc :: [Int] -> Map Int [Int] -> [Int] -> [Int]
            topoSortAcc [] _ acc = acc
            topoSortAcc (x:xs) m acc = topoSortAcc xs m (insert acc x m)

            insert :: [Int] -> Int -> Map Int [Int] -> [Int]
            insert [] x m = [x]
            insert (y:ys) x m 
                | containedInMap x [y] m = y : insert ys x m
                | otherwise = x:y:ys

    puzzle1 :: String -> Int
    puzzle1 input = calculate (snd . divide $ input) (buildMap . fst . divide $ input)
        where
            calculate :: [String] -> Map Int [Int] -> Int
            calculate [] _ = 0
            calculate (x:ys) m = evaluateUpdate (convertInt x) m + calculate ys m

    puzzle2 :: String -> Int
    puzzle2 input = calculate (snd . divide $ input) (buildMap . fst . divide $ input)
        where
            calculate :: [String] -> Map Int [Int] -> Int
            calculate [] _ = 0
            calculate (x:xs) m = evaluateUpdateCorrrected (convertInt x) m + calculate xs m 