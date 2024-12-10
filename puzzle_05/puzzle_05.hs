module Main where
    import Data.Map (Map)
    import qualified Data.Map as Map

    main :: IO()
    main = do
        input <- readFile "input.txt"
        (print . puzzle1) input
        (print . puzzle2) input

    puzzle1 :: String -> Int
    puzzle1 input = calculate (lines input) Map.empty 0
        where
            calculate :: [String] -> Map Int [Int] -> Int -> Int
            calculate [] _ acc = acc
            calculate ([a, b, '|', c, d]:xs) m acc
                | Map.member (stringToInt [a, b]) m = let (Just x) = Map.lookup (stringToInt [a, b]) m in calculate xs (Map.insert (stringToInt [a, b]) (stringToInt [c, d]:x) m) acc
                | otherwise = calculate xs (Map.insert (stringToInt [a, b]) [stringToInt [c, d]] m) acc
            calculate ((a:b:',':xs):ys) m acc = calculate ys m (acc + evaluateUpdate (convertInt (a:b:',':xs)) m)
            calculate (x:xs) m acc = calculate xs m acc

            stringToInt :: String -> Int
            stringToInt = read

            convertInt :: String -> [Int]
            convertInt [a, b] = [stringToInt [a, b]]
            convertInt (a:b:',':xs) = stringToInt [a, b]:convertInt xs

            evaluateUpdate :: [Int] -> Map Int [Int] -> Int
            evaluateUpdate x m
                | validUpdate x m = middle x
                | otherwise = 0

            validUpdate :: [Int] -> Map Int [Int] -> Bool
            validUpdate [] _ = True
            validUpdate (x:xs) m
                | not (containedInMap x xs m) = validUpdate xs m
                | otherwise = False

            containedInMap :: Int -> [Int] -> Map Int [Int] -> Bool
            containedInMap x [] _ = False
            containedInMap x (y:ys) m
                | let (Just a) = Map.lookup y m in elem x a = True
                | otherwise = containedInMap x ys m

            middle :: [Int] -> Int
            middle x = x !! max 0 (div (length x) 2)

    puzzle2 :: String -> Int
    puzzle2 input = undefined