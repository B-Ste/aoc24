module Main where

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        putStr (puzzle1 input)
        putStr (puzzle2 input)

    puzzle1 :: String -> String
    puzzle1 input = show (calculate (parseInput input)) ++ "\n"
        where
            calculate :: [[Int]] -> Int
            calculate x = length (filter safeReport x)

    puzzle2 :: String -> String
    puzzle2 input = show (calculate (parseInput input)) ++ "\n"
        where
            calculate :: [[Int]] -> Int
            calculate x = length (filter safeReportDampened x)

    parseInput :: String -> [[Int]]
    parseInput input = map (map read . words) (lines input)

    safeReport :: [Int] -> Bool
    safeReport (x:xs) = let
        f   | x - head xs < 0 = (\x y -> y - x >= 1 && y - x <= 3)
            | otherwise = (\x y -> x - y >= 1 && x - y <= 3)
        in safeReportTyped (x:xs) f

    safeReportTyped :: [Int] -> (Int -> Int -> Bool) -> Bool
    safeReportTyped [x] _ = True
    safeReportTyped (x:xs) f
        | f x (head xs) = safeReportTyped xs f
        | otherwise = False

    safeReportDampened :: [Int] -> Bool
    safeReportDampened = safeReportDampenedAcc []

    safeReportDampenedAcc :: [Int] -> [Int] -> Bool
    safeReportDampenedAcc ys [] = safeReport ys
    safeReportDampenedAcc ys (x:xs) = safeReport (ys ++ xs) || safeReportDampenedAcc (ys ++ [x]) xs