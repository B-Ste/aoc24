module Main where

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input
        print . puzzle2 $ input

    puzzle1 :: String -> Int
    puzzle1 = length . filter safeReport . parseInput 

    puzzle2 :: String -> Int
    puzzle2 = length . filter safeReportDampened . parseInput

    parseInput :: String -> [[Int]]
    parseInput = map (map read . words) . lines

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