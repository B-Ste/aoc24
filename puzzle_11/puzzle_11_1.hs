module Main where

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        (print . puzzle1) input

    parseInput :: String -> [Int]
    parseInput = map read . words

    evaluate :: [Int] -> [Int]
    evaluate = concatMap evaluateSingle
        where
            evaluateSingle :: Int -> [Int]
            evaluateSingle 0 = [1]
            evaluateSingle x
                | let 
                    xs = show x
                    l = length xs `div` 2,
                    (even . length) xs = [read (take l xs), read (drop l xs)]
                | otherwise = [x * 2024]

    puzzle1 :: String -> Int
    puzzle1 s = length (iterate evaluate (parseInput s) !! 25)
