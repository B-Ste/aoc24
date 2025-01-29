module Main where

    import Data.List (sort)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input
        print . puzzle2 $ input

    puzzle1 :: String -> Int
    puzzle1 input = sum . zipWith (\a b -> abs (a - b)) (list1 input) $ list2 input

    puzzle2 :: String -> Int
    puzzle2 input = sum . map (\x -> x * length (filter (==x) l2)) $ list1 input
        where
            l2 = list2 input

    list1 :: String -> [Int]
    list1 = sort . map (read . head . words) . lines

    list2 :: String -> [Int]
    list2 = sort . map (read . head . tail . words) . lines
