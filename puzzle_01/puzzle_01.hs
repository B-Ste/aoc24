module Main where

    import Data.List (sort)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        putStr (puzzle1 input)
        putStr (puzzle2 input)

    puzzle1 :: String -> String
    puzzle1 input = show (calculate (assembleLists input) 0) ++ "\n"
        where
            calculate :: ([Int], [Int]) -> Int -> Int
            calculate ([], []) acc = acc
            calculate (x:xs, y:ys) acc = calculate (xs, ys) (acc + abs (x - y))

    puzzle2 :: String -> String
    puzzle2 input = show (calculate (assembleLists input) 0) ++ "\n"
        where
            -- n^2 algorithm is tolerable for an input of only 1000 lines
            calculate :: ([Int], [Int]) -> Int -> Int
            calculate ([], _) acc = acc
            calculate (x:xs, l2) acc = calculate (xs, l2) (acc + x * getTimes x l2 0)

            getTimes :: Int -> [Int] -> Int -> Int
            getTimes _ [] acc = acc
            getTimes t (x:xs) acc 
                | t == x = getTimes t xs (acc + 1)
                | otherwise = getTimes t xs acc

    assembleLists :: String -> ([Int], [Int])
    assembleLists input = assembleListsAcc (lines input) ([], [])

    assembleListsAcc :: [String] -> ([Int], [Int]) -> ([Int], [Int])
    assembleListsAcc [] (l1, l2)     = (sort l1, sort l2)
    assembleListsAcc (x:xs) (l1, l2) = assembleListsAcc xs (read (head (words x)):l1, read (head (tail (words x))):l2)