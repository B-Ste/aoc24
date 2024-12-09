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

            assembleLists :: String -> ([Int], [Int])
            assembleLists input = assembleListsAcc (lines input) ([], [])

            assembleListsAcc :: [String] -> ([Int], [Int]) -> ([Int], [Int])
            assembleListsAcc [] (l1, l2)     = (sort l1, sort l2)
            assembleListsAcc (x:xs) (l1, l2) = assembleListsAcc xs (read (head (words x)):l1, read (head (tail (words x))):l2)

    puzzle2 :: String -> String
    puzzle2 _ = "undefined\n"