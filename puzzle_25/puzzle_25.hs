module Main where
    import Data.List (transpose)

    type Lock = (Int, Int, Int, Int, Int)
    type Key = Lock

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input

    group :: String -> [[String]]
    group = split 7 . filter (/= "") . lines
        where
            split :: Int -> [String] -> [[String]]
            split _ [] = []
            split i l = take i l : split i (drop i l)

    locks :: [[String]] -> [Lock]
    locks = map (parse . transpose . tail) . filter (\(x:_) -> x == "#####")

    keys :: [[String]] -> [Key]
    keys = map (parse . transpose . init) . filter (\k -> let x = last k in x == "#####")

    parse :: [String] -> Lock
    parse [a, b, c, d, e] = (count a, count b, count c, count d, count e)
        where
            count = length . filter (== '#')

    combinations :: [Lock] -> [Key] -> [(Lock, Key)]
    combinations ls ks = filter (
        \((l1, l2, l3, l4, l5), (k1, k2, k3, k4, k5)) -> l1 + k1 <= 5 && l2 + k2 <= 5 && l3 + k3 <= 5 && l4 + k4 <= 5 && l5 + k5 <= 5
        ) [(l, k) | l <- ls, k <- ks]

    puzzle1 :: String -> Int
    puzzle1 s = let g = group s; l = locks g; k = keys g in length $ combinations l k
