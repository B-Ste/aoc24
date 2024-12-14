module Main where

    type Equation = (Int, [Int])

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        (print . puzzle1) input
        (print . puzzle2) input

    parseEqs :: String -> [Equation]
    parseEqs s = map parseEq (lines s)
        where
            parseEq :: String -> Equation
            parseEq s = let ws = words s in (read (take (length (head ws) - 1) (head ws)), map read (tail ws))

    solvable :: Equation -> Bool
    solvable (k, [x]) = k == x
    solvable (k, x:y:xs)
        | x > k = False
        | otherwise = solvable (k, (x * y):xs) || solvable (k, (x + y):xs)

    solvableExt :: Equation -> Bool
    solvableExt (k, [x]) = k == x
    solvableExt (k, x:y:xs)
        | x > k = False
        | otherwise = solvableExt (k, intConcat x y:xs) || solvableExt (k, (x + y):xs) || solvableExt (k, (x * y):xs)

    intConcat :: Int -> Int -> Int
    intConcat x y = read (show x ++ show y)

    calculate :: (Equation -> Bool) -> [Equation] -> Int
    calculate f = sum . map fst . filter f

    puzzle1 :: String -> Int
    puzzle1 input = calculate solvable (parseEqs input)

    puzzle2 :: String -> Int
    puzzle2 input = calculate solvableExt (parseEqs input)
