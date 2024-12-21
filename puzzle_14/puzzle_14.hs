module Main where
    import Data.List (sort, nub)
    import Control.Monad (when, void)

    data Robot = R (Int, Int) (Int, Int)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        (print . puzzle1) input
        puzzle2 input

    parseInput :: String -> [Robot]
    parseInput = map parseRobot . lines
        where
            parseRobot :: String -> Robot
            parseRobot s = R (read (k !! 1), read (k !! 2)) (read (k !! 4), read (k !! 5))
                where
                    k = wordsWhen (\x -> x == ' ' || x == ',' || x == '=') s

                    wordsWhen :: (Char -> Bool) -> String -> [String]
                    wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

    move :: Int -> Robot -> Robot
    move i (R (x, y) (dx, dy)) = R ((x + i * dx) `mod` 101, (y + i * dy) `mod` 103) (dx, dy)

    calculateQuadrats :: [Robot] -> Int
    calculateQuadrats = calculateQuadratsAcc 0 0 0 0
        where
            calculateQuadratsAcc :: Int -> Int -> Int -> Int -> [Robot] -> Int
            calculateQuadratsAcc q1 q2 q3 q4 [] = q1 * q2 * q3 * q4
            calculateQuadratsAcc q1 q2 q3 q4 ((R (x, y) _):xs)
                | x < 50 && y < 51 = calculateQuadratsAcc (q1 + 1) q2 q3 q4 xs
                | x < 50 && y > 51 = calculateQuadratsAcc q1 (q2 + 1) q3 q4 xs
                | x > 50 && y < 51 = calculateQuadratsAcc q1 q2 (q3 + 1) q4 xs
                | x > 50 && y > 51 = calculateQuadratsAcc q1 q2 q3 (q4 + 1) xs
                | otherwise = calculateQuadratsAcc q1 q2 q3 q4 xs

    -- (x, y) -> (y, x) to facilitate sorting
    position :: Robot -> (Int, Int)
    position (R (x, y) _) = (y, x)

    printRobots :: [Robot] -> IO ()
    printRobots = printRobotsAcc 0 0 . sort . nub . map position
        where
            printRobotsAcc :: Int -> Int -> [(Int, Int)] -> IO ()
            printRobotsAcc x y []
                | x == 101 && y == 102 = void $ putChar '\n'
                | x == 101 = putChar '\n' >> printRobotsAcc 0 (y + 1) []
                | otherwise = putChar ' ' >> printRobotsAcc (x + 1) y []
            printRobotsAcc x y ((ry, rx):xs)
                | x == 101 && y == 102 = void $ putChar '\n'
                | x == 101 = putChar '\n' >> printRobotsAcc 0 (y + 1) ((ry, rx):xs)
                | x == rx && y == ry = putChar '*' >> printRobotsAcc (x + 1) y xs
                | otherwise = putChar ' ' >> printRobotsAcc (x + 1) y ((ry, rx):xs)

    puzzle1 :: String -> Int
    puzzle1 = calculateQuadrats . map (move 100) . parseInput

    puzzle2 :: String -> IO ()
    puzzle2 = puzzle2Acc 0 . parseInput
        where
            puzzle2Acc :: Int -> [Robot] -> IO ()
            puzzle2Acc i r = let x = 8087 + i * 10403 in do
                print x
                printRobots (map (move x) r)
                user <- getLine
                when (user == "") $ puzzle2Acc (i + 1) r