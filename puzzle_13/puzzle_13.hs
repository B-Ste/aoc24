{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use mapMaybe" #-}
module Main where
    import Data.Ratio ((%))
    import Data.Maybe (catMaybes)

    data Problem = P (Rational, Rational) (Rational, Rational) (Rational, Rational)

    type Solution = (Int, Int)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        (print . puzzle1) input

    parseInput :: String -> [Problem]
    parseInput = parseInputAcc . filter (/= "") . lines
        where
            parseInputAcc :: [String] -> [Problem]
            parseInputAcc [] = []
            parseInputAcc (a:b:g:xs) = P (parseLine (words a)) (parseLine (words b)) (parseLine (words g)) : parseInputAcc xs

            parseLine :: [String] -> (Rational, Rational)
            parseLine (('X':a:xs):('Y':b:ys):ls) = (read (init xs) % 1, read ys % 1)
            parseLine (x:xs) = parseLine xs

    solve :: Problem -> Maybe Solution
    solve (P (ax, ay) (bx, by) (gx, gy))
        | fromIntegral (truncate a) == a && fromIntegral (truncate b) == b = Just (fromIntegral (truncate a), fromIntegral (truncate b))
        | otherwise = Nothing
        where
            r = ay / ax
            b = (gy - gx * r) / (by - bx * r)
            a = (gx - bx * b) / ax

    cost :: Solution -> Int
    cost (a, b) = 3 * a + b

    calculate :: (Problem -> Problem) -> String -> Int
    calculate pp = sum . map cost . catMaybes . map (solve . pp) . parseInput

    puzzle1 :: String -> Int
    puzzle1 = calculate id