module Main where

    import Data.Char (isDigit)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input
        print . puzzle2 $ input

    puzzle1 :: String -> Int
    puzzle1 input = calculate $ parseMul input False False []

    puzzle2 :: String -> Int
    puzzle2 input = calculate $ parseMul input True True []

    -- first bool enables conditional parsing, second bool holds starting value
    parseMul :: String -> Bool -> Bool -> [(Int, Int)] -> [(Int, Int)]
    parseMul [_, _, _] en st t = t
    parseMul ('m':'u':'l':'(':a0:',':b0:')':xs) en st t 
        | (not en || st) && isDigit a0 && isDigit b0 = parseMul xs en st ((read [a0], read [b0]):t)
        | otherwise = parseMul ('u':'l':'(':a0:',':b0:')':xs) en st t
    parseMul ('m':'u':'l':'(':a0:a1:',':b0:')':xs) en st t 
        | (not en || st) && isDigit a0 && isDigit a1 && isDigit b0 = parseMul xs en st ((read [a0, a1], read [b0]):t)
        | otherwise = parseMul ('u':'l':'(':a0:a1:',':b0:')':xs) en st t
    parseMul ('m':'u':'l':'(':a0:a1:a2:',':b0:')':xs) en st t 
        | (not en || st) && isDigit a0 && isDigit a1 && isDigit a2 && isDigit b0 = parseMul xs en st ((read [a0, a1, a2], read [b0]):t)
        | otherwise = parseMul ('u':'l':'(':a0:a1:a2:',':b0:')':xs) en st t
    parseMul ('m':'u':'l':'(':a0:',':b0:b1:')':xs) en st t 
        | (not en || st) && isDigit a0 && isDigit b0 && isDigit b1 = parseMul xs en st ((read [a0], read [b0, b1]):t)
        | otherwise = parseMul ('u':'l':'(':a0:',':b0:b1:')':xs) en st t
    parseMul ('m':'u':'l':'(':a0:a1:',':b0:b1:')':xs) en st t 
        | (not en || st) && isDigit a0 && isDigit a1 && isDigit b0 && isDigit b1 = parseMul xs en st ((read [a0, a1], read [b0, b1]):t)
        | otherwise = parseMul ('u':'l':'(':a0:a1:',':b0:b1:')':xs) en st t
    parseMul ('m':'u':'l':'(':a0:a1:a2:',':b0:b1:')':xs) en st t 
        | (not en || st) && isDigit a0 && isDigit a1 && isDigit a2 && isDigit b0 && isDigit b1 = parseMul xs en st ((read [a0, a1, a2], read [b0, b1]):t)
        | otherwise = parseMul ('u':'l':'(':a0:a1:a2:',':b0:b1:')':xs) en st t
    parseMul ('m':'u':'l':'(':a0:',':b0:b1:b2:')':xs) en st t 
        | (not en || st) && isDigit a0 && isDigit b0 && isDigit b1 && isDigit b2 = parseMul xs en st ((read [a0], read [b0, b1, b2]):t)
        | otherwise = parseMul ('u':'l':'(':a0:',':b0:b1:b2:')':xs) en st t
    parseMul ('m':'u':'l':'(':a0:a1:',':b0:b1:b2:')':xs) en st t 
        | (not en || st) && isDigit a0 && isDigit a1 && isDigit b0 && isDigit b1 && isDigit b2 = parseMul xs en st ((read [a0, a1], read [b0, b1, b2]):t)
        | otherwise = parseMul ('u':'l':'(':a0:a1:',':b0:b1:b2:')':xs) en st t
    parseMul ('m':'u':'l':'(':a0:a1:a2:',':b0:b1:b2:')':xs) en st t 
        | (not en || st) && isDigit a0 && isDigit a1 && isDigit a2 && isDigit b0 && isDigit b1 && isDigit b2 = parseMul xs en st ((read [a0, a1, a2], read [b0, b1, b2]):t)
        | otherwise = parseMul ('u':'l':'(':a0:a1:a2:',':b0:b1:b2:')':xs) en st t
    parseMul ('d':'o':'(':')':xs) en _ t = parseMul xs en True t
    parseMul ('d':'o':'n':'\'':'t':'(':')':xs) en _ t = parseMul xs en False t
    parseMul (x:xs) en st t = parseMul xs en st t

    calculate :: [(Int, Int)] -> Int
    calculate = foldl (\i (a, b) -> i + a * b) 0
