{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
module Main where

    import Data.Char (isDigit)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        putStr (puzzle1 input)
        putStr (puzzle2 input)

    puzzle1 :: String -> String
    puzzle1 input = show (calculate (parseMul input False False [])) ++ "\n"

    puzzle2 :: String -> String
    puzzle2 input = show (calculate (parseMul input True True [])) ++ "\n"

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
    calculate x = foldl (\i (a, b) -> i + a * b) 0 x
