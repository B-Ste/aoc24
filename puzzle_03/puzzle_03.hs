module Main where

    import Data.Char (isDigit)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        putStr (puzzle1 input)
        putStr (puzzle2 input)

    puzzle1 :: String -> String
    puzzle1 input = show (calculate (parseMul input []) 0) ++ "\n"
        where
            parseMul :: String -> [(Int, Int)] -> [(Int, Int)]
            parseMul [_, _, _] t = t
            parseMul ('m':'u':'l':'(':a0:',':b0:')':xs) t 
                | isDigit a0 && isDigit b0 = parseMul xs ((read [a0], read [b0]):t)
                | otherwise = parseMul ('u':'l':'(':a0:',':b0:')':xs) t
            parseMul ('m':'u':'l':'(':a0:a1:',':b0:')':xs) t 
                | isDigit a0 && isDigit a1 && isDigit b0 = parseMul xs ((read [a0, a1], read [b0]):t)
                | otherwise = parseMul ('u':'l':'(':a0:a1:',':b0:')':xs) t
            parseMul ('m':'u':'l':'(':a0:a1:a2:',':b0:')':xs) t 
                | isDigit a0 && isDigit a1 && isDigit a2 && isDigit b0 = parseMul xs ((read [a0, a1, a2], read [b0]):t)
                | otherwise = parseMul ('u':'l':'(':a0:a1:a2:',':b0:')':xs) t
            parseMul ('m':'u':'l':'(':a0:',':b0:b1:')':xs) t 
                | isDigit a0 && isDigit b0 && isDigit b1 = parseMul xs ((read [a0], read [b0, b1]):t)
                | otherwise = parseMul ('u':'l':'(':a0:',':b0:b1:')':xs) t
            parseMul ('m':'u':'l':'(':a0:a1:',':b0:b1:')':xs) t 
                | isDigit a0 && isDigit a1 && isDigit b0 && isDigit b1 = parseMul xs ((read [a0, a1], read [b0, b1]):t)
                | otherwise = parseMul ('u':'l':'(':a0:a1:',':b0:b1:')':xs) t
            parseMul ('m':'u':'l':'(':a0:a1:a2:',':b0:b1:')':xs) t 
                | isDigit a0 && isDigit a1 && isDigit a2 && isDigit b0 && isDigit b1 = parseMul xs ((read [a0, a1, a2], read [b0, b1]):t)
                | otherwise = parseMul ('u':'l':'(':a0:a1:a2:',':b0:b1:')':xs) t
            parseMul ('m':'u':'l':'(':a0:',':b0:b1:b2:')':xs) t 
                | isDigit a0 && isDigit b0 && isDigit b1 && isDigit b2 = parseMul xs ((read [a0], read [b0, b1, b2]):t)
                | otherwise = parseMul ('u':'l':'(':a0:',':b0:b1:b2:')':xs) t
            parseMul ('m':'u':'l':'(':a0:a1:',':b0:b1:b2:')':xs) t 
                | isDigit a0 && isDigit a1 && isDigit b0 && isDigit b1 && isDigit b2 = parseMul xs ((read [a0, a1], read [b0, b1, b2]):t)
                | otherwise = parseMul ('u':'l':'(':a0:a1:',':b0:b1:b2:')':xs) t
            parseMul ('m':'u':'l':'(':a0:a1:a2:',':b0:b1:b2:')':xs) t 
                | isDigit a0 && isDigit a1 && isDigit a2 && isDigit b0 && isDigit b1 && isDigit b2 = parseMul xs ((read [a0, a1, a2], read [b0, b1, b2]):t)
                | otherwise = parseMul ('u':'l':'(':a0:a1:a2:',':b0:b1:b2:')':xs) t
            parseMul (x:xs) t = parseMul xs t

            calculate :: [(Int, Int)] -> Int -> Int
            calculate [] acc = acc
            calculate ((a,b):xs) acc = calculate xs (acc + a * b)

    puzzle2 :: String -> String
    puzzle2 input = "undefined\n"
