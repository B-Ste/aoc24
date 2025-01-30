module Main where

    main :: IO()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input
        print . puzzle2 $ input

    puzzle1 :: String -> Int
    puzzle1 input = parseXMASStringSet input
        + parseXMASStringSet (reverse input)
        + parseXMASStringSet (transpose input)
        + parseXMASStringSet (transpose (reverse input))
        + parseXMASStringSet (diagonalise input)
        + parseXMASStringSet (diagonalise (transpose input))
        + parseXMASStringSet (diagonalise (unlines (reverse (lines input))))
        + parseXMASStringSet (diagonalise (transpose (unlines (reverse (lines input)))))

    parseXMASStringSet :: String -> Int
    parseXMASStringSet [] = 0
    parseXMASStringSet ('X':'M':'A':'S':xs) = 1 + parseXMASStringSet xs
    parseXMASStringSet (x:xs) = parseXMASStringSet xs

    transpose :: String -> String
    transpose = unlines . transposeLines . lines
        where
            transposeLines :: [String] -> [String]
            transposeLines ([]:xs) = []
            transposeLines s = map head s : transposeLines (map tail s)

    diagonalise :: String -> String
    diagonalise = unlines . diagonaliseLines [] . lines
        where
            diagonaliseLines :: [String] -> [String] -> [String]
            diagonaliseLines [] [] = []
            diagonaliseLines xs [] = map head xs : diagonaliseLines (removeFirstElements xs) []
            diagonaliseLines xs (y:ys) = map head (xs ++ [y]) : diagonaliseLines (removeFirstElements (xs ++ [y])) ys

    removeFirstElements :: [String] -> [String]
    removeFirstElements = filter (/= "") . map tail

    puzzle2 :: String -> Int
    puzzle2 = parseXMASString . lines

    parseXMASString :: [String] -> Int
    parseXMASString (a:b:c:xs) = masCount a b c + parseXMASString (b:c:xs)
    parseXMASString _ = 0

    masCount :: String -> String -> String -> Int
    masCount [] [] [] = 0
    masCount ('M':a:'M':xs) (b:'A':c:ys) ('S':d:'S':zs) = 1 + masCount (a:'M':xs) ('A':c:ys) (d:'S':zs)
    masCount ('S':a:'M':xs) (b:'A':c:ys) ('S':d:'M':zs) = 1 + masCount (a:'M':xs) ('A':c:ys) (d:'M':zs)
    masCount ('M':a:'S':xs) (b:'A':c:ys) ('M':d:'S':zs) = 1 + masCount (a:'S':xs) ('A':c:ys) (d:'S':zs)
    masCount ('S':a:'S':xs) (b:'A':c:ys) ('M':d:'M':zs) = 1 + masCount (a:'S':xs) ('A':c:ys) (d:'M':zs)
    masCount (a:xs) (b:ys) (c:zs) = masCount xs ys zs
