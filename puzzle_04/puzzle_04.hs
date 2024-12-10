module Main where

    main :: IO()
    main = do
        input <- readFile "input.txt"
        print (puzzle1 input)
        print (puzzle2 input)

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
    parseXMASStringSet x = parseXMASString x 0

    parseXMASString :: String -> Int -> Int
    parseXMASString [] i = i
    parseXMASString ('X':'M':'A':'S':xs) i = parseXMASString xs (i + 1)
    parseXMASString (x:xs) i = parseXMASString xs i

    transpose :: String -> String
    transpose [] = []
    transpose s = unlines (transposeLines (lines s))
        where
            transposeLines :: [String] -> [String]
            transposeLines ([]:xs) = []
            transposeLines s = map head s : transposeLines (map tail s)

    diagonalise :: String -> String
    diagonalise [] = []
    diagonalise s = unlines (diagonaliseLines [] (lines s) [])
        where
            diagonaliseLines :: [String] -> [String] -> [String] -> [String]
            diagonaliseLines [] [] acc = acc
            diagonaliseLines xs [] acc = diagonaliseLines (removeFirstElements xs) [] (acc ++ [map head xs])
            diagonaliseLines xs (y:ys) acc = diagonaliseLines (removeFirstElements (xs ++ [y])) ys (acc ++ [map head (xs ++ [y])])

    removeFirstElements :: [String] -> [String]
    removeFirstElements s = filter (/= "") (map tail s)

    puzzle2 :: String -> Int
    puzzle2 input = parseX_MASString (lines input) 0

    parseX_MASString :: [String] -> Int -> Int
    parseX_MASString (a:b:c:xs) i = parseX_MASString (b:c:xs) (i + masCount a b c 0)
    parseX_MASString _ i = i

    masCount :: String -> String -> String -> Int -> Int
    masCount [] [] [] i = i
    masCount ('M':a:'M':xs) (b:'A':c:ys) ('S':d:'S':zs) i = masCount (a:'M':xs) ('A':c:ys) (d:'S':zs) (i + 1)
    masCount ('S':a:'M':xs) (b:'A':c:ys) ('S':d:'M':zs) i = masCount (a:'M':xs) ('A':c:ys) (d:'M':zs) (i + 1)
    masCount ('M':a:'S':xs) (b:'A':c:ys) ('M':d:'S':zs) i = masCount (a:'S':xs) ('A':c:ys) (d:'S':zs) (i + 1)
    masCount ('S':a:'S':xs) (b:'A':c:ys) ('M':d:'M':zs) i = masCount (a:'S':xs) ('A':c:ys) (d:'M':zs) (i + 1)
    masCount (a:xs) (b:ys) (c:zs) i = masCount xs ys zs i
