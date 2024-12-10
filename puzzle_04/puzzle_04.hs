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

    puzzle2 :: String -> String
    puzzle2 input = "undefined\n"

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
            diagonaliseLines xs [] acc = diagonaliseLines (removeFirstElements xs) [] (acc ++ [firstElementsToString xs])
            diagonaliseLines xs (y:ys) acc = diagonaliseLines (removeFirstElements (xs ++ [y])) ys (acc ++ [firstElementsToString (xs ++ [y])])

    firstElementsToString :: [String] -> String
    firstElementsToString = map head

    removeFirstElements :: [String] -> [String]
    removeFirstElements s = filter (/= "") (map tail s)
