module Main where
    import Data.List (sort, isPrefixOf)

    newtype LString = LS String
        deriving (Eq, Show)
    instance Ord LString where
        compare :: LString -> LString -> Ordering
        compare (LS a) (LS b)
            | length a > length b = LT
            | length a == length b = EQ
            | otherwise = GT

    wordsWhen :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =  case dropWhile p s of
        "" -> []
        s' -> w : wordsWhen p s''
            where (w, s'') = break p s'

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input

    parseInput :: String -> ([LString], [String])
    parseInput s = (sort . map LS . wordsWhen (\t -> t == ' ' || t == ',') . head . lines $ s, drop 2 . lines $ s)

    prefixes :: String -> [LString] -> [LString]
    prefixes s = filter (\(LS t) -> t `isPrefixOf` s)

    solvable :: String -> [LString] -> [LString] -> Bool
    solvable [] _ _ = True
    solvable _ [] _ = False
    solvable s ((LS p):pre) ges = solvable ss (prefixes ss ges) ges || solvable s pre ges
        where
            ss = drop (length p) s

    puzzle1 :: String -> Int
    puzzle1 s = let (t, p) = parseInput s in length . filter (\s -> solvable s (prefixes s t) t) $ p
