module Main where
    import Data.Bits (xor)
    import Control.Monad (void)

    -- A B C PC Prog
    data Computer = C Int Int Int Int [Int]

    wordsWhen     :: (Char -> Bool) -> String -> [String]
    wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        puzzle1 input

    parseInput :: String -> Computer
    parseInput s = let
        ls = lines s
        a = read . last . words . head $ ls
        b = read . last . words $ ls !! 1
        c = read . last . words $ ls !! 2
        in C a b c 0 $ tail . map read . wordsWhen (\x -> x == ' ' || x == ',') $ ls !! 4

    evaluate :: IO Computer -> IO Computer
    evaluate mc = do
        comp <- mc
        let (C a b c pc p) = comp in
            if pc >= length p then return (C a b c pc p)
            else case p !! pc of
                0 -> evaluate $ return $ C (truncate $ toRational a / (2 ^ comb comp (op p pc))) b c (pc + 2) p
                1 -> evaluate $ return $ C a (b `xor` op p pc) c (pc + 2) p
                2 -> evaluate $ return $ C a (comb comp (op p pc) `mod` 8) c (pc + 2) p
                3 -> if a /= 0
                    then evaluate $ return $ C a b c (op p pc) p
                    else evaluate $ return $ C a b c (pc + 2) p
                4 -> evaluate $ return $ C a (b `xor` c) c (pc + 2) p
                5 -> putStr (show (comb comp (op p pc) `mod` 8) ++ [',']) >> evaluate (return $ C a b c (pc + 2) p)
                6 -> evaluate $ return $ C a (truncate $ toRational a / (2 ^ comb comp (op p pc))) c (pc + 2) p
                7 -> evaluate $ return $ C a b (truncate $ toRational a / (2 ^ comb comp (op p pc))) (pc + 2) p
        where
            op :: [Int] -> Int -> Int
            op l i = l !! (i + 1)

            comb :: Computer -> Int -> Int
            comb _ 0 = 0
            comb _ 1 = 1
            comb _ 2 = 2
            comb _ 3 = 3
            comb (C a b c pc p) 4 = a
            comb (C a b c pc p) 5 = b
            comb (C a b c pc p) 6 = c

    puzzle1 :: String -> IO ()
    puzzle1 s = Control.Monad.void (evaluate (return $ parseInput s) >> putChar '\n')
