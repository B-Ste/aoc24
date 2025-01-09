module Main where
    import Data.Bits (xor, shiftL, shiftR)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input

    parse :: String -> [Int]
    parse = map read . lines

    advance :: Int -> Int
    advance = p3 . p2 . p1
        where
            p1 :: Int -> Int
            p1 t = prune $ xor t $ shiftL t 6

            p2 :: Int -> Int
            p2 t = prune $ xor t $ shiftR t 5

            p3 :: Int -> Int
            p3 t = prune $ xor t $ shiftL t 11

            prune :: Int -> Int
            prune t = t `mod` 16777216

    secrets :: Int -> [Int]
    secrets = iterate advance

    puzzle1 :: String -> Int
    puzzle1 = sum . map (\t -> secrets t !! 2000) . parse