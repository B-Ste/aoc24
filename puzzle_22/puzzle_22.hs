module Main where
    import Data.Bits (xor, shiftL, shiftR)
    import Control.Parallel
    import Data.Map (Map)
    import qualified Data.Map as Map

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input
        print . puzzle2 $ input

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

    prices :: Int -> [Int]
    prices = map (`mod` 10) . secrets

    changes :: [Int] -> [Int]
    changes [_] = []
    changes (a:b:xs) = b - a : changes (b:xs)

    buildMap :: [Int] -> Map (Int, Int, Int, Int) Int
    buildMap [] = Map.empty
    buildMap (x:xs) = par p1 $ pseq p2 $ Map.unionWith (+) p1 p2
        where
            p1 = buildMonkey x
            p2 = buildMap xs

    buildMonkey :: Int -> Map (Int, Int, Int, Int) Int
    buildMonkey x = buildMonkeyAcc 1 ch Map.empty
        where
            pr = take 2001 $ prices x
            ch = changes pr

            buildMonkeyAcc :: Int -> [Int] -> Map (Int, Int, Int, Int) Int -> Map (Int, Int, Int, Int) Int
            buildMonkeyAcc i (a:b:c:d:xs) acc
                | (a, b, c, d) `Map.notMember` acc = buildMonkeyAcc (i + 1) (b:c:d:xs) (Map.insert (a, b, c, d) (pr !! (i + 3)) acc)
                | otherwise = buildMonkeyAcc (i + 1) (b:c:d:xs) acc
            buildMonkeyAcc _ _ acc = acc

    puzzle1 :: String -> Int
    puzzle1 = sum . map (\t -> secrets t !! 2000) . parse

    puzzle2 :: String -> Int
    puzzle2 = maximum . map snd . Map.toList . buildMap . parse
