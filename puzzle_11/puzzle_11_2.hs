module Main where
    import Data.Map (Map)
    import qualified Data.Map as Map

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        (print . puzzle2) input

    parseInput :: String -> Map Int Int
    parseInput = Map.unions . map ((`Map.singleton` 1) . read) . words

    evaluate :: Map Int Int -> Map Int Int
    evaluate = Map.fromListWith (+) . evaluateList . Map.toList

    evaluateList :: [(Int, Int)] -> [(Int, Int)]
    evaluateList [] = []
    evaluateList ((a, ai):xs)
        | a == 0 = (1, ai) : evaluateList xs
        | let 
            as = show a
            l = length as
            ls = l `div` 2, even l = ((read . take ls) as, ai) : ((read . drop ls) as, ai) : evaluateList xs
        | otherwise = (a * 2024, ai) : evaluateList xs

    puzzle2 :: String -> Int
    puzzle2 s = Map.foldr (+) 0 (iterate evaluate (parseInput s) !! 75)
