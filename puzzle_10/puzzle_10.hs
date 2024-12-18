module Main where
    import Data.Map (Map)
    import qualified Data.Map as Map

    import Data.List (nub)

    type Position = (Int, Int)

    main :: IO ()
    main = do 
        input <- readFile "input.txt"
        (print . puzzle1) input
        (print . puzzle2) input

    parseInput :: String -> Map Position Int
    parseInput s = (Map.unions . zipWith parseLine [0..]) (lines s)
        where
            parseLine :: Int -> String -> Map Position Int
            parseLine l = Map.unions . zipWith (\c x -> Map.singleton (c, l) (read [x])) [0..]

    startingPositions :: Map Position Int -> [Position]
    startingPositions = Map.keys . Map.filter (== 0)

    explore :: Map Position Int -> Position -> [Position]
    explore m (x, y)
        | a == 9 = [(x, y)]
        | otherwise = exploreFollower explore [] a (x + 1, y) m
            ++ exploreFollower explore [] a (x - 1, y) m
            ++ exploreFollower explore [] a (x, y - 1) m
            ++ exploreFollower explore [] a (x, y + 1) m
        where
            Just a = Map.lookup (x, y) m

    exploreRating :: Map Position Int -> Position -> Int
    exploreRating m (x, y)
        | a == 9 = 1
        | otherwise = exploreFollower exploreRating 0 a (x + 1, y) m
            + exploreFollower exploreRating 0 a (x - 1, y) m
            + exploreFollower exploreRating 0 a (x, y - 1) m
            + exploreFollower exploreRating 0 a (x, y + 1) m
        where
            Just a = Map.lookup (x, y) m

    exploreFollower :: (Map Position Int -> Position -> a) -> a -> Int -> Position -> Map Position Int -> a
    exploreFollower f zero a (x, y) m = let l = Map.lookup (x, y) m in case l of
        Nothing -> zero
        (Just b) 
            | a + 1 == b -> f m (x, y)
            | otherwise -> zero
    
    calculateTrailHeads :: Map Position Int -> [Position] -> Int
    calculateTrailHeads m = sum . map (length . nub . explore m)

    puzzle1 :: String -> Int
    puzzle1 s = let m = parseInput s in calculateTrailHeads m (startingPositions m) 

    puzzle2 :: String -> Int
    puzzle2 s = let m = parseInput s in (sum . map (exploreRating m) . startingPositions) m 
