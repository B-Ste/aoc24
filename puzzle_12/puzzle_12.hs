module Main where
    import Data.Map (Map)
    import qualified Data.Map as Map
    import Data.Set (Set)
    import qualified Data.Set as Set
    import Data.List (delete, sort, nub, (\\))

    -- area, fences
    type Region = (Int, Int)
    type Position = (Int, Int)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        (print . puzzle1) input

    parseInput :: String -> Map Char [Position]
    parseInput s = (Map.unionsWith (++) . zipWith parseLine [0..]) (lines s)
        where
            parseLine :: Int -> String -> Map Char [Position]
            parseLine l = Map.unionsWith (++) . zipWith (\c x -> Map.singleton x [(c, l)]) [0..]

    buildRegions :: [Position] -> [[Position]]
    buildRegions l
        | null d = [k]
        | otherwise = k : buildRegions d
        where
            (x:xs) = l
            k = explore [x] l Set.empty
            d = l \\ k

            -- DFS
            explore :: [Position] -> [Position] -> Set Position -> [Position]
            explore [] _ _ = []
            explore ((x, y):xs) l s
                | (x + 1, y) `elem` l && Set.notMember (x + 1, y) s = explore ((x + 1, y):(x, y):xs) l (Set.insert (x, y) s)
                | (x, y + 1) `elem` l && Set.notMember (x, y + 1) s = explore ((x, y + 1):(x, y):xs) l (Set.insert (x, y) s)
                | (x - 1, y) `elem` l && Set.notMember (x - 1, y) s = explore ((x - 1, y):(x, y):xs) l (Set.insert (x, y) s)
                | (x, y - 1) `elem` l && Set.notMember (x, y - 1) s = explore ((x, y - 1):(x, y):xs) l (Set.insert (x, y) s)
                | otherwise = (x, y) : explore xs (delete (x, y) l) (Set.insert (x, y) s)

    buildFence :: [Position] -> Int
    buildFence p = length p * buildFenceAcc 0 (sort p)
        where
            buildFenceAcc :: Int -> [Position] -> Int
            buildFenceAcc i p
                | i == length p = 0
                | (x - 1, y) `elem` p && (x, y - 1) `elem` p = buildFenceAcc (i + 1) p
                | (x - 1, y) `elem` p || (x, y - 1) `elem` p = 2 + buildFenceAcc (i + 1) p
                | otherwise = 4 + buildFenceAcc (i + 1) p
                where
                    (x, y) = p !! i

    puzzle1 :: String -> Int
    puzzle1 = sum . map buildFence . concatMap (buildRegions . snd) . Map.toList . parseInput
