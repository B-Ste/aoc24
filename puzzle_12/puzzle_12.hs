module Main where
    import Data.Map (Map)
    import qualified Data.Map as Map
    import Data.Set (Set)
    import qualified Data.Set as Set
    import Data.List (delete, sort, (\\))

    type Position = (Int, Int)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        (print . puzzle1) input
        (print . puzzle2) input

    parseInput :: String -> Map Char [Position]
    parseInput = Map.unionsWith (++) . zipWith parseLine [0..] . lines
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

    buildSides :: [Position] -> Int
    buildSides l = length l * (sum . map (\x -> outerEdges l x + innerEdges l x)) l
        where
            outerEdges :: [Position] -> Position -> Int
            outerEdges l (x, y) = checkOuterEdge (x - 1, y) (x, y - 1) l 
                + checkOuterEdge (x - 1, y) (x, y + 1) l
                + checkOuterEdge (x + 1, y) (x, y - 1) l
                + checkOuterEdge (x + 1, y) (x, y + 1) l

            checkOuterEdge :: Position -> Position -> [Position] -> Int
            checkOuterEdge p1 p2 l
                | p1 `notElem` l && p2 `notElem` l = 1
                | otherwise = 0

            innerEdges :: [Position] -> Position -> Int
            innerEdges l (x, y) = checkInnerEdge (x - 1, y - 1) (x - 1, y) (x, y - 1) l
                + checkInnerEdge (x + 1, y - 1) (x + 1, y) (x, y - 1) l
                + checkInnerEdge (x - 1, y + 1) (x - 1, y) (x, y + 1) l
                + checkInnerEdge (x + 1, y + 1) (x + 1, y) (x, y + 1) l

            checkInnerEdge :: Position -> Position -> Position -> [Position] -> Int
            checkInnerEdge d p1 p2 l
                | d `notElem` l && p1 `elem` l && p2 `elem` l = 1
                | otherwise = 0

    calculate :: ([Position] -> Int) -> String -> Int
    calculate f = sum . map f . concatMap (buildRegions . snd) . Map.toList . parseInput

    puzzle1 :: String -> Int
    puzzle1 = calculate buildFence

    puzzle2 :: String -> Int
    puzzle2 = calculate buildSides
