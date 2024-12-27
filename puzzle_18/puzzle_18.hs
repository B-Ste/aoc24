module Main where
    import Data.Set (Set)
    import qualified Data.Set as Set
    
    type Position = (Int, Int)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input

    parseInput :: Int -> String -> Set Position
    parseInput i = Set.fromList . take i . map parseCoordinate . lines
        where
            parseCoordinate :: String -> Position
            parseCoordinate s = let (f, k) = break (== ',') s in
                (read f, read . tail $ k)

    bfs :: Position -> Set Position -> [(Position, Int)]
    bfs p bar = bfsAcc [(p, 0)] Set.empty
        where
            bfsAcc :: [(Position, Int)] -> Set Position -> [(Position, Int)]
            bfsAcc [] _  = []
            bfsAcc (x : xs) seen = let 
                ((py, px), d) = x
                n = [(pn, d + 1) 
                    | pn <- [(py + 1, px), (py - 1, px), (py, px + 1), (py, px - 1)], 
                    inBounds pn && pn `Set.notMember` bar && pn `Set.notMember` seen]
                in x : bfsAcc (xs ++ n) (Set.union seen $ Set.fromList . map fst $ n)
                where
                    inBounds :: Position -> Bool
                    inBounds (y, x) = y >= 0 && y <= 70 && x >= 0 && x <= 70

    puzzle1 :: String -> Int
    puzzle1 = snd . head . dropWhile (\(p, _) -> p /= (70, 70)) . bfs (0, 0) . parseInput 1024
    