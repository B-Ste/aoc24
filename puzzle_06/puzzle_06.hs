module Main where
    import Data.Set (Set)
    import qualified Data.Set as Set
    import Control.Parallel (par, pseq)

    data Dir = Up | Right | Down | Left

    type Position = (Int, Int)

    data Guard = G Dir Position
    instance Eq Guard where
        (==) :: Guard -> Guard -> Bool
        (G Main.Up p1)      == (G Main.Up p2)       = p1 == p2
        (G Main.Right p1)   == (G Main.Right p2)    = p1 == p2
        (G Main.Down p1)    == (G Main.Down p2)     = p1 == p2
        (G Main.Left p1)    == (G Main.Left p2)     = p1 == p2
        _                   == _                    = False

    type Movement = Guard

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input
        print . puzzle2 $ input

    obsMap :: String -> [Position]
    obsMap = obsMapAcc (0, 0) . lines
        where
            obsMapAcc :: Position -> [String] -> [Position]
            obsMapAcc (_, _) [] = []
            obsMapAcc (x, y) ([]:ks) = obsMapAcc (0, y + 1) ks
            obsMapAcc (x, y) ((i:is):ks)
                | i == '#' = (x, y) : obsMapAcc (x + 1, y) (is:ks)
                | otherwise = obsMapAcc (x + 1, y) (is:ks)

    guardPos :: String -> Guard
    guardPos s = guardPosAcc (lines s) (0, 0)
        where
            guardPosAcc :: [String] -> Position -> Guard
            guardPosAcc [] acc = error "failed to find guard"
            guardPosAcc ([]:ks) (x, y) = guardPosAcc ks (0, y + 1)
            guardPosAcc ((i:is):ks) (x, y)
                | i == '^' = G Main.Up (x, y)
                | i == '>' = G Main.Right (x, y)
                | i == 'v' = G Main.Down (x, y)
                | i == '<' = G Main.Left (x, y)
                | otherwise = guardPosAcc (is:ks) (x + 1, y)

    moveGuard :: Guard -> [Position] -> Guard
    moveGuard (G Main.Up (x, y)) obs
        | (x, y - 1) `elem` obs = G Main.Right (x, y)
        | otherwise = G Main.Up (x, y - 1)
    moveGuard (G Main.Right (x, y)) obs
        | (x + 1, y) `elem` obs = G Main.Down (x, y)
        | otherwise = G Main.Right (x + 1, y)
    moveGuard (G Main.Down (x, y)) obs
        | (x, y + 1) `elem` obs = G Main.Left (x, y)
        | otherwise = G Main.Down (x, y + 1)
    moveGuard (G Main.Left (x, y)) obs
        | (x - 1, y) `elem` obs = G Main.Up (x, y)
        | otherwise = G Main.Left (x - 1, y)

    fieldLength :: String -> Int
    fieldLength = length . head . lines

    fieldHeight :: String -> Int
    fieldHeight = length . lines

    fieldBorders :: String -> Position
    fieldBorders s = (fieldLength s, fieldHeight s)

    puzzle1 :: String -> Int
    puzzle1 input = Set.size (calculate (guardPos input) (obsMap input) (fieldBorders input) Set.empty)
        where
            calculate :: Guard -> [Position] -> Position -> Set Position -> Set Position
            calculate g obs bor s
                | let
                    G d (x, y) = g
                    (bx, by) = bor,
                    x >= 0 && y >= 0 && x < bx && y < by = calculate (moveGuard g obs) obs bor (Set.insert (x, y) s)
                | otherwise = s

    puzzle2 :: String -> Int
    puzzle2 input = calculate (guardPos input) (obsMap input) (fieldBorders input) 0
        where
            calculate :: Guard -> [Position] -> Position -> Int -> Int
            calculate _ _ (_, by) y | by == y = 0
            calculate g obs bor y = par p1 (pseq p2 (p1 + p2))
                where
                    p1 = calculateLine g obs bor 0 y 0
                    p2 = calculate g obs bor (y+1)

            calculateLine :: Guard -> [Position] -> Position -> Int -> Int -> Int -> Int
            calculateLine _ _ (bx, _) x y acc | bx == x = acc
            calculateLine g obs bor x y acc = calculateLine g obs bor (x + 1) y (acc + loopCheck g ((x, y):obs) bor)

            loopCheck :: Guard -> [Position] -> Position -> Int
            loopCheck g obs bor = loopCheckAcc g obs bor []
                where
                    loopCheckAcc :: Guard -> [Position] -> Position -> [Movement] -> Int
                    loopCheckAcc g obs bor mov
                        | g `elem` mov = 1
                        | let
                            G d (x, y) = g
                            (bx, by) = bor,
                            x < 0 || x >= bx || y < 0 || y >= by = 0
                        | otherwise = loopCheckAcc (moveGuard g obs) obs bor (g:mov)
