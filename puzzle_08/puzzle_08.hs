module Main where
    import Data.Map (Map)
    import qualified Data.Map as Map

    import Data.Set (Set)
    import qualified Data.Set as Set
    
    type Position = (Int, Int)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        (print . puzzle1) input
        (print . puzzle2) input

    parseInput :: String -> Map Char [Position]
    parseInput s = parseInputAcc (lines s) (0, 0) Map.empty
        where
            parseInputAcc :: [String] -> Position -> Map Char [Position] -> Map Char [Position]
            parseInputAcc [] _ acc = acc
            parseInputAcc ([]:rs) (x, y) acc = parseInputAcc rs (0, y + 1) acc
            parseInputAcc ((a:as):rs) (x, y) acc
                | a == '.' = parseInputAcc (as:rs) (x + 1, y) acc
                | otherwise = parseInputAcc (as:rs) (x + 1, y) (Map.insertWith (++) a [(x, y)] acc)

    -- takes list of antennas and returns list of antinodes (for part 1)
    antinodes :: [Position] -> [Position]
    antinodes [a] = []
    antinodes (a:as) = singleAntinodes a as ++ antinodes as
        where
            singleAntinodes :: Position -> [Position] -> [Position]
            singleAntinodes _ [] = []
            singleAntinodes (ax, ay) ((bx, by):bs) =
                (2 * ax - bx, 2 * ay - by):(2 * bx - ax, 2 * by - ay):singleAntinodes (ax, ay) bs

    -- takes size of field and list of antennas and returns list of antinodes (for part 2)
    antinodesExt :: Position -> [Position] -> [Position]
    antinodesExt bor [a] = []
    antinodesExt bor (a:as) = singleAntinodes bor a as ++ antinodesExt bor as
        where
            singleAntinodes :: Position -> Position -> [Position] -> [Position]
            singleAntinodes _ _ [] = []
            singleAntinodes bor (ax, ay) ((bx, by):bs) = 
                let dx = bx - ax
                    dy = by - ay
                    d = gcd dx dy in line bor (dx `div` d, dy `div` d) (ax, ay) ++ singleAntinodes bor (ax, ay) bs
            
            line :: Position -> Position -> Position -> [Position]
            line bor (dx, dy) a = lineExpl bor (-dx, -dy) a ++ lineExpl bor (dx, dy) a
                where
                    lineExpl :: Position -> Position -> Position -> [Position]
                    lineExpl (bx, by) (dx, dy) (x, y) 
                        | x >= 0 && x < bx && y >= 0 && y < by = (x, y):lineExpl (bx, by) (dx, dy) (x + dx, y + dy)
                        | otherwise = []

    inField :: Position -> Position -> Bool
    inField (bx, by) (x, y) = x >= 0 && x < bx && y >= 0 && y < by

    mapDimentions :: String -> Position
    mapDimentions s = (mapLength s, mapHeight s)
        where
            mapLength :: String -> Int
            mapLength = length . head . lines

            mapHeight :: String -> Int
            mapHeight = length . lines

    calculate :: ([Position] -> [Position]) -> Map Char [Position] -> Int
    calculate f = Set.size . Set.fromList . concatMap f . Map.elems

    puzzle1 :: String -> Int
    puzzle1 s = calculate (filter (inField (mapDimentions s)) . antinodes) (parseInput s)

    puzzle2 :: String -> Int
    puzzle2 s = calculate (antinodesExt (mapDimentions s)) (parseInput s)
    