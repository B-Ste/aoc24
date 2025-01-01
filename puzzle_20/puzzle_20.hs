module Main where
    import Data.Array (Array)
    import qualified Data.Array as Array
    import Data.Map (Map)
    import qualified Data.Map as Map
    import Data.Maybe (mapMaybe)

    type Position = (Int, Int)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input
        print . puzzle2 $ input

    maxInd :: String -> (Int, Int)
    maxInd s = ((length . lines $ s) - 1, (length . head . lines $ s) - 1)

    parseMap :: String -> Array Position Char
    parseMap s = Array.listArray ((0, 0), maxInd s) $ concat . lines $ s

    start :: Array Position Char -> Position
    start = fst. head . filter (\(i, e) -> e == 'S') . Array.assocs

    traversable :: Char -> Bool
    traversable c = c == '.' || c == 'S' || c == 'E'

    parseRace :: Position -> Array Position Char -> Map Position Int
    parseRace p a = parseRaceAcc (-1, -1) p $ Map.singleton (-1, -1) (-1)
        where
            parseRaceAcc :: Position -> Position -> Map Position Int -> Map Position Int
            parseRaceAcc prev curr acc =
                let (cy, cx) = curr
                    next    | traversable (a Array.! (cy + 1, cx)) && (cy + 1, cx) /= prev = (cy + 1, cx)
                            | traversable (a Array.! (cy - 1, cx)) && (cy - 1, cx) /= prev = (cy - 1, cx)
                            | traversable (a Array.! (cy, cx + 1)) && (cy, cx + 1) /= prev = (cy, cx + 1)
                            | traversable (a Array.! (cy, cx - 1)) && (cy, cx - 1) /= prev = (cy, cx - 1)
                            | otherwise = undefined
                    (Just pk) = Map.lookup prev acc
                in
                    if a Array.! curr == 'E' then Map.insert curr (pk + 1) acc
                    else parseRaceAcc curr next $ Map.insert curr (pk + 1) acc

    parseCheats :: Array Position Char -> Map Position Int -> Map Position Int
    parseCheats a race = Map.fromList . mapMaybe parseCheat . Array.assocs $ a
        where
            parseCheat :: (Position, Char) -> Maybe (Position, Int)
            parseCheat (p, c)
                | c /= '#' = Nothing
                | otherwise = case cheatable p of
                    Nothing -> Nothing
                    (Just i) -> Just (p, i)
            
            cheatable :: Position -> Maybe Int
            cheatable (py, px)
                | let (_ , (by, bx)) = Array.bounds a, py == 0 || px == 0 || py == by || px == bx = Nothing
                | traversable (a Array.! ym) && traversable (a Array.! yp) = Just $ cheatSave ym yp
                | traversable (a Array.! xm) && traversable (a Array.! xp) = Just $ cheatSave xm xp
                | otherwise = Nothing
                where
                    ym = (py - 1, px)
                    yp = (py + 1, px)
                    xm = (py, px - 1)
                    xp = (py, px + 1) 

            cheatSave :: Position -> Position -> Int
            cheatSave p1 p2 = let 
                Just pk1 = Map.lookup p1 race
                Just pk2 = Map.lookup p2 race
                in abs (pk1 - pk2) - 2

    parseAllCheats :: Array Position Char -> Map Position Int -> [Int]
    parseAllCheats a race = concatMap (parseParticularCheats . fst) . filter (traversable . snd) . Array.assocs $ a
        where
            parseParticularCheats :: Position -> [Int]
            parseParticularCheats s = mapMaybe (parseCheat s) $ allCheats s

            parseCheat :: Position -> Position -> Maybe Int
            parseCheat start end 
                | traversable $ a Array.! end = let 
                    Just pk1 = Map.lookup start race
                    Just pk2 = Map.lookup end race
                in Just (pk2 - pk1 - distance start end)
                | otherwise = Nothing

            allCheats :: Position -> [Position]
            allCheats p = [(y, x) | y <- [0..140], x <- [0..140], distance p (y, x) <= 20]

            distance :: Position -> Position -> Int
            distance (py, px) (y, x) = abs(py - y) + abs(px - x)

    puzzle1 :: String -> Int
    puzzle1 s = let 
        a = parseMap s 
        race = parseRace (start a) a
        in Map.size . Map.filter (>= 100) $ parseCheats a race

    puzzle2 :: String -> Int
    puzzle2 s = let
        a = parseMap s
        race = parseRace (start a) a
        in length . filter (>= 100) $ parseAllCheats a race
        