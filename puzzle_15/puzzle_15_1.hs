{-# LANGUAGE MultiWayIf #-}
module Main where
    import Data.List (elemIndex)
    import Data.Map (Map)
    import qualified Data.Map as Map

    type Position = (Int, Int)
    type Direction = (Int, Int)

    up = (0, -1) :: Direction
    down = (0, 1) :: Direction
    left = (-1, 0) :: Direction
    right = (1, 0) :: Direction

    sum :: (Int, Int) -> (Int, Int) -> (Int, Int)
    sum (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

    data Type = Barrier | Box

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input

    parseInputMap :: String -> Map Position Type
    parseInputMap = Map.unions . zipWith parseLine [0..] . takeWhile (/= "") . lines
        where
            parseLine :: Int -> String -> Map Position Type
            parseLine l = Map.unions . zipWith (\c x -> if
                | x == '#' -> Map.singleton (c, l) Barrier
                | x == 'O' -> Map.singleton (c, l) Box
                | otherwise -> Map.empty) [0..]

    botPos :: String -> Position
    botPos s = let
        (Just i) = elemIndex '@' $ filter (/='\n') s
        width = length . head . lines $ s 
        in (i `mod` width, i `div` width)
    
    moveable :: Position -> Direction -> Map Position Type -> Maybe Position
    moveable p d m = case Map.lookup p m of
        Nothing -> Just p
        (Just Barrier) -> Nothing
        (Just Box) -> moveable (Main.sum p d) d m

    move :: String -> Position -> Map Position Type -> Map Position Type
    move [] _ m = m
    move (x:xs) p m
        | x == '^' = if Map.member (jump up) m then 
            case moveable (jump up) up m of
                (Just np) -> move xs (jump up) $ newMap np up
                Nothing -> move (dropWhile (=='^') xs) p m
            else move xs (jump up) m
        | x == '>' = if Map.member (jump right) m then 
            case moveable (jump right) right m of
                (Just np) -> move xs (jump right) $ newMap np right
                Nothing -> move (dropWhile (=='>') xs) p m
            else move xs (jump right) m
        | x == 'v' = if Map.member (jump down) m then
            case moveable (jump down) down m of
                (Just np) -> move xs (jump down) $ newMap np down
                Nothing -> move (dropWhile (=='v') xs) p m
            else move xs (jump down) m
        | x == '<' = if Map.member (jump left) m then
            case moveable (jump left) left m of
                (Just np) -> move xs (jump left) $ newMap np left
                Nothing -> move (dropWhile (=='<') xs) p m
            else move xs (jump left) m
        where
            newMap :: Position -> Direction -> Map Position Type
            newMap p d = Map.insert p Box $ Map.delete (jump d) m

            jump :: Direction -> Position
            jump = Main.sum p

    gps :: Map Direction Type -> Int
    gps = Prelude.sum . map (\(x, y) -> 100 * y + x) . Map.keys . Map.filter (\a -> case a of
        Barrier -> False
        Box -> True)

    moves :: String -> String
    moves = concat . tail . dropWhile (/="") . lines

    puzzle1 :: String -> Int
    puzzle1 s = gps $ move (moves s) (botPos s) (parseInputMap s)