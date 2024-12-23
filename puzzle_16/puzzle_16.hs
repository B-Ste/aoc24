{-
The Plan:
- Transform the maze into a graph where we can run Dijkstra's Algorithm
- A node in the maze is any point where the reindeer has more options than only moving forward
    or cannot move forward any more
- A node at (y, x) is constructed as four nodes in the Graph: (y, x, North), (y, yx, South), (y, x, East) and (y, x, West)
- (y, x, North) contains, if present, the edge travling north; (y, x, South) contains, if present, the edge traveling south; etc.
- They are connected as follows:

                    (y, x, North)
                    /           \
                1000            1000
                /                  \
        (y, x, West)            (y, x, East)
                \                  /
                1000           1000
                    \           /
                    (y, x, South)

- An Edge coming from the South will enter (y, x, North) (as it is traveling north)
- An Edge coming from the North will enter (y, x, South)
- An Edge coming from the West will enter (y, x, East)
- An Edge coming from the East will enter (y, x, West)
-}
module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Data.List (sort, delete)

type Position = (Int, Int)

type Direction = (Int, Int)

north = (-1, 0)

south = (1, 0)

west = (0, -1)

east = (0, 1)

data Orientation = North | South | West | East
    deriving (Eq, Ord, Show)

data Vertex = V Position Orientation
    deriving (Eq, Ord, Show)

data DVertex = DV Vertex Int
    deriving (Eq, Show)

instance Ord DVertex where
    (<=) :: DVertex -> DVertex -> Bool
    (<=) (DV _ a) (DV _ b) = a <= b


tsum :: (Int, Int) -> (Int, Int) -> (Int, Int)
tsum (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . puzzle1 $ input

-- use Set for O(log n) lookup later when parsing edges
parseNodes :: String -> Set Position
parseNodes = nodesLinewise 1 . lines
    where
        nodesLinewise :: Int -> [String] -> Set Position
        nodesLinewise i (x : y : z : as) = Set.union (nodesOnLine i 1 x y z Set.empty) (nodesLinewise (i + 1) (y : z : as))
        nodesLinewise _ _ = Set.empty

        nodesOnLine :: Int -> Int -> String -> String -> String -> Set Position -> Set Position
        nodesOnLine l c x y z s
            | c == length x - 1 = s
            | isNode c x y z = nodesOnLine l (c + 1) x y z (Set.insert (l, c) s)
            | otherwise = nodesOnLine l (c + 1) x y z s

        isNode :: Int -> String -> String -> String -> Bool
        isNode c x y z
            | center == '#' = False
            | center == '.' && west == '.' && east == '.' && north == '#' && south == '#' = False
            | center == '.' && west == '.' && (east == 'S' || east == 'E') && north == '#' && south == '#' = False
            | center == '.' && (west == 'S' || west == 'E') && east == '.' && north == '#' && south == '#' = False
            | center == '.' && west == '#' && east == '#' && north == '.' && south == '.' = False
            | center == '.' && west == '#' && east == '#' && (north == 'S' || north == 'E') && south == '.' = False
            | center == '.' && west == '#' && east == '#' && north == '.' && (south == 'S' || south == 'E') = False
            | otherwise = True
            where
                center = y !! c
                north = x !! c
                south = z !! c
                east = y !! (c + 1)
                west = y !! (c - 1)

{-
Use the maze and the positions of nodes to parse the graph data-structure.
-}
parseEdges :: String -> Set Position -> Map Vertex (Map Vertex Int)
parseEdges s n = Map.unions . map (createVertex s n) . Set.toList $ n
    where
        createVertex :: String -> Set Position -> Position -> Map Vertex (Map Vertex Int)
        createVertex s n p = Map.insert (V p North) (northVertex s p n)
            $ Map.insert (V p South) (southVertex s p n)
            $ Map.insert (V p West) (westVertex s p n)
            $ Map.singleton (V p East) (eastVertex s p n)

        northVertex :: String -> Position -> Set Position -> Map Vertex Int
        northVertex s p n = Map.insert (V p East) 1000 $ Map.insert (V p West) 1000 $ exploreVertex (V p North) (tsum p north) s n north

        southVertex :: String -> Position -> Set Position -> Map Vertex Int
        southVertex s p n = Map.insert (V p East) 1000 $ Map.insert (V p West) 1000 $ exploreVertex (V p South) (tsum p south) s n south

        westVertex :: String -> Position -> Set Position -> Map Vertex Int
        westVertex s p n = Map.insert (V p North) 1000 $ Map.insert (V p South) 1000 $ exploreVertex (V p West) (tsum p west) s n west

        eastVertex :: String -> Position -> Set Position -> Map Vertex Int
        eastVertex s p n = Map.insert (V p North) 1000 $ Map.insert (V p South) 1000 $ exploreVertex (V p East) (tsum p east) s n east

        exploreVertex :: Vertex -> Position -> String -> Set Position -> Direction -> Map Vertex Int
        exploreVertex v p s n d
            | charAt s p == '#' = Map.empty
            | let
                (V p' o) = v
                (py', px') = p'
                (py, px) = p,
                Set.member p n =
                Map.singleton (V p o) (max (abs (py - py')) (abs (px - px')))
            | otherwise = exploreVertex v (tsum p d) s n d

        charAt :: String -> Position -> Char
        charAt s (y, x) = (lines s !! y) !! x

dijkstra :: Vertex -> Map Vertex (Map Vertex Int) -> [DVertex]
dijkstra v m = exploreGraph m [DV v 0] []
    where
        exploreGraph :: Map Vertex (Map Vertex Int) -> [DVertex] -> [Vertex] -> [DVertex]
        exploreGraph _ [] _ = []
        exploreGraph m l exp = let
            min = minimum l
            f = delete min l
            DV minp mina = min
            (Just k) = Map.lookup minp m
            r = explore mina k
            s = filter (\(DV p _) -> p `notElem` exp) (update r f)
            in DV minp mina : exploreGraph m s (minp:exp)

        -- this makes the implementation slow
        update :: [DVertex] -> [DVertex] -> [DVertex]
        update n m = map (\(p, i) -> DV p i) . Map.toList $ Map.union ((Map.fromList . map (\(DV p i) -> (p, i))) n) ((Map.fromList . map (\(DV p i) -> (p, i))) m)

        explore :: Int -> Map Vertex Int -> [DVertex]
        explore i = map (\(p, a) -> DV p (a + i)) . Map.toList

start :: String -> Position
start s = ((length . lines $ s) - 2, 1)

end :: String -> Position
end s = (1, (length . head . lines $ s) - 2)

endpoints :: String -> [DVertex] -> [DVertex]
endpoints s = filter (\(DV (V p _) _) -> p == end s)

puzzle1 :: String -> Int
puzzle1 s = minimum . map (\(DV _ a) -> a) . endpoints s $ dijkstra (V (start s) East) (parseEdges s (parseNodes s))
