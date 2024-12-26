module Main where

import qualified Data.List as List (filter)
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as Heap
import Data.Map (Map)
import qualified Data.Map as Map

type Position = (Int, Int)
type Orientation = (Int, Int)

tsum :: (Int, Int) -> (Int, Int) -> (Int, Int)
tsum (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

data Vertex = V Position Orientation
    deriving (Ord, Eq, Show)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . puzzle1 $ input
    print . puzzle2 $ input

width :: String -> Int
width = length . head . lines

height :: String -> Int
height = length . lines

orientations :: [Orientation]
orientations = [(-1, 0), (0, 1), (1, 0), (0, -1)]

parseInput :: String -> Array Position Char
parseInput s = Array.listArray ((0, 0), (height s - 1, width s - 1)) $ filter (/= '\n') s

dijkstra :: Vertex -> Array Position Char -> [(Vertex, Int)]
dijkstra start map = explore (Heap.singleton (0, start)) Set.empty map
    where
        explore :: MinPrioHeap Int Vertex -> Set Vertex -> Array Position Char -> [(Vertex, Int)]
        explore q seen map = if Heap.isEmpty q then []
            else let (Just ((d, v), q')) = Heap.view q in
                if v `Set.notMember` seen then
                    (v, d) : explore (exploreVertex d v q' seen map) (Set.insert v seen) map
                else
                    explore q' seen map

        exploreVertex :: Int -> Vertex -> MinPrioHeap Int Vertex -> Set Vertex -> Array Position Char -> MinPrioHeap Int Vertex
        exploreVertex dis (V pos ori) q seen map = let
            k = if map Array.! tsum pos ori /= '#' then Heap.insert (dis + 1, V (tsum pos ori) ori) q else q
            in if ori == (1, 0) || ori == (-1, 0) then
                Heap.insert (dis + 1000, V pos (0, 1)) $ Heap.insert (dis + 1000, V pos (0, -1)) k
            else
                Heap.insert (dis + 1000, V pos (1, 0)) $ Heap.insert (dis + 1000, V pos (-1, 0)) k

puzzle1 :: String -> Int
puzzle1 s = snd . head . dropWhile (\(V p _, _) -> p /= (1, width s - 2))
    $ dijkstra (V (height s - 2, 1) (0, 1)) (parseInput s)

puzzle2 :: String -> Int
puzzle2 s = Map.size $ Map.mapKeys (\(V p _) -> p) $ results s
    where
        results :: String -> Map Vertex Int
        results s = Map.filter (== puzzle1 s) $ Map.unionWith (+) (start s) (end s)

        start :: String -> Map Vertex Int
        start s = Map.fromList $ dijkstra (V (height s - 2, 1) (0, 1)) (parseInput s)

        end :: String -> Map Vertex Int
        end s = Map.mapKeys (\(V p (dy, dx)) -> V p (dy * (-1), dx * (-1)))
            . Map.fromList $ dijkstra (V (1, width s - 2) (1, 0)) (parseInput s)
