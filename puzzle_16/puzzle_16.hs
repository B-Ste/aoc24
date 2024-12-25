module Main where

import qualified Data.List as List (map, filter, foldl)
import Data.Array (Array)
import qualified Data.Array as Array
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Heap (MinPrioHeap)
import qualified Data.Heap as Heap

type Position = (Int, Int)
type Orientation = (Int, Int)

tsum :: (Int, Int) -> (Int, Int) -> (Int, Int)
tsum (a1, b1) (a2, b2) = (a1 + a2, b1 + b2)

data Vertex = V Position Orientation
    deriving (Ord, Eq)

main :: IO ()
main = do
    input <- readFile "input.txt"
    print . puzzle1 $ input

width :: String -> Int
width = length . head . lines

height :: String -> Int
height = length . lines

parseInput :: String -> Array Position Char
parseInput s = Array.listArray ((0, 0), (height s - 1, width s - 1)) $ filter (/= '\n') s

dijkstra :: Vertex -> Position -> Array Position Char -> Int
dijkstra start end map = (\(V p _, d) -> d) (head . dropWhile (\(V p _, _) -> p /= end)
    $ explore (Heap.singleton (0, start)) Set.empty map)
    where
        explore :: MinPrioHeap Int Vertex -> Set Vertex -> Array Position Char -> [(Vertex, Int)]
        explore q seen map = if Heap.isEmpty q then []
            else let (Just ((d, v), q')) = Heap.view q in
                if v `Set.notMember` seen then
                    (v, d) : explore (exploreVertex d v q' seen map) (Set.insert v seen) map
                else
                    explore q' seen map

        exploreVertex :: Int -> Vertex -> MinPrioHeap Int Vertex -> Set Vertex -> Array Position Char -> MinPrioHeap Int Vertex
        exploreVertex dis (V pos ori) q seen map = List.foldl insertToQ q newNeighbors
            where
                insertToQ :: MinPrioHeap Int Vertex -> Vertex -> MinPrioHeap Int Vertex
                insertToQ qu (V p d) =
                    let c = map Array.! p in
                        if c /= '#' then
                            if sameOri (V pos ori) (V p d) then
                                Heap.insert (dis + 1, V p d) qu
                            else
                                Heap.insert (dis + 1001, V p d) qu
                        else
                            qu

                sameOri :: Vertex -> Vertex -> Bool
                sameOri (V _ o1) (V _ o2) = o1 == o2

                newNeighbors :: [Vertex]
                newNeighbors = List.filter (`Set.notMember` seen) neighbors

                neighbors :: [Vertex]
                neighbors = List.map (\x -> V (tsum pos x) x) [(-1, 0), (0, 1), (1, 0), (0, -1)]

puzzle1 :: String -> Int
puzzle1 s = dijkstra (V (height s - 2, 1) (0, 1)) (1, width s - 2) (parseInput s)
