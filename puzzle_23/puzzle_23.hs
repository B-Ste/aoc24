module Main where
    import Data.Map (Map)
    import qualified Data.Map as Map
    import Data.Set (Set)
    import qualified Data.Set as Set
    import Data.List (nub, sort, maximumBy)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input
        print . puzzle2 $ input

    parseGraph :: String -> Map String (Set String)
    parseGraph = foldl f Map.empty . lines
        where
            f :: Map String (Set String) -> String -> Map String (Set String)
            f m (a:b:_:c:d:_) =
                Map.insertWith Set.union [a, b] (Set.singleton [c, d])
                $ Map.insertWith Set.union [c, d] (Set.singleton [a, b]) m

    parseNodes :: (String -> Bool) -> String -> [String]
    parseNodes f = nub . filter f . words . map (\t -> case t of
        '-' -> ' '
        '\n' -> ' '
        t -> t)

    threeLoops :: Map String (Set String) -> String -> [[String]]
    threeLoops m s = let
            s1l = let (Just s1) = Map.lookup s m in Set.toList s1
            s2l = map (\ss -> let (Just ssl) = Map.lookup ss m in Set.toList ssl) s1l
        in concatMap (filter (not . null)) $ zipWith (\x1 x2 -> map (\x2m ->
            let (Just p) = Map.lookup x2m m
            in if Set.member s p then sort [s, x1, x2m] else []) x2) s1l s2l

    clique :: Map String (Set String) -> String -> [String] -> [String]
    clique m st = foldl (\a ss -> if cliqueMember a ss then ss:a else a) [st]
        where
            cliqueMember :: [String] -> String -> Bool
            cliqueMember c s = let (Just sk) = Map.lookup s m
                in Set.isSubsetOf (Set.fromList c) sk

    cliques :: Map String (Set String) -> [String] -> [[String]]
    cliques m n = map (\s -> clique m s n) n

    puzzle1 :: String -> Int
    puzzle1 s = let
        g = parseGraph s
        n = parseNodes (\(x:xs) -> x == 't') s
        in length . nub $ concatMap (threeLoops g) n

    puzzle2 :: String -> String
    puzzle2 s = let
        g = parseGraph s
        n = parseNodes (const True) s
        in init . concatMap (\a -> a ++ [',']) . sort . maximumBy (\s1 s2 ->
            if length s1 > length s2
                then GT
            else if length s1 == length s2
                then EQ
            else LT) $ cliques g n
