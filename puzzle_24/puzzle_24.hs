module Main where
    import Data.Map (Map)
    import qualified Data.Map as Map
    import Data.List (sort)

    main :: IO ()
    main = do
        input <- readFile "input.txt"
        print . puzzle1 $ input

    parseState :: [String] -> Map String Bool
    parseState = foldl f Map.empty
        where
            f :: Map String Bool -> String -> Map String Bool
            f m (a:b:c:_:_:x:xs) = Map.insert [a, b, c] (toEnum $ read [x]) m

    processState :: Map String Bool -> [String] -> Map String Bool
    processState m [] = m
    processState m ((x1:x2:x3:_:'A':'N':'D':_:y1:y2:y3:_:_:_:_:z1:z2:z3:a):xs) =
            if Map.member [x1, x2, x3] m && Map.member [y1, y2, y3] m
                then processState (fp (&&) m [x1, x2, x3] [y1, y2, y3] [z1, z2, z3]) xs
            else processState m (xs ++ [x1:x2:x3:' ':'A':'N':'D':' ':y1:y2:y3:' ':' ':' ':' ':z1:z2:z3:a])
    processState m ((x1:x2:x3:_:'X':'O':'R':_:y1:y2:y3:_:_:_:_:z1:z2:z3:a):xs) =
            if Map.member [x1, x2, x3] m && Map.member [y1, y2, y3] m
                then processState (fp (/=) m [x1, x2, x3] [y1, y2, y3] [z1, z2, z3]) xs
            else processState m (xs ++ [x1:x2:x3:' ':'X':'O':'R':' ':y1:y2:y3:' ':' ':' ':' ':z1:z2:z3:a])
    processState m ((x1:x2:x3:_:'O':'R':_:y1:y2:y3:_:_:_:_:z1:z2:z3:a):xs) =
            if Map.member [x1, x2, x3] m && Map.member [y1, y2, y3] m
                then processState (fp (||) m [x1, x2, x3] [y1, y2, y3] [z1, z2, z3]) xs
            else processState m (xs ++ [x1:x2:x3:' ':'O':'R':' ':y1:y2:y3:' ':' ':' ':' ':z1:z2:z3:a])

    fp :: (Bool -> Bool -> Bool) -> Map String Bool -> String -> String -> String -> Map String Bool
    fp f m x y z = let Just xv = Map.lookup x m; Just yv = Map.lookup y m in Map.insert z (f xv yv) m

    collectResult :: Map String Bool -> Int
    collectResult = foldr ((\i a -> a * 2 + i) . fromEnum . snd) 0 . sort . Map.toList . Map.filterWithKey (\(a:as) _ -> a == 'z')

    puzzle1 :: String -> Int
    puzzle1 s = let (a, b) = span (/= "") (lines s) in collectResult $ processState (parseState a) (tail b)
    