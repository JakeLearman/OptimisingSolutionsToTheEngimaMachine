> module Menu where

> import Enigma
> import Crib
> import Data.List
> import Data.Maybe
> import Data.Char

---------------
Menu Generation
---------------
A menu is a graph showing all the connections between the letters in both the crib and the encrypted Word

> type Vertex = Char
> type Edge = [Vertex]
> type Graph = [Edge]

> tuplesToList :: [(a,a)] -> [[a]]
> tuplesToList = map (\(x,y) -> [x,y])

> c2i :: Char -> Int
> c2i c = ord c

> s2i :: String -> [Int]
> s2i (x:xs) = c2i x : s2i xs

> i2c :: Int -> Char
> i2c i = chr i

> incChar :: Char -> Char
> incChar c =  alphabet !! ((fromJust(elemIndex c alphabet)) + 1)

> menu = tuplesToList (groupTuples' "KEINEBESONDERENEREIGNISSE" "UAENFVRLBZPWMEPMIHFSRJXFMJKWRAXQEZ")

> isAdjacent :: Vertex -> Vertex -> Graph -> Bool
> isAdjacent x y [] = False
> isAdjacent x y (z:zs) = if x == head z && y == last z then True else isAdjacent x y zs
