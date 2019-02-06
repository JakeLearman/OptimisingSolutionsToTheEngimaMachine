> module Menu where

> import Crib
> import Data.List
> import Data.Char
> import Data.Function
> import Data.Ord
> import Data.Graph

---------------
Menu Generation
---------------
A menu is a graph showing all the connections between the letters in both the crib and the encrypted Word


> menu :: [[Char]]
> menu = tuplesToList (groupTuples' "KEINEBESONDERENEREIGNISSE" "UAENFVRLBZPWMEPMIHFSRJXFMJKWRAXQEZ")

> tuplesToList :: [(a,a)] -> [[a]]
> tuplesToList = map (\(x,y) -> [x,y])

> listToTuple :: [a] -> (a,a)
> listToTuple  [x,y] = (x,y)

> c2i :: Char -> Int
> c2i c = ord c

> s2i :: String -> [Int]
> s2i [] = []
> s2i (x:xs) = c2i x : s2i xs

> i2c :: Int -> Char
> i2c i = chr i

> menuToInt :: [String] -> [[Int]]
> menuToInt [] = []
> menuToInt (m:ms) = s2i m : menuToInt ms

> menuToTuple :: [String] -> [(Char, Char)]
> menuToTuple [] = []
> menuToTuple (x:xs) = listToTuple x : menuToTuple xs

> tuplesToInt :: (Char, Char) -> (Int, Int)
> tuplesToInt t = (c2i(fst t), c2i(snd t))

> tuplesToInt' :: [(Char, Char)] -> [(Int, Int)]
> tuplesToInt' ts = map tuplesToInt ts

groupByVertex groups each pair into a list of each vertex and each letter that is linked to that vertex

> groupByVertex :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
> groupByVertex = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst) . sortBy (comparing fst)

-------------------------
----Graph Generation-----
-------------------------

Graph made using fgl types where
type Vertex = Int
type Edge = (Vertex, Vertex)

Bound are the bounds of the graph i.e. the highest and lowest value

> bound :: (Vertex, Vertex)
> bound = (65,90)

> menuToGraph :: [[Char]] -> [Edge]
> menuToGraph menu = tuplesToInt'(menuToTuple menu)

> makeGraph :: Bounds -> [[Char]] -> Graph
> makeGraph bound menu = buildG bound (menuToGraph menu)

> listVertices :: Bounds -> [[Char]] -> [Vertex]
> listVertices bound menu = vertices (makeGraph bound menu)

> graph = makeGraph bound menu

> reachableVertex :: Graph -> Vertex -> [Vertex]
> reachableVertex graph vertex = reachable graph vertex

> reachableVertex' :: Graph -> [Vertex] -> [[Vertex]]
> reachableVertex' graph [] = []
> reachableVertex' graph (v:vs) = reachableVertex graph v : reachableVertex' graph vs

