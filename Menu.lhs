> module Menu where

> import Enigma
> import Crib

---------------
Menu Generation
---------------
A menu is a graph showing all the connections between the letters in both the crib and the encrypted Word

> type Vertex = Char
> type Edge = (Vertex, Vertex)
> type Graph = [Edge]

> adjacent :: Vertex -> Vertex -> Graph -> Bool
> adjacent x y [] = False
> adjacent x y (z:zs) = if x == fst z && y == snd z then True else adjacent x y zs

> testVertexList = groupByVertex (groupTuples' "KEINEBESONDERENEREIGNISSE" "UAENFVRLBZPWMEPMIHFSRJXFMJKWRAXQEZ")