> module Menu where

> import Enigma
> import Crib

---------------
Menu Generation
---------------
A menu is a graph showing all the connections between the letters in both the crib and the encrypted Word

> type Node = Char
> type Graph = [(Node, [Node])]