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

> menuToTuple :: [String] -> [(Char, Char)]
> menuToTuple [] = []
> menuToTuple (x:xs) = listToTuple x : menuToTuple xs


groupByVertex groups each pair into a list of each vertex and each letter that is linked to that vertex

> groupByVertex :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
> groupByVertex = map (\l -> (fst . head $ l, map snd l)) . groupBy ((==) `on` fst) . sortBy (comparing fst)

-------------------------
----Graph Generation-----
-------------------------

> type Menu = [Int]

> findMenu :: [(Char, Char)] -> [Menu]
> findMenu crib = linkElements(findLink crib) [] []

> linkElements :: [Menu] -> [Menu] -> [Menu] -> [Menu]
> linkElements [] passiive closed = passiive
> linkElements (m:ms) passiive closed 
>   | not (null ms') = linkElements (ms ++ ms') passiive (m:closed)
>   | otherwise = linkElements ms (m:passiive) closed
>   where
>   ms' = [lm | lm <- lms, filterDuplicates lm]
>   lms = [m ++ om | (_:om) <- filteredMenu]
>   filteredMenu = [mm | mm @ (mm1:_) <- (ms ++ passiive), mm1 == last m]

> filterDuplicates ::Eq a => [a] -> Bool
> filterDuplicates [] = True
> filterDuplicates [_] = True
> filterDuplicates (m:ms)
>   | (elem m ms) = False
>   | otherwise = filterDuplicates ms

> findLink :: [(Char, Char)] -> [Menu]
> findLink crib = [[xs, ys] | (xs, x) <- a, (ys, y) <- b, x == y]
>   where
>   (as, bs) = unzip crib
>   a = zip indice as
>   b = zip indice bs
>   indice = [0 .. ((length crib) - 1)]

> findMaxCycle :: [(Char, Char)] -> Menu
> findMaxCycle crib = maximumBy (\m1 m2 ->(compare (length m1) (length m2))) (findMenu crib)
 
> crib = zip  "AABBCC" "CCBBAA"
> test = findMaxCycle crib