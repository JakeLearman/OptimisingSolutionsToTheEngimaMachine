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
----Menu Generation-----
-------------------------

A menu is a type coined by Alan Turing used to describe a relationship between letters in the crib and the cipher text. In this case
these letters are converted to integers to allow for easier mathematical computation.

> type Menu = [Int]

findMenu will take a tuple of the crib and the encrypted message and return a list of menus

> findMenu :: [(Char, Char)] -> [Menu]
> findMenu crib = growMenu (findLink crib)

No duplicates will check to see if there is a duplicate element in the list and return a boolean
if one exists therefore proving a cycle exists in the menu.

> boolDups :: Eq a => [a] -> Bool
> boolDups [] = True
> boolDups [_] = True
> boolDups (h : t)
>  |(elem h t) = False
>  |otherwise = boolDups t

findLink finds a list of menus based upon a crib and cipher text. It begins by splitting the tuple
into segments and creating sublists of based upon the relationship between the elements within

> findLink :: [(Char, Char)] -> [Menu]
> findLink crib = [[xs, ys] |(ys, y) <- m,(xs,x) <- c, y ==x] 
>  where 
>   cycle = [0 .. ((length crib) - 1)]
>   (ms, cs) = unzip crib
>   m = zip cycle ms
>   c = zip cycle cs

 growMenu recursively expands the menu by linking all menus with common elements together until no more
 joins can be made. Initlally the null case will return the list of menus if no more joins can be done. 
 Otherwise the function recurses to do more joins where through list comprehension menus are found with 
 overlaps. Furthermore all duplicate menus are removed as well as circular menus and submenus.

> growMenu :: [Menu] -> [Menu] 
> growMenu ms
>  |null joins = ms 
>  |otherwise = growMenu noDupsMs 
>  where
>   joins = [(joinMenu x y) | x <-ms, y <- ms, x /= y, (findOverlaps x y) > 0] 
>   filteredJoins = [m | m <-joins, boolDups m] 
>   filteredMenu = removeSubMenus filteredJoins 
>   noDupsMs  = nub filteredMenu     

findOverlaps will take two lists and return any overlapping elements. This lists indexed and the
function returns the number of shared elements. The lists are labelled left and right such that when
joined later they meet in the correct place.

> findOverlaps :: [Int] -> [Int] -> Int
> findOverlaps xs ys = findOverlaps' xs ys 0
 
> findOverlaps' :: [Int] -> [Int] -> Int-> Int
> findOverlaps' xs ys n 
>  | n == length xs = 0
>  |left == right = (length left)
>  |otherwise = findOverlaps' xs ys (n+1)
>  where
>   left =  (drop n xs)
>   right = (take (length left) ys)
 
joinMenu effectively concatenates two lists will removing any overlapping elements.

> joinMenu :: [Int] -> [Int] -> [Int]
> joinMenu ms ns = (take ((length ms) - n) ms) ++ ns
>  where 
>   n = findOverlaps ms ns
 
> removeSubMenus :: [[Int]] -> [[Int]]
> removeSubMenus ms = [m | m <-ms, null [ns | ns <- ms, (length ns) > (length m), m == (take (length m) ns)]]

> menuToChar :: [Menu] -> [[Char]]
> menuToChar menu = [map chr (map (+65) m) |m <- menu]
