> module Bombe where

> import Enigma
> import Menu
> import Data.Maybe
> import Data.List
> import Data.Char
> import Data.Ord

This basic brute force will only account for the possible combinations of rotors, not the plugboard or reflectors

A list of all the rotors which could be used in the machine set up

> rotorList :: [Rotor]
> rotorList = [rotorI, rotorII, rotorIII, rotorIV, rotorV, rotorVI, rotorVII, rotorVIII]

> position :: [Int]
> position = [1 .. 26]

> bombe :: Enigma
> bombe = Enigma {
>     rotors = [],
>     reflector = reflectorB,
>     grundstellung = [],
>     ringstellung = [],
>     plugboard = alphabet}

The groupInNs takes a list of elements and an integer n and returns all possible combinations of the elements in the list
in sublists of size n.

> groupInNs :: Eq a => [a] -> Int -> [[a]]
> groupInNs xs n = filter ((n==) . length . nub) $ mapM (const xs) [1..n]

Fetch rotor combination fetches a set of rotors at a specifies index

> fetchRotorCombination ::  Eq a => [a] -> Int -> [a]
> fetchRotorCombination rs n = (groupInNs rs 3) !! n

> runBombe :: Traversable t => t Char -> Int -> t Char
> runBombe cs n = encryption (e) cs
>   where e = Enigma {
>     rotors = (fetchRotorCombination rotorList n),
>     reflector = reflectorB, grundstellung = "AAA",
>     ringstellung = "AAA", plugboard = alphabet }

> rotorLength :: Int
> rotorLength = length (groupInNs rotorList 3) 

> limitBombe :: Int -> Maybe Int
> limitBombe n = if n < rotorLength then Just n else Nothing

> runBombe' :: Traversable t => t Char -> [t Char]
> runBombe' cs = [runBombe cs n | n <- [0 .. (rotorLength -1)]]

> prepBreak :: p -> Int -> [(Char, Char)]
> prepBreak xs n = zip alphabet $ runBombe alphabet n

> prepBreak' :: Traversable t => t Char -> [[(Char, Char)]]
> prepBreak' xs = [prepBreak x n| x <- runBombe' xs, n <- [0 .. (rotorLength -1)]]

> crib1 = zip "KEINEBESONDERENEREIGNISSE" "RWIVTYRESXBFOGKUHQBAISE"

> menu1 = menuToChar(findMenu crib1)

> cribUp :: Eq a => [a] -> [(a,a)]
> cribUp menu = [(x, y) | x <- menu, y <- menu , x /= y]

> findClosestMatch :: [[Char]] -> Int -> [(Char, Char)]
> findClosestMatch menu n =  intersect (cribUp (head menu)) (prepBreak alphabet n)

> findClosestMatch' :: [[Char]] -> [[(Char, Char)]]
> findClosestMatch' menu = [findClosestMatch menu n | n <- [0 .. (rotorLength -1)]]

> sortMatches :: [[a]] -> [[a]]
> sortMatches = sortBy (comparing length)

> fetchClosestMatch :: [[Char]] -> [(Char, Char)]
> fetchClosestMatch menu = head(reverse(sortMatches(findClosestMatch' menu)))
