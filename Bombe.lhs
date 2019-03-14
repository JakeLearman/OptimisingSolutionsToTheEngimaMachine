> module Bombe where

> import Enigma
> import Menu
> import Data.Maybe
> import Data.List
> import Data.Char

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

> runBombe' :: Traversable t => t Char -> Int -> [t Char]
> runBombe' cs n = runBombe cs n : runBombe' cs (fromJust (limitBombe (succ n)))
