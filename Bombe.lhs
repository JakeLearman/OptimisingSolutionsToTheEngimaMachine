> module Bombe where

> import Enigma
> import Menu
> import Data.Maybe
> import Data.List
> import Data.List.Split
> import Data.Char

This basic brute force will only account for the possible combinations of rotors, not the plugboard or reflectors

A list of all the rotors which could be used in the machine set up

> rotorList :: [Rotor]
> rotorList = [rotorI, rotorII, rotorIII, rotorIV, rotorV, rotorVI, rotorVII, rotorVIII]

> position :: [Int]
> position = [1 .. 26]

> steckerB :: SteckeredPair
> steckerB = [('A','B'), ('C','D'), ('E','F'),('G','H'), ('I','J')]

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
> groupInNs xs n = filter ((n==). length . nub) $ mapM (const xs) [1..n]

> breakEnigma = do
>	plainText <- getLine
>	putStrLn plainText
>	let encrypted = runMachine (map toUpper plainText)
>	putStrLn encrypted