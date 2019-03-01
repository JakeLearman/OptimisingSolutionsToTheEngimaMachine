> module Bombe where

> import Enigma
> import Menu
> import Data.Maybe
> import Data.List
> import Data.List.Split
> import Data.Char

This basic brute force will only account for the possible combinations of rotors, not the plugboard or reflectors

A list of all the rotors which could be used in the machine set up

> rotorList :: [([Char],[Char])]
> rotorList = [rotorI, rotorII, rotorIII, rotorIV, rotorV, rotorVI, rotorVII, rotorVIII]

> steckerB :: SteckeredPair
> steckerB = [('A','B'), ('C','D'), ('E','F'),('G','H'), ('I','J')]

> bombe :: Enigma
> bombe = Enigma {
>     rotors = [],
>     reflector = reflectorB,
>     grundstellung = "AAA",
>     ringstellung = "AAA",
>     plugboard = alphabet }

