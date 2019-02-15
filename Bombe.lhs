> module Bombe where

> import Enigma
> import Menu
> import Data.Maybe
> import Data.List

This basic brute force will only account for the possible combinations of rotors, not the plugboard or reflectors

A list of all the rotors which could be used in the machine set up

> rotorList :: [([Char],[Char])]
> rotorList = [rotorI, rotorII, rotorIII, rotorIV, rotorV, rotorVI, rotorVII, rotorVIII]

A function to create a list of all possible rotor combination

> combinations :: [a] -> [(a, a)]
> combinations [] = []
> combinations (x : xs) = map ((,) x) xs ++ combinations xs

> type Offsets = (Int,Int,Int)
> type SteckeredPair = [(Char,Char)]

> testMachine :: Enigma
> testMachine = Enigma {
>     rotors = [rotorI, rotorII, rotorIII],
>     reflector = reflectorB,
>     grundstellung = "AAA",
>     ringstellung = "AAA",
>     plugboard = alphabet }

> stepOffset :: Offsets -> Offsets
> stepOffset (l, m, 25) = stepOffset' (l, m, 0)
> stepOffset (l, m, r) = (l, m, r+1)

> stepOffset' :: Offsets -> Offsets
> stepOffset' (l, 25, 0) = resetRotor (l, 0, 0)
> stepOffset' (l, m, 0) = (l, m+1, 0)
 
> resetRotor :: Offsets -> Offsets
> resetRotor (25, 0, 0)= (0, 0, 0)
> resetRotor (l, 0, 0)=(l+1, 0, 0)
 
 breakEnigma :: [(Char,Char)] -> Maybe(Offsets, SteckeredPair)
 breakEnigma crib = breakEnigma' crib (findMaxCycle crib) (0,0,0) testMachine

 breakEnigma' :: [(Char,Char)] -> Menu -> Offsets -> Enigma -> Maybe(Offsets,SteckeredPair)
 breakEnigma' crib menu offsets enigma
  |newPair == Nothing && offsets == (25,25,25) = Nothing
  |newPair == Nothing = breakEnigma' crib menu (stepOffset offsets) enigma
  | otherwise = Just (offsets, (fromJust newPair))
  where newPair = findPair crib menu [(fst (crib !! (menu !! 0)),'A')] offsets enigma

 