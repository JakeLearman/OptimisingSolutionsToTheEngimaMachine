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

> stepOffset :: Offsets -> Offsets
> stepOffset (l, m, 25) = stepOffset' (l, m, 0)
> stepOffset (l, m, r) = (l, m, r+1)

> stepOffset' :: Offsets -> Offsets
> stepOffset' (l, 25, 0) = resetRotor (l, 0, 0)
> stepOffset' (l, m, 0) = (l, m+1, 0)
 
> resetRotor :: Offsets -> Offsets
> resetRotor (25, 0, 0)= (0, 0, 0)
> resetRotor (l, 0, 0)=(l+1, 0, 0)
 
> findLetter :: Char -> SteckeredPair -> Bool
> findLetter letter pair 
>  | filter (( == letter ) . fst) pair /= [] = False 
>  | filter (( == letter ) . snd) pair /= [] = False
>  | otherwise = True

> getPair :: (Char, Char) -> SteckeredPair -> Maybe SteckeredPair
> getPair (x, y) pair 
>  | filter ( == (x, y)) pair /= [] || filter ( == (y, x)) pair /= [] = Just pair
>  | findLetter x pair && findLetter y pair = Just ((x, y): pair) 
>  | otherwise = Nothing   

> steckerExample :: SteckeredPair
> steckerExample = [('A','B'), ('C','D'), ('E','F'),('G','H'), ('I','J')]

> data SteckeredEnigma = SteckeredEnigma {
>	rotors :: [(String, String)], reflector :: String,
>	grundstellung :: String, ringstellung :: String,
>	plugboard :: String, steckerPair :: SteckeredPair } deriving (Eq, Show)
