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

The offset is a mechanism for connecting outputs of rotors at specific locations. When a rotor is stepped, it is important for the
offset to be taken into accoutn such that the it known where the signal will enter the next rotor. This is represented by a triple where
each digit represents a rotor

> type Offsets = (Int,Int,Int)
> type Stecker = [(Char,Char)]

The stepOffset function is used to handle the rotation of the rotors, the right most integer is incremented once every time the 
function is called on the triple. Once this number passes 25, it resets to 0 and increments the column to the left by 1. This ensures
that each letter of the alphabet is incremented at least once.

> stepOffset :: Offsets -> Offsets
> stepOffset (l, m, 25) = stepOffset' (l, m, 0)
> stepOffset (l, m, r) = (l, m, r+1)

stepOffset' is responsible for handling the rotation of the middle rotor, such that when it reaches 25, the leftmost rotor is incremented and 
the middle rotor is reset to 0. Furthermore it ensures that when the right is 0 and the middle is 0, the leftmost rotor is incremented approprately. 

> stepOffset' :: Offsets -> Offsets
> stepOffset' (l, 25, 0) = resetRotor (l, 0, 0)
> stepOffset' (l, m, 0) = (l, m+1, 0)
 
The reset rotor function is used to reset the rotors once the final rotor reaches 25 - this means that the rotors in the machine have performed
a full cycle. This is useful in the bombe machine as it ensures that when the menu is passed into the machine, each and every possible combination 
of rotor is fully handled.

> resetRotor :: Offsets -> Offsets
> resetRotor (25, 0, 0)= (0, 0, 0)
> resetRotor (l, 0, 0)=(l+1, 0, 0)
 
findLetter is a function used to check whether or not a letter is found in the list of steckered pairs derived from the menu. It takes in a letter
and a list of pair. It then searches both the first and second position of each pair and returns a boolean value depending on if it exists. The function
returns true if the letter is not found in a pair.
 
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

> data Bombe = Bombe {Rotor Rotor Rotor Reflector Stecker} deriving (Eq, Show)
> bombe = bombe rotorI rotorII rotorIII reflectorB steckerExample
