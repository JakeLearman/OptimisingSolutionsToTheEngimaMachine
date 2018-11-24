> module Enigma where

> import Data.Maybe

> alphabet = ['A' .. 'Z']

Two functions are then defined to handle the substitution of letter when passed throughout 
the various rotors in the machine.

c is the c is the char for which substitution is being handled
s is the substitution of which the permutation of the alphabet is being represented

> substitute s c = fromMaybe c $ lookup c $ zip alphabet s

> unsubstitute s c = fromMaybe c $ lookup c $ zip s alphabet

A ceasar shift can also be implemented to account for the mapping of the alphabet to whatever
order the rotor sets the alphabet to be
The parameter k is used to represent the key of the shift

> shift k = substitute $ dropWhile (/= k) $ cycle alphabet

> unshift k = unsubstitute $ dropWhile (/= k) $ cycle alphabet

Now we can begin to define the enigma machine:

> data enigmaMachine = Enigma {} deriving (Eq, Show)