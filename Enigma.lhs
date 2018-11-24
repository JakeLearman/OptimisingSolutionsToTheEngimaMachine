> module Enigma where

> alphabet = ['A' .. 'Z']

Two functions are then defined to handle the substitution of letter when passed throughout 
the various rotors in the machine.

c is the c is the char for which substitution is being handled
s is the substitution of which the permutation of the alphabet is being represented

> substitute s c = fromMaybe c $ lookup c $ zip alphabet s

> unsubstitute s c = fromMaybe c $ lookup c $ zip s alphabet


