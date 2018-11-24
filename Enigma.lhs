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

The Enigma machine has 3 main components, the rotors, the reflectors and the plugboard. All of 
which are some form of shifting letters from one to another. The Grundstellung is the starting
position of the rotors, this was chosen by the operator and was different for every message which
is one of the reasons the encryption is so strong. The ringstellung is the inital ring setting 
relative to the rotor discs, this setting is effectively an initialisation vector for the 
encryption adding a level of randomness to the machines encryption.

> data Enigma = Enigma {
>	rotors :: [(String, String)], reflector :: String,
>	grundstellung :: String, ringstellung :: String,
>	plugboard :: String } deriving (Eq, Show)

Rotors:

The M3 Army Enigma Machine was adopted by the German army in 1930 and later the Navy in 1934.
Origionally the machine had 3 rotors but a later 2 were added in 1938, giving the operator a 
choice of 3 out of 5. In 1939 the Navy added two more rotors: these are defined as strings below.

> rotorI   = ("EKMFLGDQVZNTOWYHXUSPAIBRCJ", "Q")
> rotorII  = ("AJDKSIRUXBLHWTMCQGZNPYFVOE", "E")
> rotorIII = ("BDFHJLCPRTXVZNYEIWGAKMUSQO", "V")
> rotorIV  = ("ESOVPZJAYQUIRHXLNFTGKDCMWB", "J")
> rotorV   = ("VZBRGITYUPSDNHLXAWMJQOFECK", "Z")
> rotorVI  = ("JPGVOUMFYQBENHZRDKASXLICTW", "M")
> rotorVII = ("NZJHGRCXMYSWBOUFAIVLPEKQDT", "Z")
> rotorVIII = ("FKQHTLXOCBJSPDZRAMEWNIUYGV", "M")

The letters at the end of the rotor definintions are the turnover notches. These were point 
if a specific shift in letter occur, it would rotor the next rotor as well as the one being
used.

Reflectors:

A reflector (or reversal rotor) is where the outputs of the final rotor were connected in 
pairs and then redirected back through the rotors via a different route. This ensured that no
letter could be mapped back to itself and that encryption was the same as decryption - a  flaw 
which made cracking the machine possible.

> reflectorA = "EJMZALYXVBWFCRQUONTSPIKHGD"
> reflectorB = "YRUHQSLDPXNGOKMIEBFZCWVJAT"
> reflectorC = "FVPJIAOYEDRZXWGCTKUQSBNMHL"