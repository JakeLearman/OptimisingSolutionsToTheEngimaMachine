> module Enigma where

> import Data.Bool
> import Data.Char
> import Data.List
> import Data.Maybe

The reverse of each function has been implemented to account for testing the encryption

> alphabet = ['A' .. 'Z']

Two functions are then defined to handle the substitution of letter when passed throughout
the various rotors in the machine.

c is the c is the char for which substitution is being handled
s is the substitution of which the permutation of the alphabet is being represented

> substitute :: [Char] -> Char -> Char
> substitute s c = fromMaybe c $ lookup c $ zip alphabet s

> unsubstitute :: [Char] -> Char -> Char
> unsubstitute s c = fromMaybe c $ lookup c $ zip s alphabet

A ceasar shift can also be implemented to account for the mapping of the alphabet to whatever
order the rotor sets the alphabet to be
The parameter k is used to represent the key of the shift

> shift :: Char -> Char -> Char
> shift k = substitute $ dropWhile (/= k) $ cycle alphabet

> unshift :: Char -> Char -> Char
> unshift k = unsubstitute $ dropWhile (/= k) $ cycle alphabet

Now we can begin to define the enigma machine:

The Enigma machine has 3 main components, the rotors, the reflectors and the plugboard. All of
which are some form of shifting letters from one to another. The Grundstellung is the starting
position of the rotors, this was chosen by the operator and was different for every message which
is one of the reasons the encryption is so strong. The ringstellung is the inital ring setting
relative to the rotor discs, this setting is effectively an initialisation vector for the
encryption adding a level of randomness to the machines encryption. The plugboard is a variable
wiring system that would be manually reconfigured by the operator using patch cables. You could
manually map letters together forming a 'steckered pair' of letters which swapped the letter
respectively both before and after the rotor scrambling.

> data Enigma = Enigma {
>	rotors :: [(String, String)], reflector :: String,
>	grundstellung :: String, ringstellung :: String,
>	plugboard :: String } deriving (Eq, Show)

Rotors:

The M3 Army Enigma Machine was adopted by the German army in 1930 and later the Navy in 1934.
Origionally the machine had 3 rotors but a later 2 were added in 1938, giving the operator a
choice of 3 out of 5. In 1939 the Navy added two more rotors: these are defined as strings below.
A 4 rotor Enigma was implemented in 1942 so rotors have been added to account for that.

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

Now we can define the Enigma machine we wish to use:

> enigmaMachine = Enigma {
>     rotors = [rotorI, rotorII, rotorIII],
>     reflector = reflectorB,
>     grundstellung = "AAA",
>     ringstellung = "AAA",
>     plugboard = alphabet }


The rotation function handles the rotation of a rotor. This function performs a case analysis
of the bool to check what the starting value of each rotor then checking if that is an element
in next rotor.
sR refers to the starting value in that rotor. nR refers to the next value in the rotor

> rotation :: Enigma -> Enigma
> rotation r = r {
>     grundstellung = [bool sR1 (shift 'B' sR1) $ sR2 `elem` nR2,
>     bool sR2 (shift 'B' sR2) $ sR2 `elem` nR2 || sR3 `elem` nR3, shift 'B' sR3
>	  ]}
>     where
>      [sR1, sR2, sR3] = grundstellung r
>      [nR1, nR2, nR3] = snd <$> rotors r

Next we apply the shift by conjugating with the alphabet

> applyShift :: [Char] -> Char -> [Char]
> applyShift cs key = unshift key . substitute cs . shift key <$> alphabet

Then we can apply the rotation to the shifted input

> applyRotation :: Enigma -> [[Char]]
> applyRotation cs = zipWith applyShift (fst <$> rotors cs) $ zipWith unshift (ringstellung cs) $ grundstellung cs

We can then follow the process of the enigma machine e through the shifting,
rotation, reflecting and the finally the plugboard mapping.

> findMap :: Enigma -> Char -> Char
> findMap e = plugboardOutput . unsubstitute rotatedInput . substitute (reflector e)
>    .  substitute rotatedInput . plugboardOutput
>    where
>      rotatedInput = foldr1 (.) (substitute <$> applyRotation e) <$> alphabet
>      plugboardOutput = substitute $ plugboard e

Finally we create a function that take a letter, return it encrypted and increment the
machine as necessary.

> encryptChar :: Enigma -> Char -> (Enigma, Char)
> encryptChar machine c = bool(machine, c)(machine', findMap machine' c) $ isLetter c
>    where machine' = rotation machine

> encryption :: Traversable t => Enigma -> t Char -> t Char
> encryption machine = snd . mapAccumL encryptChar machine

The run machine function is used to traverse a string of chars, apply encryption to each one via the engima machine

> runMachine :: Traversable t => t Char -> t Char
> runMachine cs = encryption (enigmaMachine) cs

> main = do
> 	input <- getLine
>	let output = runMachine (map toUpper input)
>	let decrypt = runMachine output
>	putStrLn output
>	putStrLn decrypt
