> module Brute where

> import Enigma 
> import Data.List
> import Data.Char

This basic brute force will only account for the possible combinations of rotors, not the plugboard or reflectors

A list of all the rotors which could be used in the machine set up

> rotorList :: [([Char],[Char])]
> rotorList = [rotorI, rotorII, rotorIII, rotorIV, rotorV, rotorVI, rotorVII, rotorVIII]

A function to create a list of all possible rotor combination

> combinations :: [a] -> [(a, a)]
> combinations [] = []
> combinations (x : xs) = map ((,) x) xs ++ combinations xs

A test machine with no rotors set up

> testMachine = Enigma {
>     rotors = [],
>     reflector = reflectorB,
>     grundstellung = "AAA",
>     ringstellung = "AAA",
>     plugboard = alphabet }

The first step in attempting to crack the enigma machine is to find a crib. A crib is plaintext known to be in encrypted
string. alignCrib is responsible for comparing the crib and the encrypted string in order to see if any character is in the 
index in both strings

> alignCrib :: String -> String -> String
> alignCrib [] [] = []
> alignCrib crib encryptedOutput = map fst . filter (uncurry (==)) $ zip crib encryptedOutput

filterAlignment returns a bool depending on whether or not cribs can be aligned without error

> filterAlignment :: String -> String -> Bool
> filterAlignment crib encryptedOutput = if alignCrib crib encryptedOutput == [] then True else False

shiftCrib adds a space to the beginning of the crib such that it can be tested as a new crib for alignment

> shiftCrib :: String -> String 
> shiftCrib crib = " " ++ crib

findNoCrashes is used to return the best shifted crib with the encrypted string. A crash is used to refer to positions in which
a letter in the crib matches the letter at the same index in the encrypted string. This is because the enigma machine cannot map
letters to itself. Once this returns a valid list of pairs.

For example is we have a string KEINEBESONDERENEREIGNISSE and an encrypted string UAENFVRLBZPWMEPMIHFSRJXFMJKWRAXQEZ we would get
returned [(' ','U'),(' ','A'),(' ','E'),(' ','N'),(' ','F'),('K','V'),('E','R'),('I','L'),('N','B'),('E','Z'),('B','P'),
('E','W'),('S','M'),('O','E'),('N','P'),('D','M'),('E','I'),('R','H'),('E','F'),('N','S'),('E','R'),('R','J'),('E','X'),
('I','F'),('G','M'),('N','J'),('I','K'),('S','W'),('S','R'),('E','A')] as a list of pair. Note the offset of 5 letters to account
for the N's and E's matching in the original postions of the crib.

> findNoCrashes :: String -> String -> [(Char, Char)]
> findNoCrashes crib encryptedOutput =  if (filterAlignment crib encryptedOutput) == True then (zip crib encryptedOutput) else findNoCrashes (shiftCrib crib) encryptedOutput

Assuing that we have a valid crib and now have calculated the offset, we can then filter out all letters which are encrypted more than
once. For example using the above example:
        Word : U|A|E|N|F|V|R|L|B|Z|P|W|M|E|P|M|I|H|F|S|R|J|X|F|M|J|K|W|R|A|X|Q|E|Z|
        Crib : | | | | | K|E|I|N|E|B|E|S|O|N|D|E|R|E|N|E|R|E|I|G|N|I|S|S|E|

Remove removeDuplicates is used to compare the list of tuples to remove any where either the key or the value are the same

> removeDuplicates xs = nubBy compare xs 
>  where
>    compare (x, y) (x', y') = x == x' || y == y'


> brute = do
>   input <- getLine
>   let encryptedOutput = runMachine (map toUpper input) 
>   putStr "This is the input text: "
>   putStrLn input
>   putStr "This is the input text encrypted: "
>   putStrLn encryptedOutput
>   putStr "Input a crib: "
>   crib <- getLine

   alignCrib crib encryptedOutput

>   putStrLn crib
>   let crib2 = shiftCrib crib
>   putStrLn crib2