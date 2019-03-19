> module Bombe where

> import Enigma
> import Menu
> import Data.Maybe
> import Data.List
> import Data.Char
> import Data.Ord

A list of all the rotors which could be used in the machine set up

> rotorList :: [Rotor]
> rotorList = [rotorI, rotorII, rotorIII, rotorIV, rotorV, rotorVI, rotorVII, rotorVIII]

> position :: [Int]
> position = [1 .. 26]

A Bombe is similar to an Enigma machine expect that the rotor list is left empty - this is due to the fact the following code 
will be working out the rotors based upon the encrypted text. 

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
> groupInNs xs n = filter ((n==) . length . nub) $ mapM (const xs) [1..n]

Fetch rotor combination fetches a set of rotors at a specifies index

> fetchRotorCombination ::  Eq a => [a] -> Int -> [a]
> fetchRotorCombination rs n = (groupInNs rs 3) !! n  

runBombe will take in a list, in this case the alphabet and return all possible encryption variations possible

> runBombe :: Traversable t => t Char -> Int -> t Char
> runBombe cs n = encryption (e) cs
>   where e = Enigma {
>     rotors = (fetchRotorCombination rotorList n),
>     reflector = reflectorB, grundstellung = "AAA",
>     ringstellung = "AAA", plugboard = alphabet }

The rotor length is used as a limiter in recursive functions to ensure no out of bounds errors can occur

> rotorLength :: Int
> rotorLength = length (groupInNs rotorList 3) 

Limit bombe is used to handle any missing return value errors 

> limitBombe :: Int -> Maybe Int
> limitBombe n = if n < rotorLength then Just n else Nothing

runBombe' uses list comprehension to iterate through the list of rotors such that every rotor combination encrypts the alphabet
once

> runBombe' :: Traversable t => t Char -> [t Char]
> runBombe' cs = [runBombe cs n | n <- [0 .. (rotorLength -1)]]

prepBreak makes use of a predicate as a placeholder for the alphabet when the function is called in its prime form below. 
It takes an integer and a list of characters (i.e. the alphabet) and calls the encrypted version found in the encrypted 
alphabets made in runBombe. This is then zipped with the alphabet to create a list of tuples 

> prepBreak :: p -> Int -> [(Char, Char)]
> prepBreak xs n = zip alphabet $ runBombe alphabet n

prepBreak' is a traversable version of the above function which traverses through the list of rotors in order to zip each
encrypted alphabet with the standard alphabet in order to create a list of tupled letter pairs where one of which in encrypted.

> prepBreak' :: Traversable t => t Char -> [[(Char, Char)]]
> prepBreak' xs = [prepBreak x n| x <- runBombe' xs, n <- [0 .. (rotorLength -1)]]

cribUp is used to take the menu and split it into a list of tuples such that every letter in the menu can be mapped to any other 
letter as long as they are not equal

> cribUp :: Eq a => [a] -> [(a,a)]
> cribUp menu = [(x, y) | x <- menu, y <- menu , x /= y]

findClosestMatch is used to find the closest encrypted alphabet to the mappings found in the menu.

> findClosestMatch :: [[Char]] -> Int -> [(Char, Char)]
> findClosestMatch menu n =  intersect (cribUp (head menu)) (prepBreak alphabet n)

findClosestMatch' is used to iterate through the list of encrypted alphabets and find the best match based upon the menu
inputted

> findClosestMatch' :: [[Char]] -> [[(Char, Char)]]
> findClosestMatch' menu = [findClosestMatch menu n | n <- [0 .. (rotorLength -1)]]

sortMatches iis used to sort the matches by length

> sortMatches :: [[a]] -> [[a]]
> sortMatches = sortBy (comparing length)

fetchClosestMatch uses the list of matches to find the largest match between the menu and the encrypted alphabet. This means
that the largest majority of letter mappings can be found.

> fetchClosestMatch :: [[Char]] -> [(Char, Char)]
> fetchClosestMatch menu = head(reverse(sortMatches(findClosestMatch' menu)))

findRotorCombination uses list comprehension in order to find if the closest matches in each menu can be found in a set
encrypted alphabet. A maybe wrapper is used too handle any null elements.

> findRotorCombination :: [[Char]] -> [Maybe Int]
> findRotorCombination menu = [elemIndex x y | x <- (fetchClosestMatch menu), y <- (prepBreak' alphabet)]

filterRotors is used to remove all rotor combinations that return the nothing type meaning that only valid letters indexs are 
found

> filterRotors :: [[Char]] -> [Maybe Int]
> filterRotors menu = filter (/= Nothing) (findRotorCombination menu)

findRotorCombination is used to find which specific set of rotors is used to decrypt the cipher text used to form the menu
back to its original plaintext.

> findRotorCombination' :: [[Char]] -> [Rotor]
> findRotorCombination' menu = fetchRotorCombination rotorList ((fromJust(head (filterRotors menu))) - 1)

breakEnigma is used to break the encryption. This is done by making an Enigma machine with the rotors found using the above 
function.

> breakEnigma crib = encryption (e) (snd(unzip crib))
>  where e = Enigma {
>     rotors = (findRotorCombination' (menuToChar(findMenu crib))),
>     reflector = reflectorB, grundstellung = "AAA",
>     ringstellung = "AAA", plugboard = alphabet }


> crib2 = zip "HELLOTHERE" "ILBDA"

> menu1 = menuToChar(findMenu crib1)
> menu2 = menuToChar(findMenu crib2)
