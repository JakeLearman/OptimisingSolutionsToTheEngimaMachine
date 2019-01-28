> module Crib where

> import Enigma 
> import Data.List as List
> import Data.Char
> import Data.Ord
> import Data.Tuple
> import Data.Map as Map
> import Data.Function

This basic brute force will only account for the possible combinations of rotors, not the plugboard or reflectors

A list of all the rotors which could be used in the machine set up

> rotorList :: [([Char],[Char])]
> rotorList = [rotorI, rotorII, rotorIII, rotorIV, rotorV, rotorVI, rotorVII, rotorVIII]

A function to create a list of all possible rotor combination

> combinations :: [a] -> [(a, a)]
> combinations [] = []
> combinations (x : xs) = List.map ((,) x) xs ++ combinations xs

A test machine with no rotors set up

> testMachine = Enigma {
>     rotors = [],
>     reflector = reflectorB,
>     grundstellung = "AAA",
>     ringstellung = "AAA",
>     plugboard = alphabet }

------------------------
Cracking the machine:
------------------------
The first step in attempting to crack the enigma machine is to find a crib. A crib is plaintext known to be in encrypted
string. alignCrib is responsible for comparing the crib and the encrypted string in order to see if any character is in the 
index in both strings

> alignCrib :: String -> String -> String
> alignCrib [] [] = []
> alignCrib crib encryptedOutput = List.map fst . List.filter (uncurry (==)) $ zip crib encryptedOutput

filterAlignment returns a bool depending on whether or not cribs can be aligned without error

> filterAlignment :: String -> String -> Bool
> filterAlignment crib encryptedOutput = if alignCrib crib encryptedOutput == [] then True else False

shiftCrib adds a space to the beginning of the crib such that it can be tested as a new crib for alignment

> shiftCrib :: String -> String 
> shiftCrib crib = " " ++ crib

findNoCrashes is used to return the best shifted crib with the encrypted string. A crash is used to refer to positions in which
a letter in the crib matches the letter at the same index in the encrypted string. This is because the enigma machine cannot map
letters to itself. Once this returns a valid list of pairs.

For example is we have a string KEINEBESONDERENEREIGNISSE and an encrypted string "UAENFVRLBZPWMEPMIHFSRJXFMJKWRAXQEZ" we would get
returned [(' ','U'),(' ','A'),(' ','E'),(' ','N'),(' ','F'),('K','V'),('E','R'),('I','L'),('N','B'),('E','Z'),('B','P'),
('E','W'),('S','M'),('O','E'),('N','P'),('D','M'),('E','I'),('R','H'),('E','F'),('N','S'),('E','R'),('R','J'),('E','X'),
('I','F'),('G','M'),('N','J'),('I','K'),('S','W'),('S','R'),('E','A')] as a list of pair. Note the offset of 5 letters to account
for the N's and E's matching in the original postions of the crib.

> findNoCrashes :: String -> String -> [(Char, Char)]
> findNoCrashes crib encrypted =  if (filterAlignment crib encrypted) == True then (zip crib encrypted) else findNoCrashes (shiftCrib crib) encrypted

Assuing that we have a valid crib and now have calculated the offset, we can then filter out all letters which are encrypted more than
once. For example using the above example:
        Word : U|A|E|N|F|V|R|L|B|Z|P|W|M|E|P|M|I|H|F|S|R|J|X|F|M|J|K|W|R|A|X|Q|E|Z|
        Crib : | | | | | K|E|I|N|E|B|E|S|O|N|D|E|R|E|N|E|R|E|I|G|N|I|S|S|E|
        
----------------
Finding Loops
----------------

A loop is a term used to refer to a closed cycle of letter pairings. Using the above example we can see that B is encrypted to P,
N is also encrypted to P and N is encrypted to B this relationship can be used to take advantage of the machine.

setUpForLoop removes any pairs containing spaces such that the list of tuples has already handled the offset 

> setUpForLoop :: [(Char, Char)] -> [(Char, Char)]
> setUpForLoop alignedText = [c | c <- alignedText, fst c /= ' ']

orderTuples orders the list of tuples in descending order according the the first letter in each pair.

> orderTuples :: String -> String -> [(Char, Char)]
> orderTuples crib encrypted = sortBy (comparing fst) (setUpForLoop (findNoCrashes crib encrypted))

This does the same as above but sorts by the second element

> orderTuplesSnd :: String -> String -> [(Char, Char)]
> orderTuplesSnd crib encrypted = sortBy (comparing snd) (setUpForLoop (findNoCrashes crib encrypted))

groupTuples splits the list of ordered tuples into lists where the first value is the same

> groupTuples :: String -> String -> [[(Char, Char)]]
> groupTuples crib encrypted = groupBy ((==) `on` fst) ((orderTuples crib encrypted))

invert inverts the order of the elements in a tuples

> invert :: [(a, b)] -> [(b, a)]
> invert = List.map swap

groupTuplesSnd splits the list of ordered pairs into lists where the second element is the same and then sorted so that the second element is first

> groupTuplesSnd :: String -> String -> [[(Char, Char)]]
> groupTuplesSnd crib encrypted = groupBy ((==) `on` snd) (invert(orderTuplesSnd crib encrypted))

Flatten flattens a list 

> flatten :: [[a]] -> [a]         
> flatten xs = (\z n -> List.foldr (\x y -> List.foldr z y x) n xs) (:) []

groupTuples' groups both grouped lists from groupTuples and groupTuplesSnd, flattens the list such that all elements are on the same level and
then removes any duplicate values in the list

> joinLists :: String -> String -> [[(Char, Char)]]
> joinLists crib encrypted = (groupTuples crib encrypted) ++ (groupTuplesSnd crib encrypted)

> groupTuples' :: String -> String -> [(Char, Char)]
> groupTuples' crib encrypted =  nub (flatten (joinLists crib encrypted))

groupByVertex groups each pair into a list of each vertex and each letter that is linked to that vertex

> groupByVertex :: (Eq a, Ord a) => [(a, b)] -> [(a, [b])]
> groupByVertex = List.map (\l -> (fst . head $ l, List.map snd l)) . groupBy ((==) `on` fst) . sortBy (comparing fst)