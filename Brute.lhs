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

A simple looping function

> looper :: (Eq a, Num a) => a -> IO()
> looper 0  = return ()
> looper n = do
>    putStrLn "This is temp"

A test machine with no rotors set up

> testMachine = Enigma {
>     rotors = [],
>     reflector = reflectorB,
>     grundstellung = "AAA",
>     ringstellung = "AAA",
>     plugboard = alphabet }
]

> brute = do
>   input <- getLine
>   let encryptedOutput = runMachine (map toUpper input) 
>   putStr "This is the input text: "
>   putStrLn input
>   putStr "This is the input text encrypted: "
>   putStrLn encryptedOutput
