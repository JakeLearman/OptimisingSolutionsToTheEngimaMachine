> module Project where

> import Enigma
> import Crib
> import Menu
> import Bombe
> import Data.Char

> encrypt :: IO ()
> encrypt = do
>   input <- getLine
>   let output = runMachine (map toUpper input)
>   let decrypt = runMachine output
>   putStr "The Encrypted String is: "
>   putStrLn output

> bruteForce :: IO ()
> bruteForce = do
>   putStrLn "Input a phrase to encrypt"
>   input <- getLine
>   let output = runMachine (map toUpper input)
>   putStr "The Encrypted String is: "
>   putStrLn output
>   putStr "Input a crib: "
>   crib <- getLine
>   let crib' = zip output crib
>   let menu = menuToChar(findMenu crib')
>   let broken = breakEnigma crib'
>   putStrLn broken
