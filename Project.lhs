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

