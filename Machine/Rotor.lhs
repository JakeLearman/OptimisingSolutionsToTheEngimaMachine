> module Rotor where

> import Types
> import Data.Map as Map

> type Rotor = Map.Map Echar Echar

> type RotorPosition = Int

> incrementRotor :: RotorPosition -> RotorPosition
> incrementRotor = (+1)

> makeRotor :: [(Echar, Echar)] -> Map.Map Echar Echar
> makeRotor = Map.fromList

> rotateRotor :: Int -> Echar -> Echar
> rotateRotor notch = setMachineBase . (+ notch) . fromEnum

> rotateLetter :: Rotor -> Echar -> Echar
> rotateLetter rotor e = case (Map.lookup e rotor) of
>   Just e' -> e'
>   Nothing -> e

> convertBase :: Int -> [Int]
> convertBase 0 = [0]
> convertBase n
>   | n < sizeOfAlphabet = [n]
>   | otherwise = div n sizeOfAlphabet : convertBase (rem n sizeOfAlphabet)

> convertBase' :: Int -> Int -> [Int]
> convertBase' base n
>   | size == base = l
>   | size < base = (replicate (base - size) 0) ++ l
>   where n' = mod n (base * sizeOfAlphabet)
>         l = convertBase n'
>         size = length l

> multiRotation :: Echar -> [Rotor] -> [Int] -> Echar
> multiRotation e [] _ = e
> multiRotation e _ [] = e
> multiRotation e (r:rs) (n:ns) = multiRotation e'' rs ns
>   where e' = rotateRotor n e
>         e'' = rotateLetter r e

> multiRotation' :: Echar -> [Rotor] -> Int -> Echar
> multiRotation' e rs ns = multiRotation e rs (convertBase' (length rs) ns)