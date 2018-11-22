> module Rotor where

> import Types
> import Data.Map as Map

> type Rotor = Map.Map Echar Echar

> makeRotor :: [(Echar, Echar)] -> Map.Map Echar Echar
> makeRotor = Map.fromList

> rotateRotor :: Int -> Echar -> Echar
> rotateRotor notch = setMachineBase . (+ notch) . fromEnum