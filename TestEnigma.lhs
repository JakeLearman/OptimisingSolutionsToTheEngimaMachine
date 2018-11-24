> module TestEnigma where

> import Enigma
> import Test.QuickCheck

Testing reflectors:

> prop_reflector = and $ zipWith (/=) alphabet $ map (substitute reflectorB) alphabet
> prop_reflectorCheckSelfInverse = iterate (map (substitute reflectorB)) alphabet !!2 == alphabet

Testing encryption: 

> prop_enigmaExample = encryption (enigmaMachine { ringstellung = "BBB" }) "AAAAA" == "EWTYX"

> main = do
> quickCheck prop_enigmaExample
> quickCheck prop_reflector
> quickCheck prop_reflectorCheckSelfInverse