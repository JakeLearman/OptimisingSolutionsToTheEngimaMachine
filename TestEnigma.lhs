> module TestEnigma where

> import Enigma
> import Data.List
> import Data.Char
> import Test.QuickCheck

Testing Substitution

Testing reflectors:

> permutation key = nub $ filter isUpper key ++ alphabet

> prop_checkSubsitution key = (unsubstitute ( permutation key) . 
>                              substitute (permutation key)  <$> alphabet) == alphabet 

> prop_reflector = and $ zipWith (/=) alphabet $ map (substitute reflectorB) alphabet
> prop_reflectorCheckSelfInverse = iterate (map (substitute reflectorB)) alphabet !!2 == alphabet

Testing encryption: 

> prop_enigmaExample = encryption (enigmaMachine { ringstellung = "BBB" }) "AAAAA" == "EWTYX"

> main = do
> quickCheck prop_checkSubsitution
> quickCheck prop_reflector
> quickCheck prop_reflectorCheckSelfInverse
> quickCheck prop_enigmaExample