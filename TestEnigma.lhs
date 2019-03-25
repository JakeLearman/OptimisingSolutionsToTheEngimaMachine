> module TestEnigma where

> import Enigma
> import Bombe
> import Data.List
> import Data.Char
> import Test.QuickCheck

Testing substitution

> permutation key = nub $ filter isUpper key ++ alphabet

> prop_checkSubsitution key = (unsubstitute ( permutation key) . substitute (permutation key)  <$> alphabet) == alphabet

Testing reflectors:

> prop_reflector = and $ zipWith (/=) alphabet $ map (substitute reflectorB) alphabet
> prop_reflectorCheckSelfInverse = iterate (map (substitute reflectorB)) alphabet !!2 == alphabet

Testing Shift of mapping

> prop_mapping = findMap enigmaMachine 'A' == 'U'

Testing encryption:

> prop_enigmaEncryption = encryption (enigmaMachine { ringstellung = "BBB" }) "AAAAA" == "EWTYX"

Testing decryption:

> prop_enigmaDecryption = encryption(enigmaMachine{ringstellung="BBB"}) "EWTYX" == "AAAAA"

Testing Deciphering

> prop_enigmaDecipher = breakEnigma(crib3) == "NOTHINGTOREPORTWILLREPLYINANHOUR"

Testing encryption and deciphering

breakEnigma is used to break the encryption. This is done by making an Enigma machine with the rotors found using the above 
function.

> breakEnigma :: [(Char, Char)] -> [Char]
> breakEnigma crib  = runMachine (snd(unzip crib))

> prop_BruteForce = breakEnigma (zip "INCOMINGTRANSMISSIONNOTHINGTOREPORT" (runMachine "NOTHINGTOREPORTWILLREPLYINANHOUR")) == "NOTHINGTOREPORTWILLREPLYINANHOUR"

> tests = do
> quickCheck prop_checkSubsitution
> putStrLn "Checked substitution"
> quickCheck prop_reflector
> putStrLn "Checked reflector"
> quickCheck prop_reflectorCheckSelfInverse
> putStrLn "Checked reflector inversion"
> quickCheck prop_mapping
> putStrLn "Checked mapping"
> quickCheck prop_enigmaEncryption
> putStrLn "Checked encryption"
> quickCheck prop_enigmaDecryption
> putStrLn "Checked decryption"
> quickCheck prop_enigmaDecipher
> putStrLn "Checked Deciphering"
> quickCheck prop_BruteForce
> putStrLn "Checked Encryption and Brute Force"
