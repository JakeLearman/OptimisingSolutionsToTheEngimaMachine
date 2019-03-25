> import Enigma
> import Bombe
> import Data.Char

> input :: String 
> input = "NOTHINGTOREPORTWILLREPLYINANHOUR"

> cribZip :: [(Char, Char)]
> cribZip = zip "INCOMINGTRANSMISSIONNOTHINGTOREPORT" "YIHKDEZRMXNGPMUTYAMVYESKXGYFREEJ"

> crib :: String
> crib = "INCOMINGTRANSMISSIONNOTHINGTOREPORT"

> main :: IO ()
> main = do
> let output = runMachine (map toUpper input)
> let broken = breakEnigma cribZip
> putStr "The Encrypted String is: "
> putStrLn output
> putStr "The Crib is: "
> putStrLn crib
> putStr "The deciphered text is: "
> putStrLn broken