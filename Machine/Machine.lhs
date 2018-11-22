> module Machine where

> import Types
> import Rotor

> import Control.Monad
> import Control.Monad.State

> data Machine = Machine [Rotor] RotorPosition deriving (Show, Eq)

> rotor1 = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
> rotor2 = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
> rotor3 = "BDFHJLCPRTXVZNYEIWGAKMUSQO"
> rotor4 = "ESOVPZJAYQUIRHXLNFTGKDCMWB"
> rotor5 = "VZBRGITYUPSDNHLXAWMJQOFECK"

> alphabet = stringToEchar "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

> eRI = stringToEchar rotor1
> eRII = stringToEchar rotor2
> eRIII = stringToEchar rotor3
> eRIV = stringToEchar rotor4
> eRV = stringToEchar rotor5

> rotorI = makeRotor (zip eRI alphabet)
> rotorII = makeRotor (zip eRII alphabet)
> rotorIII = makeRotor (zip eRIII alphabet)
> rotorIV = makeRotor (zip eRIV alphabet)
> rotorV = makeRotor (zip eRV alphabet)

> enigmaMachine :: Machine
> enigmaMachine = Machine [rotorI, rotorII, rotorIII, rotorIV, rotorV] 0

> encrypt :: Echar -> State Machine Echar
> encrypt e = do
>   (Machine rotor position) <- get
>   put (Machine rotor (incrementRotor position))
>   return $ multiRotation' e rotor position

> encryptString :: [Echar] -> State Machine [Echar]
> encryptString es = mapM encrypt es

Decryption:

> decrypt :: Echar -> State Machine Echar
> decrypt e = do
>   (Machine rotor position) <- get
>   put (Machine rotor (incrementRotor position))
>   return $ reverseRotation' e rotor position

> decryptString :: [Echar] -> State Machine [Echar]
> decryptString es = mapM decrypt es

