> module Machine where

> import Types
> import Rotor

> import Control.Monad
> import Control.Monad.State

> data Machine = Machine [Rotor] RotorPosition deriving (Show, Eq)

> rotor1 = "EKMFLGDQVZNTOWYHXUSPAIBRCJ"
> rotor2 = "AJDKSIRUXBLHWTMCQGZNPYFVOE"
> rotor3 = "BDFHJLCPRTXVZNYEIWGAKMUSQO"

> alphabet = stringToEchar "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

> eRI :: [Echar]
> eRI = stringToEchar rotor1

> eRII :: [Echar]
> eRII = stringToEchar rotor2

> eRIII :: [Echar]
> eRIII = stringToEchar rotor3

> rotorI = makeRotor (zip eRI alphabet)
> rotorII = makeRotor  (zip eRII alphabet)
> rotorIII = makeRotor  (zip eRIII alphabet)

> enigmaMachine = Machine [rotorI, rotorII, rotorIII] 0

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

