> module Machine where

> import Types
> import Rotor

> import Control.Monad
> import Control.Monad.State

> data Machine = Machine [Rotor] RotorPosition deriving (Show, Eq)

> rotor1 = makeRotor  [ (A,B), (B,A), (C,D), (D,C) ]
> rotor2 = makeRotor [ (A,C), (C,A), (B,D), (D,B) ]

> enigmaMachine = Machine [rotor1, rotor2] 0

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

