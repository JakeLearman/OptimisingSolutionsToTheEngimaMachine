> module Machine.Enigma where

> import Types
> import Rotor
> import Machine

> import Control.Monad.State
> import Data.Maybe

> main = do
>   input <- getLine
>   let encryption = fst $ runState (encryptString (stringToEchar input)) enigmaMachine
>   print encryption
>   let decryption = fst $ runState (decryptString (encryption)) enigmaMachine
>   print decryption