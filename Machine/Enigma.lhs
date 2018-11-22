> module Machine.Enigma where

> import Types
> import Rotor
> import Machine

> import Control.Monad.State
> import Data.Maybe

> main = do
>   input <- getLine
>   let encryption = fst $ runState (encryptString (stringToEchar input)) enigmaMachine
>   let decryption = fst $ runState (encryptString encryption) enigmaMachine
>   print encryption
>   print decryption
