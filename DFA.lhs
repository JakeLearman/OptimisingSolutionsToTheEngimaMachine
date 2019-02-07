> module DFA where

> import Data.List
> import Data.Maybe
> import Control.Monad.Trans.State.Lazy

> alphabet = ['A'..'Z']
> data Input = A | B | C | D | E | F | G | H | I | J | K | L | M | N | O | P | Q | R | S | T | U | V | W | X | Y |Z deriving (Enum, Show)
> type Transition = Char
> type Output = [Char]
> type Negative = Bool
> type Running = Bool
> type MMState = (Negative, Running)
> allInput = [A ..]


> putInput :: Char -> Input
> putInput c =  allInput !! (fromJust(elemIndex c alphabet))

> putInput' :: [Char] -> [Input]
> putInput' [] = []
> putInput' (x:xs) = putInput x : putInput' xs
