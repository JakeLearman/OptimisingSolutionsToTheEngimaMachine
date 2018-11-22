> module Types where

> import qualified Data.Map as Map (Map, lookup, fromList)
> import Data.Char (isLower)
> import Data.Tuple (swap)


> data Echar  = A | B | C | D | E |F | G | H | I | J |
>                   K | L | M | N | O | P | Q | R | S | T |
>                   U | V | W | X | Y | Z 
>               deriving (Show, Eq, Enum, Ord, Bounded)

> sizeOfAlphabet :: Int
> sizeOfAlphabet = (+1) $ fromEnum (maxBound :: Echar)

> eHashTable = [('a', A), ('A', A), ('b', B), ('B', B), ('c', C), ('C', C), ('d', D), ('D', D), 
>               ('e', E), ('E', E), ('f', F), ('F', F), ('g', G), ('G', G), ('h', H), ('H', H),
>               ('i', I), ('I', I), ('j', J), ('J', J), ('k', K), ('K', K), ('l', L), ('L', L),
>               ('m', M), ('M', M), ('n', N), ('N', N), ('o', O), ('O', O), ('p', P), ('P', P),
>               ('q', Q), ('Q', Q), ('r', R), ('R', R), ('s', S), ('S', S), ('t', T), ('T', T),
>               ('u', U), ('U', U), ('v', V), ('V', V), ('w', W), ('W', W), ('x', X), ('X', X),
>               ('y', Y), ('Y', Y), ('z', Z), ('Z', Z)]

> charMap :: Map.Map Char Echar
> charMap = Map.fromList eHashTable

> charToEchar :: Char -> Maybe Echar
> charToEchar c = Map.lookup c charMap


Decoding: 

> echarMap :: Map.Map Echar Char
> echarMap  = Map.fromList $ map swap $ filter (not . isLower . fst) eHashTable

> echarToChar :: Echar -> Maybe Char
> echarToChar e = Map.lookup e echarMap