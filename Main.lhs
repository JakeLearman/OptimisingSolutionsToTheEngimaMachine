> import Enigma
> import Bombe
> import Menu
> import Data.Char

> crib1 :: [(Char, Char)]
> crib1 = zip "HELLO" "ILBDA"

> crib2 :: [(Char, Char)]
> crib2 = zip "BATTLEFIELD" "ADHFUNDBWPF"

> crib3 :: [(Char, Char)]
> crib3 = zip "INCOMINGTRANSMISSIONNOTHINGTOREPORT" "YIHKDEZRMXNGPMUTYAMVYESKXGYFREEJ"

> crib4 :: [(Char, Char)]
> crib4 = zip "NORTHNORTHEASTRETURNFIREEXPECTBACKUPSEVENTEENHUNDRED" "JORFRNRVPHFLLFBVSORKMQZUSNRAJJYSWBZQJFFEKKSHCBAAAOMV"

> crib5 :: [(Char, Char)]
> crib5 = zip "NORTHNORTHEASTRETURNFIREEXPECTBACKUPSEVENTEENHUNDREDH" "JORFRNRVPHFLLFBVSORKMQZUSNRAJJYSWBZQJFFEKKSHCBAAAOMVK"

> crib6 :: [(Char, Char)]
> crib6 = zip "NORTHNORTHEASTRETURNFIREEXPECTBACKUPSEVENTEENHUNDREDHOURSUBOATSAPPROACH" "JORFRNRVPHFLLFBVSORKMQZUSNRAJJYSWBZQJFFEKKSHCBAAAOMVYXHCRFTCJOGZHTLBAYM"

> runMachine' crib = encryption (enigmaMachine) (snd(unzip crib))
>   where enigmaMachne = Enigma {
>     rotors = (findRotorCombination' (menuToChar(findMenu crib))),
>     reflector = reflectorB,
>     grundstellung = "AAA",
>     ringstellung = "AAA",
>     plugboard = alphabet }

