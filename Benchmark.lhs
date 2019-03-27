> import Bombe
> import Menu
> import Enigma
> import Criterion.Main

> benchmarkBreaking :: [(Char, Char)] -> [Rotor]
> benchmarkBreaking crib  = findRotorCombination' (menuToChar(findMenu crib))

> crib1 :: [(Char, Char)]
> crib1 = zip "HELLO" "ILBDA"

> crib2 :: [(Char, Char)]
> crib2 = zip "BATTLEFIELD" "ADHFUNDBWPF"

> crib3 :: [(Char, Char)]
> crib3 = zip "WETTERVORHERSAGEBISKAYA" "RWIVTYRESXBFOGKUHQBAISE"

> crib4 :: [(Char, Char)]
> crib4 = zip "KEINEBESONDERENEREIGNISSE" "VRLBZPWMEPMIHFSRJXFMJKWRA"

> crib5 :: [(Char, Char)]
> crib5 = zip "INCOMINGTRANSMISSIONNOTHINGTOREPORT" "YIHKDEZRMXNGPMUTYAMVYESKXGYFREEJ"

> crib6 :: [(Char, Char)]
> crib6 = zip "NORTHNORTHEASTRETURNFIREEXPECTBACKUPSEVENTEENHUNDRED" "JORFRNRVPHFLLFBVSORKMQZUSNRAJJYSWBZQJFFEKKSHCBAAAOMV"

> crib7 :: [(Char, Char)]
> crib7 = zip "NORTHNORTHEASTRETURNFIREEXPECTBACKUPSEVENTEENHUNDREDH" "JORFRNRVPHFLLFBVSORKMQZUSNRAJJYSWBZQJFFEKKSHCBAAAOMVK"

> crib8 :: [(Char, Char)]
> crib8 = zip "NORTHNORTHEASTRETURNFIREEXPECTBACKUPSEVENTEENHUNDREDHOURSUBOATSAPPROACH" "JORFRNRVPHFLLFBVSORKMQZUSNRAJJYSWBZQJFFEKKSHCBAAAOMVYXHCRFTCJOGZHTLBAYM"

> main = defaultMain [
>   bgroup "cribs" [ bench "Length = 5"  $ whnf benchmarkBreaking crib1
>               , bench "Length = 11"  $ whnf benchmarkBreaking crib2
>               , bench "Length = 23"  $ whnf benchmarkBreaking crib3
>               , bench "Length = 25"  $ whnf benchmarkBreaking crib4
>               , bench "Length = 35"  $ whnf benchmarkBreaking crib5
>               , bench "Length = 52"  $ whnf benchmarkBreaking crib6
>               , bench "Length = 53"  $ whnf benchmarkBreaking crib7
>               , bench "Length = 71"  $ whnf benchmarkBreaking crib8
>               ]
>  ]