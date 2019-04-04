# Optimising Solutions To The Engima Machine

### Running

```
You'll need ghci or some other Haskell interpreter/compiler to run this.

The project has 2 main sections: encrypting the plantext using the Enigma machine, found in Enigma.lhs, and attempting to crack the Enimga encryption, comprised of Crib.lhs, Meny.lhs and Bombe.lhs. Some QuickCheck tests can be found in testEngima.lhs.

```
### Encrypting Some Text
```
First you need to run the Enigma script, this is done as follows:
ghci Enigma

[1 of 1] Compiling Enigma           ( Enigma.lhs, interpreted )
Ok, one module loaded.
*Enigma>
```
The Enigma Machine is made up of various parts. When choosing to encrypt some text, there are various options one can choose from in the machine's configuration. Each part will have varying effects of the complexity of the machines encryption. For the standard 3 rotor Enigma machine, typically the first 5 rotors were used in rotation and for the 4 rotor Enigma machine, a choice out of the 8 were used.  Although there are 3 options for reflectors, reflector B was most commonly used throughout WWII.

## The Rotors

|Rotor  | Encryption | Turn Over Notch|
| ------------- | -------------  | -------------|
| RotorI | EKMFLGDQVZNTOWYHXUSPAIBRCJ | Q |
| RotorII | AJDKSIRUXBLHWTMCQGZNPYFVOE | E |
| RotorIII | BDFHJLCPRTXVZNYEIWGAKMUSQO | V |
| RotorIV | ESOVPZJAYQUIRHXLNFTGKDCMWB | J|
| RotorV | VZBRGITYUPSDNHLXAWMJQOFECK | Z |
| RotorVI | JPGVOUMFYQBENHZRDKASXLICTW |M|
| RotorVII |NZJHGRCXMYSWBOUFAIVLPEKQDT|Z|
| RotorVIII |FKQHTLXOCBJSPDZRAMEWNIUYGV|M|


## The Reflectors

| Reflector | Reflected Characters |
| ------------- | ------------- |
|reflectorA | EJMZALYXVBWFCRQUONTSPIKHGD |
|reflectorB | YRUHQSLDPXNGOKMIEBFZCWVJAT |
|reflectorC | FVPJIAOYEDRZXWGCTKUQSBNMHL |

```
We can then define an Enigma machine as follows:
> enigmaMachine :: Enigma
> enigmaMachine = Enigma {
>     rotors = [rotorI, rotorII, rotorIII],
>     reflector = reflectorB,
>     grundstellung = "AAA",
>     ringstellung = "AAA",
>     plugboard = alphabet }


This Enigma machine has rotors 1-3 and reflectorB.
```

In order to encrypt some text we use the runMachine function where cs is some text we wish to encrypt and enigmaMachine is the machine we have set up.

```
> runMachine cs = encryption (enigmaMachine) cs
```
### Decrypting Some Text
In order to decrypt some cipher text, you need to run Bombe.lhs
```
ghci Bombe.lhs

[1 of 4] Compiling Enigma           ( Enigma.lhs, interpreted )
[2 of 4] Compiling Crib             ( Crib.lhs, interpreted )
[3 of 4] Compiling Menu             ( Menu.lhs, interpreted )
[4 of 4] Compiling Bombe            ( Bombe.lhs, interpreted )
Ok, four modules loaded.
*Bombe>
```
Make note that several other scripts are used in this case.

## Finding a menu
In terms of cracking Enigma, a menu refers to a link between some cipher text and a suspected string of plain text such that mappings between the letters of both can be made. If we have some plain text "BATTLEFIELD" and some cipher text "ADHFUNDBWPF" we can zip them together like so:

```
> crib2 = zip "BATTLEFIELD" "ADHFUNDBWPF"
```
we then find a menu as follows:

```
*Bombe> menuToChar(findMenu(crib2))
["HABKG"]
```
We can then call findRotorCombination' to find the rotors used to map the inputted crib and ciphertext.

```
*Bombe> findRotorCombination'(menuToChar(findMenu(crib2)))
[("EKMFLGDQVZNTOWYHXUSPAIBRCJ","Q"),("BDFHJLCPRTXVZNYEIWGAKMUSQO","V"),("AJDKSIRUXBLHWTMCQGZNPYFVOE","E")]
```
In order to retrieve the plaintext, you can define a new Enigma machine with the above settings.

## Authors

* **Jake Learman** - (https://github.com/jakelearman)

## License

This project is licensed under the BSD 2-Clause "Simplified" License - see the [LICENSE.md](LICENSE.md) file for details
