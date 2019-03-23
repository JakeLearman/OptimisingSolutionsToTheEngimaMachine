# Optimising Solutions To The Engima Machine

Description goes here..

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
The Enigma Machine is made up of various parts:
##The rotors

 Rotor  | Encryption | Turn Over Notch|
| ------------- | -------------  | -------------|
| RotorI | EKMFLGDQVZNTOWYHXUSPAIBRCJ | Q |
| RotorII | AJDKSIRUXBLHWTMCQGZNPYFVOE | E |
| RotorIII | BDFHJLCPRTXVZNYEIWGAKMUSQO | V |
| RotorIV | ESOVPZJAYQUIRHXLNFTGKDCMWB | J|
| RotorV | VZBRGITYUPSDNHLXAWMJQOFECK | Z |
| RotorVI | JPGVOUMFYQBENHZRDKASXLICTW |M|
| RotorVII |NZJHGRCXMYSWBOUFAIVLPEKQDT|Z|
| RotorVIII |FKQHTLXOCBJSPDZRAMEWNIUYGV|M|






## Authors

* **Jake Learman** - (https://github.com/jakelearman)

## License

This project is licensed under the BSD 2-Clause "Simplified" License - see the [LICENSE.md](LICENSE.md) file for details
