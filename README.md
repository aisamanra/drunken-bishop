# `drunken-bishop`

The [Drunken Bishop](http://www.dirk-loss.de/sshvis/drunken_bishop.pdf) algorithm is a visual fingerprinting algorithm originally implemented by OpenSSH for visualizing key fingerprints. This package implements OpenSSH's visualization while also offering extra configuration to allow for specialized uses.

## Basic usage

The `Data.Digest.DrunkenBishop(drunkenBishop)` function takes a bytestring and produces a multiline string representing the visualization of the hash of that string.

```haskell
>>> :set -XOverloadedStrings
>>> putStrLn (drunkenBishop "hello!")

             .
          . o
         = o
      . ^ B .
       o = . o +
        ..+.+ = E
         +o..=
         ..  ..

```

## Configurable usage

The `Data.Digets.DrunkenBishop(DrunkenBishopOptions)` type contains several fields which can be configured to change the operation of the algorithm. The default used by the `drunkenBishop` function are kept in an instance of this type called `drunkenBishopDefaultOptions`.

The `drunkenBishopBoardSize` field can set the size of the overall fingerprint image. Note that smaller images will produce "noisier" fingerprints, while larger images will likely produce "sparser" fingerprints.

```haskell
> :set -XOverloadedStrings
> let opts = drunkenBishopDefaultOptions { drunkenBishopBoardSize = (9, 9) }
> putStrLn (drunkenBishopWithOptions opts "hello!")

        .
      ...
    .+..
  . ^++.
   o.=.oo
 .o.+ =.E
 oo..=  .
 ..  ..
```

The `drunkenBishopInitialPosition` field can change the initial position of the bishop:

```haskell
> :set -XOverloadedStrings
> let opts = drunkenBishopDefaultOptions { drunkenBishopInitialPosition = Just (0, 0) }
> putStrLn (drunkenBishopWithOptions opts "hello!")
S .   ..
 . o ..
o o.+.
.. ++ .
  .... . o
   .  ..o..
   .o.ooo E
   oo..o.
   ..  .

```

The `drunkenBishopCharMap` field can change the ASCII visualizer used for the cells of the visualization algorithm:

```haskell
> :set -XOverloadedStrings
> import Data.Char (chr)
> let opts = drunkenBishopDefaultOptions { drunkenBishopCharMap = \ c -> chr (fromIntegral (c + 65)) }
> putStrLn (drunkenBishopWithOptions opts "hello!")
AAAAAAAAAAAAAAAAA
AAAAAAAAAAAAABAAA
AAAAAAAAAABACAAAA
AAAAAAAAAEACAAAAA
AAAAAABAOAGABAAAA
AAAAAAACAEABACADA
AAAAAAAABBDBDAEAQ
AAAAAAAAADCBBEAAA
AAAAAAAAABBAABBAA
```

Finally, the `drunkenBishopHash` field can change the hash function used by the Drunken Bishop algorithm. This is parametric and can take any type, but must produce a strict `ByteString`. Note that the default MD5 hash produces 128-bit values, so hash functions that produce more values will produce considerably "noisier" fingerprints unless you also change the fingerprint size. The following example uses `hashlazy` from the `cryptohash-sha256` package as an example:

```haskell
> :set -XOverloadedStrings
> import Crypto.Hash.SHA256 (hashlazy)
> let opts = drunkenBishopDefaultOptions { drunkenBishopHash = hashlazy }
> putStrLn (drunkenBishopWithOptions opts "hello!")
           .oo+
   .        .= o
  . o   .   o + .
   o . = . . = ..
  . = = ^ . . =o
   = + o +.    o+
    o ooo=.o  . o
   .  ..= =.o  o.
      .o.E.. o...

```

## The `drunken-bishop` executable

```
$ echo 'drunken bishop' | drunken-bishop
+-----------------+
|      o.. ..o  Eo|
|     .o..o o ...o|
|       *. . .. . |
|      o +.. .    |
|       =S. .     |
|      +o .       |
|   . o  .        |
|    o            |
|                 |
+-----------------+
```
