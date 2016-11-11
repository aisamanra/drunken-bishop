# `drunken-bishop`

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

This is a small implementation of the [Drunken Bishop algorithm](https://pthree.org/2013/05/30/openssh-keys-and-the-drunken-bishop/) for generating those random art images you see for SSH key fingerprints. It was mostly for my own edification. There's a library that exposes a function

```{.haskell}
drunkenBishop :: Data.ByteString.Lazy.ByteString -> String
```

which performs an MD5 hash of the input and then uses that hash to guide the Drunken Bishop algorithm. There's also an executable that reads `stdin` and formats the output image in an ASCII art frame.
