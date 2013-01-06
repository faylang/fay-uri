fay-uri
=======

This is a thin persistent FFI layer for using
[jsUri](https://github.com/derek-watson/jsUri) with
[Fay](http://www.fay-lang.org).

See the source code for comments on the implementation, names are kept
similar to what jsUri uses. Hopefully it's easy to understand even if
you haven't used jsUri before. The argument order is changed to be
more haskelly, such as always putting the Uri as the last argument and
not combining setting and removal functions. Any other changes are
documented in the source.

Since jsUri is a small library fay-uri also serves as a good example
on how to write FFI bindings in Fay.

Usage
-----

To use this with fay, cabal install the package which will put the
source files in fay ~/.cabal/share/fay-uri-0.1.0.0/src. You can then
compile with fay using

in fay >= 0.12:
```bash
fay --package fay-uri MyFile.hs
```

in fay < 0.12:
```bash
fay --include ~/.cabal/share/fay-uri-0.1.0.0/src MyFile.hs
```

Example
-------
```haskell
import Language.Fay.Prelude
import Language.Fay.Uri

main :: Fay ()
main = putStrLn . toString . removePath . withProtocol "https" . newUri =<< currentUri
```
