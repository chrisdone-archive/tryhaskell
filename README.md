tryhaskell
=====

Try Haskell at [tryhaskell.org](http://tryhaskell.org/)!

Hacking
=====

Get the Git version of Mueval from here:
https://github.com/gwern/mueval It has some additions that tryhaskell needs.

Build tryhaskell
```
$ cabal sandbox init
$ cabal install --only-dependencies
$ cabal build
$ cabal sandbox hc-pkg hide monads-tf
```

Start tryhaskell, hosted at http://127.0.0.1:4001/
```
$ env PATH=./.cabal-sandbox/bin:$PATH \
  GHC_PACKAGE_PATH=$(cabal sandbox hc-pkg list | grep '^/.*\.cabal-sandbox') \
  ./dist/build/tryhaskell/tryhaskell
```

tryhaskell does not currently support any command line arguments
or configuration files.
