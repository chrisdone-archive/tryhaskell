tryhaskell
=====

Try Haskell at [tryhaskell.org](http://tryhaskell.org/)!

Hacking
=====

Get the Git version of Mueval from here:
https://github.com/gwern/mueval It has some additions that tryhaskell needs.

## Sandboxes

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

## Stackage and hsenv

    $ hsenv
    $ source .hsenv/bin/activate

Set your remote-repo in your `.hsenv/cabal/config` to

    stackage:http://www.stackage.org/stackage/924db6d52b90801aa1aaf7ab5d0686720d5b3964

Then install

    $ cabal update
    $ cabal install
