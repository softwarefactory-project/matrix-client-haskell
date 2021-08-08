# matrix-client-haskell

[![Hackage](https://img.shields.io/hackage/v/matrix-client.svg)](https://hackage.haskell.org/package/matrix-client)

A client library for [matrix.org](https://matrix.org)

## Contribute

To work on this project you need a Haskell toolchain, for example on fedora:

```ShellSession
$ sudo dnf install -y ghc cabal-install && cabal update
```

Run the tests:

```ShellSession
$ cabal test
```

Some tests requires a local matrix server, run integration service:

```ShellSession
# In another terminal:
$ nix develop -c dendrite-start
# Before running cabal test:
$ export $(nix develop -c dendrite-setup)
$ cabal test
```

If you experience any difficulties, please don't hesistate to raise an issue.
