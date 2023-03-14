# matrix-client-haskell

> [matrix] is an open network for secure, decentralized communication.

This project contains Haskell libraries for [matrix.org](https://matrix.org).
This allows you to rapidly integrate matrix events in your application.

## matrix-client

[![Hackage](https://img.shields.io/hackage/v/matrix-client.svg)](https://hackage.haskell.org/package/matrix-client)

A low level library to implements the [client-server spec](https://matrix.org/docs/spec/client_server/latest):

### Implemented

- [x] Basic room membership to leave and join rooms
- [x] Basic sync to read room's timeline
- [x] Sending text events
- [x] Identity lookup
- [x] Invites

### Next/Missing

- [ ] Rich messages
- [ ] Decoders to support OLM with https://github.com/livmackintosh/matrix-sdk
- [ ] The rest of the HTTP API...

## Contribute

To work on this project you need a Haskell toolchain, for example on fedora:

```ShellSession
$ sudo dnf install -y ghc cabal-install && cabal update
```

Run the tests:

```ShellSession
$ cabal build all && cabal test all
```

Some tests requires a local matrix server, run integration service:

```ShellSession
# In another terminal:
$ nix develop -c conduit-start
# Before running cabal test:
$ export $(nix develop -c conduit-setup)
$ cabal test all
```

Restart the commands to run the test on a fresh environment.

If you experience any difficulties, please don't hesistate to raise an issue.
