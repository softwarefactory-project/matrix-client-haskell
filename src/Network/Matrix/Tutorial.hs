-- | The @matrix-client@ library provides a simple interface for interacting with Matrix servers.
--
-- This tutorial introduces how to use the @matrix-client@ library.
--
-- You will need a token to create a session, if you already have an account, you can get it with the @element@ client
-- by visiting the account @Settings@ page, @Help & About@ panel, then click @Access Token@.
--
-- Alternatively, you can setup a test service by running these commands in a terminal:
--
-- > git clone https://github.com/matrix-org/dendrite
-- > cd dendrite; ./build.sh; ./bin/generate-keys --private-key matrix_key.pem; cp dendrite-config.yaml dendrite.yaml
-- > ./bin/dendrite-monolith-server --config dendrite.yaml
-- > curl -XPOST http://localhost:8008/_matrix/client/r0/register -d'{"username": "tristanC", "password": "supersecret", "auth": {"type": "mlogin.dummy"}}
--
-- To avoid manipulating the token directly, put it in your environment:
--
-- > export MATRIX_TOKEN="THE_ACCESS_TOKEN"
module Network.Matrix.Tutorial
  ( -- * Introduction
    -- $intro

    -- * Create a session
    -- $session
  )
where

-- $intro
--   To start using this library you need a haskell toolchain, on fedora run:
--
--   > $ sudo dnf install -y ghc cabal-install && cabal update
--
--   Then get a copy of the library by running:
--
--   > $ git clone https://github.com/softwarefactory-project/matrix-client-haskell
--   > $ cd matrix-client-haskell
--
--   Start a REPL:
--
--   > $ cabal repl
--   > Prelude> import Network.Matrix.Client
--   > Prelude Netowrk.Matrix.Client> :set prompt "> "
--   > > :set -XOverloadedStrings
--   > > :type getTokenOwner
--   > getTokenOwner :: ClientSession -> MatrixIO WhoAmI

-- $session
--   Most functions require 'ClientSession' which carries the
--   endpoint url and the http client manager.
--
--   The only way to get the client is through the 'Client.createSession' function:
--
--   > > token <- getTokenFromEnv "MATRIX_TOKEN"
--   > > sess <- createSession "https://matrix.org" token
--   > > getTokenOwner sess
--   > Right (WhoAmI "@tristanc_:matrix.org")
