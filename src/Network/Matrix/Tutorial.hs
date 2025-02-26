{- | The @matrix-client@ library provides a simple interface for interacting with Matrix servers.

This tutorial introduces how to use the @matrix-client@ library.

You will need a token to create a session, if you already have an account, you can get it with the @element@ client
by visiting the account @Settings@ page, @Help & About@ panel, then click @Access Token@.

Alternatively, you can setup a test service by running these commands in a terminal:

> git clone https://github.com/matrix-org/dendrite
> cd dendrite; ./build.sh; ./bin/generate-keys --private-key matrix_key.pem; cp dendrite-config.yaml dendrite.yaml
> ./bin/dendrite-monolith-server --config dendrite.yaml
> curl -XPOST http://localhost:8008/_matrix/client/r0/register -d'{"username": "tristanC", "password": "supersecret", "auth": {"type": "m.login.dummy"}}'

To avoid manipulating the token directly, put it in your environment:

> export MATRIX_TOKEN="THE_ACCESS_TOKEN"
-}
module Network.Matrix.Tutorial (
    -- * Introduction
    -- $intro

    -- * Create a session
    -- $session

    -- * Get messages
    -- $sync

    -- * Lookup identity
    -- $identity
)
where

{- $intro
  To start using this library you need a haskell toolchain, on fedora run:

  > $ sudo dnf install -y ghc cabal-install && cabal update

  Then get a copy of the library by running:

  > $ git clone https://github.com/softwarefactory-project/matrix-client-haskell
  > $ cd matrix-client-haskell

  Start a REPL:

  > $ cabal repl
  > Prelude> import Network.Matrix.Client
  > Prelude Netowrk.Matrix.Client> :set prompt "> "
  > > :set -XOverloadedStrings
  > > :type getTokenOwner
  > getTokenOwner :: ClientSession -> MatrixIO WhoAmI
-}

{- $session
  Most functions require 'Network.Matrix.Client.ClientSession' which carries the
  endpoint url and the http client manager.

  The only way to get the client is through the 'Network.Matrix.Client.createSession' function:

  > > token <- getTokenFromEnv "MATRIX_TOKEN"
  > > sess <- createSession "https://matrix.org" token
  > > getTokenOwner sess
  > Right (WhoAmI "@tristanc_:matrix.org")
-}

{- $sync
  Create a filter to limit the sync result using the 'Network.Matrix.Client.createFilter' function.
  To keep room message only, use the 'Network.Matrix.Client.messageFilter' default filter:

  > > Right userId <- getTokenOwner sess
  > > Right filterId <- createFilter sess userId messageFilter
  > > getFilter sess (UserID "@gerritbot:matrix.org") filterId
  > Right (Filter {filterEventFields = ...})

  Call the 'Network.Matrix.Client.sync' function to synchronize your client state:

  > > Right syncResult <- sync sess (Just filterId) Nothing (Just Online) Nothing
  > > putStrLn $ take 512 $ show (getTimelines syncResult)
  > SyncResult {srNextBatch = ...}

  Get next batch with a 300 second timeout using the @since@ argument:

  > > Right syncResult' <- sync sess (Just filterId) (Just (srNextBatch syncResult)) (Just Online) (Just 300000)

  Here are some helpers function to format the messages from sync results, copy them in your REPL:

  > > import qualified Data.Text.IO as Text
  > > :{
  >   let printEvent re = Text.putStrLn $ case reContent re of
  >         EventRoomMessage (RoomMessageText mt) -> unAuthor (reSender re) <> ": " <> mtBody mt
  >         _ -> ""
  >   :}
  > > let printRoomEvent room event = Text.putStr room >> putStr "| " >> printEvent event
  > > let printRoomEvents (RoomID room, events) = traverse (printRoomEvent room) events
  > > let printTimelines sr = mapM_ printRoomEvents (getTimelines sr)
  > > printTimelines syncResult
  > ...

  Use the 'Network.Matrix.Client.syncPoll' utility function to continuously get events,
  here is an example to print new messages, similar to a @tail -f@ process:

  > > syncPoll sess (Just filterId) (Just (srNextBatch syncResult)) (Just Online) printTimelines
  > room1| test-user: Hello world!
  > ...
-}

{- $identity
 To use the Identity api you need another token. Get it by running these commands:

 > $ MATRIX_OPENID=$(curl -XPOST https://matrix.org/_matrix/client/r0/user/${USER}/openid/request_token -H "Authorization: Bearer ${MATRIX_TOKEN}" -d '{}')
 > $ export MATRIX_IDENTITY_TOKEN=$(curl -XPOST https://matrix.org/_matrix/identity/v2/account/register -d "${MATRIX_OPENID}" | jq -r '.access_token')

 Then here is how to lookup a matrix identity:

 > > import Network.Matrix.Identity
 > > tokenId <- getTokenFromEnv "MATRIX_IDENTITY_TOKEN"
 > > sessId <- createIdentitySession "https://matrix.org" tokenId
 > > Right hd <- hashDetails sessId
 > > identityLookup sessId hd (Email "tdecacqu@redhat.com")
 > Right (Just (UserID "@tristanc_:matrix.org"))
-}
