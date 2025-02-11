# Run tests with:
#   nix develop -c matrix-client-test
{
  description = "The matrix-client library";

  nixConfig.bash-prompt = "[nix]λ ";

  inputs = {
    nixpkgs.url =
      "github:NixOS/nixpkgs/d3780c92e64472e8f9aa54f7bbb0dd4483b98303";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        config = { };
        overlays = [
          (final: prev: {
            myHaskellPackages = prev.haskellPackages.override {
              overrides = hpFinal: hpPrev: {
                matrix-client =
                  hpPrev.callCabal2nix "matrix-client" ./matrix-client/. { };
                matrix-bot =
                  hpPrev.callCabal2nix "matrix-bot" ./matrix-bot/. { };
              };
            };
          })
        ];
        pkgs = import nixpkgs { inherit config overlays system; };
        conduitHome = "/tmp/conduit-home";

        conduitConfig = pkgs.writeTextFile {
          name = "conduit.toml";
          text = ''
            [global]
            server_name = "localhost"
            database_path = "${conduitHome}"
            database_backend = "rocksdb"
            port = 6167
            max_request_size = 20_000_000
            allow_registration = true
            allow_federation = false
            trusted_servers = []
            #log = "info,state_res=warn,rocket=off,_=off,sled=off"
            address = "127.0.0.1"
          '';
        };

        # A script to start a local matrix server with conduit
        conduitStart = pkgs.writeScriptBin "conduit-start" ''
          #!/bin/sh -e
          rm -Rf ${conduitHome}
          mkdir -p ${conduitHome}
          exec env CONDUIT_CONFIG=${conduitConfig} ${pkgs.matrix-conduit}/bin/conduit
        '';

        # A script to setup test environment
        conduitSetup = pkgs.writeScriptBin "conduit-setup" ''
          #!/bin/sh -e
          HOMESERVER_URL=http://localhost:6167
          export PATH=$PATH:${pkgs.jq}/bin:${pkgs.curl}/bin
          create_token () {
            REGISTER_TOKEN=$(curl -XPOST $HOMESERVER_URL/_matrix/client/v3/register -d '{"auth":{"type": "m.login.dummy"}, "username": "'$1'", "password": "'$2'"}' | jq -r ".access_token")
            if [ "$REGISTER_TOKEN" != "null" ]; then
                echo $REGISTER_TOKEN
            else
                curl -XPOST $HOMESERVER_URL/_matrix/client/v3/login -d '{"type": "m.login.password", "identifier": {"type": "m.id.user", "user": "'$1'"}, "password": "'$2'"}' | jq -r ".access_token"
            fi
          }
          echo HOMESERVER_URL=$HOMESERVER_URL
          echo PRIMARY_TOKEN=$(create_token "test-user" "test-pass")
          echo SECONDARY_TOKEN=$(create_token "other-user" "test-pass")
        '';

        testScript = pkgs.writeScriptBin "matrix-client-test" ''
          #!/bin/sh -ex
          # running doctest in the haskellPackages.shellFor environment seems to be
          # more reliable
          doctest ./matrix-client/ -XOverloadedStrings
          hlint .
          cabal build all
          cabal test all
        '';

      in rec {
        packages = with pkgs.myHaskellPackages; { inherit matrix-client; };
        defaultPackage = packages.matrix-client;
        devShell = pkgs.myHaskellPackages.shellFor {
          packages = p: [ p.matrix-client p.matrix-bot ];

          buildInputs = with pkgs.myHaskellPackages; [
            cabal-install
            doctest
            hlint
            pkgs.haskell-language-server
            pkgs.ghcid
            testScript
            conduitStart
            conduitSetup
          ];

          withHoogle = false;
        };
      });
}
