# Run tests with:
#   nix develop -c matrix-client-test
{
  description = "The matrix-client library";

  nixConfig.bash-prompt = "[nix]λ ";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ] (system:
      let
        config = { };
        compilerVersion = "8104";
        compiler = "ghc" + compilerVersion;
        overlays = [
          (final: prev: {
            haskell-language-server = prev.haskell-language-server.override {
              supportedGhcVersions = [ compilerVersion ];
            };

            myHaskellPackages = prev.haskell.packages.${compiler}.override {

            };
          })
        ];
        pkgs = import nixpkgs { inherit config overlays system; };
        dendriteHome = "/tmp/dendrite-home";

        # A script to start a local matrix server with dendrite
        dendriteStart = pkgs.writeScriptBin "dendrite-start" ''
          #!/bin/sh -e
          mkdir -p ${dendriteHome}
          cd ${dendriteHome}
          if ! test -f dendrite.yaml; then
             ${pkgs.dendrite}/bin/generate-config > dendrite.yaml
             sed -e 's|/var/log/dendrite|${dendriteHome}/logs|' -i dendrite.yaml
          fi
          if ! test -f matrix_key.pem; then
             ${pkgs.dendrite}/bin/generate-keys -private-key matrix_key.pem -tls-cert test.crt -tls-key test.key
          fi
          ${pkgs.dendrite}/bin/dendrite-monolith-server
        '';

        # A script to setup test environment
        dendriteSetup = pkgs.writeScriptBin "dendrite-setup" ''
          #!/bin/sh -e
          cd ${dendriteHome}
          HOMESERVER_URL=http://localhost:8008
          create_token () {
            ${pkgs.dendrite}/bin/create-account -username $1 -password $2 || true
            ${pkgs.curl}/bin/curl -XPOST $HOMESERVER_URL/_matrix/client/r0/login -d '{"user": "'$1'", "password": "'$2'", "type": "m.login.password"}' | ${pkgs.jq}/bin/jq -r ".access_token"
          }
          echo HOMESERVER_URL=$HOMESERVER_URL
          echo PRIMARY_TOKEN=$(create_token "test-user" "test-pass")
          echo SECONDARY_TOKEN=$(create_token "other-user" "test-pass")
        '';

        testScript = pkgs.writeScriptBin "matrix-client-test" ''
          #!/bin/sh -ex
          # running doctest in the haskellPackages.shellFor environment seems to be
          # more reliable
          doctest ./src/ -XOverloadedStrings
          hlint .
          cabal build
          cabal test
        '';

      in rec {
        packages = with pkgs.myHaskellPackages; { inherit matrix-client; };
        defaultPackage = packages.matrix-client;
        devShell = pkgs.myHaskellPackages.shellFor {
          packages = p: [ p.matrix-client ];

          buildInputs = with pkgs.myHaskellPackages; [
            cabal-install
            doctest
            hlint
            pkgs.haskell-language-server
            testScript
            dendriteStart
            dendriteSetup
          ];

          withHoogle = false;
        };
      });
}
