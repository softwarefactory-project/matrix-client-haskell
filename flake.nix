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
          ];

          withHoogle = false;
        };
      });
}
