{
  description = "Plutus development environment";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-22.11";
    haskellNix.url = "github:input-output-hk/haskell.nix";
    iohk-nix.url = "github:input-output-hk/iohk-nix";
    flake-utils.url = "github:numtide/flake-utils";
    plutus-apps.url = "github:input-output-hk/plutus-apps";
  };

  outputs = { self, nixpkgs, haskellNix, iohk-nix, flake-utils, plutus-apps }:
    flake-utils.lib.eachSystem [ "x86_64-linux" "x86_64-darwin" ] (system:
      let
        overlays = [
          haskellNix.overlay
          iohk-nix.overlays.crypto
          (final: prev: {
            plutus-apps = plutus-apps.packages.${system};
          })
        ];

        pkgs = import nixpkgs {
          inherit system overlays;
          inherit (haskellNix) config;
        };

        haskell = pkgs.haskell-nix;

        plutus-project = haskell.haskellLib.selectProject {
          name = "plutus-apps";
          directory = plutus-apps;
        };

        devShell = plutus-project.shellFor {
          packages = ps: with ps; [
            plutus-ledger
            plutus-contract
            plutus-playground-server
            plutus-pab
          ];
          
          buildInputs = with pkgs; [
            haskell-language-server
            haskellPackages.cabal-install
            haskellPackages.ghc
            haskellPackages.hlint
            haskellPackages.fourmolu
            nixpkgs-fmt
            cabal2nix
            pkg-config
            zlib
            libsodium
            secp256k1
          ];

          shellHook = ''
            echo "Entering Plutus development environment"
            export NIX_ENFORCE_PURITY=0
            export LD_LIBRARY_PATH=${pkgs.libsodium}/lib:${pkgs.zlib}/lib:${pkgs.secp256k1}/lib:$LD_LIBRARY_PATH
            export PKG_CONFIG_PATH=${pkgs.libsodium}/lib/pkgconfig:${pkgs.zlib}/lib/pkgconfig:${pkgs.secp256k1}/lib/pkgconfig:$PKG_CONFIG_PATH
          '';
        };
      in {
        devShells.default = devShell;
        packages.default = devShell;
      });
}
