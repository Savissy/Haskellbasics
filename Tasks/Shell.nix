{ 
  pkgs ? import (fetchTarball "https://github.com/NixOS/nixpkgs/archive/nixos-22.11.tar.gz") {}
}:

let
  plutus-apps = import (fetchGit {
    url = "https://github.com/input-output-hk/plutus-apps";
    ref = "main";
    rev = "7f53f18dfc788bf6aa929f47d840efa1247e11fd"; # Your specific commit
  }) {};

  haskell = pkgs.haskell-nix;

  plutus-project = haskell.haskellLib.selectProject {
    name = "plutus-apps";
    directory = plutus-apps;
  };

in plutus-project.shellFor {
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
}
