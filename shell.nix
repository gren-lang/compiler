{ config ? import ./nix/config.nix { } }:
let
  pkgs = config.pkgs;
  haskellPackages = config.haskellPackages;
in haskellPackages.shellFor {
  packages = p: [ p.gren ];
  buildInputs = with pkgs; [
    # Haskell dev tools
    cabal-install
    haskellPackages.ghcid
    config.haskell-language-server
    ormolu

    # nix dev tools
    cabal2nix
    niv
    nixfmt
  ];
}
