{ sources ? import ./sources.nix }:
let
  compiler = "ghc924";

  # Create an override of nixpkgs that has the set of haskell packages/versions that we want
  # This follows the approach explained at https://github.com/Gabriella439/haskell-nix
  config = {
    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          forGren = pkgs.haskell.packages."${compiler}".override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              # Add gren to the set of available haskell packages
              gren = haskellPackagesNew.callPackage ./cabal2nix/gren.nix { };

              # Use text-2.x as the default version of text
              text = haskellPackagesOld.text_2_0_1;
            };
          };
        };
      };
    };
  };
  pkgs = import sources.nixpkgs { inherit config; };

  # Make the following available to default.nix and shell.nix
in {
  pkgs = pkgs;
  haskellPackages = pkgs.haskell.packages.forGren;

  # We need haskell-language-server to match the ghc version we are using,
  # but we can't use the one from the "forGren" environment we created
  # because something it depends on still requires text-1.x.
  haskell-language-server =
    pkgs.haskell.packages."${compiler}".haskell-language-server;
}
