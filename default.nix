let config = import ./nix/config.nix { };
in { gren = config.haskellPackages.gren; }
