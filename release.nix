let
  pkgs = import <nixpkgs> { };
in
  { user-service = pkgs.haskellPackages.callPackage ./user-service.nix { };
  }
