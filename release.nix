let
  config = {
    packageOverrides = pkgs: rec {
      docker-image = pkgs.dockerTools.buildImage {
        name = "carp-sushi/user-service";
        tag = "latest";
        config.Cmd = [ "${haskellPackages.user-service}/bin/user-service-exe" ];
      };
      haskellPackages = pkgs.haskellPackages.override {
        overrides = haskellPackagesNew: haskellPackgesOld: rec {
          user-service =
            pkgs.haskell.lib.overrideCabal
              ( pkgs.haskell.lib.justStaticExecutables
                  ( haskellPackagesNew.callPackage ./user-service.nix {}
                  )
              )
              ( oldDerivation: {}
              );
        };
      };
    };
  };
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs { inherit config; system = "x86_64-linux"; };
in
  rec {
    user-service = pkgs.haskellPackages.user-service;
    docker-image = pkgs.docker-image;
  }