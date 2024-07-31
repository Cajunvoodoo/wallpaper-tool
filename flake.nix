{
  # FIXME: fix the nix substituter config
  # nixConfig = {
  #   extra-experimental-features = "nix-command flakes";
  #   substituters = "https://nix-cache.cajun.page/cs3700";
  #   trusted-public-keys =
  #     "cs3700:2kyrJPa9t720OwM02UEg9cMLxQXGjtsu4Sz7MB5Lc9Q=";
  #   netrc-file = ./netrc;
  # };

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
    nix-filter.url = "github:numtide/nix-filter";
    nix-pre-commit.url = "github:jmgilman/nix-pre-commit";
    cabal-gild = {
      url = "github:tfausak/cabal-gild";
      flake = false;
    };
  };

  outputs = inputs:
    let
      inherit (inputs.nixpkgs) lib;
      foreach = xs: f: with lib; foldr recursiveUpdate { } (
        if isList xs then map f xs
        else if isAttrs xs then mapAttrsToList f xs
        else error "foreach: expected list or attrset but got ${builtins.typeOf xs}"
      );
      # Define the source directory, ignoring files that should not require
      # a rebuild for the haskell project
      hsSrc = with lib.fileset; toSource {
        root = ./.;
        fileset =
          intersection
            (gitTracked ./.)
            (unions [
              (fileFilter (file: file.hasExt "hs") ./.)
              (fileFilter (file: file.hasExt "cabal") ./.)
              #(fileFilter (file: file.hasExt "md") ./.)
            ]);
      };
      pname = "cajun-kriegs-wallpaper";
    in
    foreach inputs.nixpkgs.legacyPackages (system: pkgs:
      let
        defaultGhc = builtins.replaceStrings ["-" "."] ["" ""] pkgs.haskellPackages.ghc.name;
        precommit-config = {
          repos = [{
            repo = "local";
            hooks = [
              {
                id = "nixpkgs-fmt";
                entry = "${pkgs.nixpkgs-fmt}/bin/nixpkgs-fmt";
                language = "system";
                files = "\\.nix";
              }
              {
                id = "fourmolu";
                entry = "${pkgs.haskellPackages.fourmolu}/bin/fourmolu --mode=inplace";
                language = "system";
                files = "\\.(hs)$";
              }
              {
                id = "cabal-gild";
                entry = "${pkgs.haskellPackages.cabal-gild}/bin/cabal-gild --io=${pname}.cabal";
                language = "system";
                always_run = true;
                files = "^$";
              }
            ];
          }];
        };
      in
      lib.recursiveUpdate
        {
          formatter.${system} = pkgs.nixpkgs-fmt;
          packages.${system}.default = inputs.self.packages.${system}."${pname}-${defaultGhc}";
          devShells.${system}.default = inputs.self.devShells.${system}.${defaultGhc};
        }
        (foreach (lib.filterAttrs (name: _: builtins.match "ghc[0-9]+" name != null) pkgs.haskell.packages)
          (ghcName: haskellPackages:
            let
              hp = haskellPackages.override {
                overrides = self: super:
                  #with pkgs.haskell.lib.compose;
                  {
                    cabal-gild = super.haskell.lib.doJailbreak (self.callCabal2nix "cabal-gild" "${inputs.cabal-gild}" { });
                    ${pname} = self.callCabal2nix pname hsSrc { };
                  };
              };
            in
            {
              packages.${system}."${pname}-${ghcName}" = hp.${pname};
              devShells.${system}.${ghcName} = pkgs.mkShell {
                nativeBuildInputs = with pkgs; [
                  cabal-install
                  ghc
                ];
              };
              # devShells.${system}.${ghcName} = hp.shellFor {
              #   packages = ps: [ ps.${pname} ];

              #   shellHook = (inputs.nix-pre-commit.lib.${system}.mkConfig {
              #     inherit pkgs;
              #     config = precommit-config;
              #   }).shellHook;

              #   nativeBuildInputs = with hp; [
              #     cabal-install
              #     fourmolu
              #     haskell-language-server
              #   ];
              # };
            }
          )
        ));
}
