{
  description = "The N* compiler";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };

        haskellPackages = pkgs.haskell.packages.ghc8107.override {
          overrides = new: old:
            let
              diagnose' = pkgs.haskell.lib.enableCabalFlag old.diagnose "megaparsec-compat";
              diagnose = pkgs.haskell.lib.overrideCabal diagnose' (drv: {
                buildDepends = [ old.megaparsec ];
              });
            in
            rec {
              inherit diagnose elfgen nsc-core nsc-codegen nsc-flags nsc-typechecker nsc-pretty nsc-parser;
            };
        };

        # jailbreakUnbreak = pkg:
        #   pkgs.haskell.lib.doJailbreak (pkg.overrideAttrs (_: { meta = { }; }));

        elfgen = pkgs.haskell.lib.overrideCabal (haskellPackages.callCabal2nix "elfgen" "${self}/lib/elfgen" { }) (drv: {
          buildDepends = [ haskellPackages.c2hs ];
        });
        nsc-codegen = haskellPackages.callCabal2nix "nsc-codegen" "${self}/lib/nsc-codegen" { };
        nsc-core = haskellPackages.callCabal2nix "nsc-core" "${self}/lib/nsc-core" { };
        nsc-flags = haskellPackages.callCabal2nix "nsc-flags" "${self}/lib/nsc-flags" { };
        nsc-typechecker = haskellPackages.callCabal2nix "nsc-typechecker" "${self}/lib/nsc-typechecker" { };
        nsc-parser = haskellPackages.callCabal2nix "nsc-parser" "${self}/lib/nsc-parser" { };
        nsc-pretty = haskellPackages.callCabal2nix "nsc-pretty" "${self}/lib/nsc-pretty" { };
        nsc = haskellPackages.callCabal2nix "nsc" self { };
      in
      {
        packages = {
          inherit elfgen nsc-codegen nsc-core nsc-flags nsc-typechecker nsc-parser nsc-pretty nsc;
        };

        apps = rec {
          nsc = flake-utils.lib.mkApp {
            drv = self.packages.${system}.nsc;
            name = "nsc";
          };
          default = nsc;
        };

        devShell = pkgs.mkShell {
          buildInputs = [
            haskellPackages.cabal-install
            haskellPackages.haskell-language-server
          ];
          inputsFrom = builtins.attrValues self.packages.${system};
        };
      });
}
