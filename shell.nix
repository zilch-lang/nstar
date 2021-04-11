{ pkgs ? import <nixpkgs> {} }:

pkgs.mkShell {
  name = "nstar";
  version = "1.0.0";

  buildInputs = with pkgs; [
    stack
    
#    haskellPackages.haskell-language-server
    haskellPackages.hoogle

    haskellPackages.c2hs

    gcc
  ];
}
