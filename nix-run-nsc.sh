#!/usr/bin/env bash

nix-shell -E 'import ./nsc.nix { pkgs = import <nixpkgs> {}; }' --run 'eval "$buildPhase"'
