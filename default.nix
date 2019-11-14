{ pkgs ? import ./nix {} }:

let
  pkgSet = pkgs.haskell-nix.mkStackPkgSet {
    stack-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };

in
  pkgSet.config.hsPkgs
