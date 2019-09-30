{ pkgs ? import ./nix {} }: with pkgs;
let
  pkgSet = haskell-nix.mkCabalProjectPkgSet {
    plan-pkgs = import ./pkgs.nix;
    pkg-def-extras = [];
    modules = [];
  };
in

pkgSet.config.hsPkgs
