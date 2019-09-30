{ pkgs ? import ./nix {} }: with pkgs;

let
  hsPkgs = import ./default.nix {};
in
  hsPkgs.nixfmt-bot.components.all
