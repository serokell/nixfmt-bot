{ sources ? import ./sources.nix }:
let
  overlays = (import "${sources."haskell.nix"}/overlays")
    ++ [(import ./overlays.nix { inherit sources; })];

  config = import "${sources."haskell.nix"}/config.nix";

in
  import sources.nixpkgs { inherit overlays config; }
