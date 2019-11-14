{ sources ? import ./sources.nix }:
let
  haskellNixArgs = import sources."haskell.nix";

  pinnedHackageOverlay = self: super: {
    haskell-nix = super.haskell-nix // {
      hackageSourceJSON = ./hackage-src.json;
      stackageSourceJSON = ./stackage-src.json;
    };
  };

  haskellNixArgsWithPinnedHackage = haskellNixArgs // { overlays = haskellNixArgs.overlays ++ [ pinnedHackageOverlay ]; };

in

import sources.nixpkgs haskellNixArgsWithPinnedHackage
