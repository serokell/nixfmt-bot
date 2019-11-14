let
  nixpkgs = import ./nix {};

  sources = import ./nix/sources.nix;
  nixpkgs-old = import sources.nixpkgs-old {};
in

nixpkgs.mkShell {
  buildInputs = [
    # using nixpkgs 19.03 release because we need stack 1.x
    nixpkgs-old.stack

    nixpkgs.haskell-nix.nix-tools
  ];
}
