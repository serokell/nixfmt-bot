{ pkgs ? import ./nix {} }: with pkgs;
haskell-nix.cabalProject {
  src = haskell-nix.haskellLib.cleanGit { src = ./.; };
  ghc = buildPackages.pkgs.haskell.compiler.ghc865;
}
