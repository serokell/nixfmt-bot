## Building & Development

The `nix-shell` contains all necessary tools for development, including cabal, ghc, and nix-tools.

### Building the executable

```sh
$ nix-shell
λ nix build -f default.nix nixfmt-bot.components.exes.nixfmt-bot
```

Binary will be in `result/bin/nixfmt-bot`.

### Updating `pkgs.nix`

This file holds the Nix version of the Cabal dependency tree. It needs to be
updated any time you update module dependencies.

```sh
$ nix-shell
λ ./cabal-hpack.sh new-configure
λ plan-to-nix --output . \
    --plan-json dist-newstyle/cache/plan.json \
    --cabal-project cabal.project
```

### Updating the Hackage snapshot

This snapshot defines which Haskell packages are available to us, including
available versions. If you find nix complaining about `missing attribute 0.3.1`
or similar, this is probably why.

```sh
$ cd nix
$ nix-prefetch-git https://github.com/input-output-hk/hackage.nix | tee hackage-src.json
```

### Updating nix packages

Currently pinned packages are:

* haskell.nix -- The build system
* nixpkgs -- The Nix package set

```sh
$ niv update
```
