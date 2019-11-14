## Building & Development

The `nix-shell` contains all necessary tools for development, including ghc.

### Building the executable

```sh
$ nix build -f default.nix nixfmt-bot.components.exes.nixfmt-bot
```

Binary will be in `result/bin/nixfmt-bot`.

### Updating `pkgs.nix`

This file holds the Nix version of the package dependency tree. It needs to be
updated any time you update module dependencies.

```sh
$ nix-shell tools.nix
λ stack-to-nix --output .
```

### Updating the Hackage and Stackage pins

This pins define which Haskell packages and Stackage snapshots are available
to us. If you find nix complaining about `missing attribute 0.3.1` or
similar, this is probably why.

```sh
$ cd nix
$ nix-prefetch-git https://github.com/input-output-hk/hackage.nix | tee hackage-src.json
$ nix-prefetch-git https://github.com/input-output-hk/stackage.nix | tee stackage-src.json
```

### Updating nix packages

Currently pinned packages are:

* haskell.nix -- The build system
* nixpkgs -- The Nix package set
* nixpkgs-old -- NixOS 19.03 release, we need it for the old version of stack

```sh
$ niv update
```
## Running 

The bot would try to read a few environment variables: 

$KEY -- secret key from the webhook configuration 

$PORT -- listening port 

$GITHUB_LOGIN -- login for github

$GIHUB_PASSWORD -- password 

The bot would react on the comments with "@nixfmt" text in any PR which is connected to an issue on the repository.
It would create new pull request to the head branch of the pull request where the bot was mentioned. 

One should configure webhooks to mention the bot about issue comments.
