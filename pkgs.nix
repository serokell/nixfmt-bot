{
  extras = hackage:
    {
      packages = {
        "aeson-options" = (((hackage.aeson-options)."0.1.0").revisions).default;
        "github" = (((hackage.github)."0.23").revisions).default;
        "github-webhooks" = (((hackage.github-webhooks)."0.11.0").revisions).default;
        "servant-github-webhook" = (((hackage.servant-github-webhook)."0.4.2.0").revisions).default;
        "binary-instances" = (((hackage.binary-instances)."1").revisions).default;
        "shelly" = (((hackage.shelly)."1.9.0").revisions).default;
        nixfmt-bot = ./nixfmt-bot.nix;
        loot-base = ./loot-base.nix;
        loot-prelude = ./loot-prelude.nix;
        };
      };
  resolver = "lts-14.14";
  modules = [ ({ lib, ... }: { packages = {}; }) { packages = {}; } ];
  }