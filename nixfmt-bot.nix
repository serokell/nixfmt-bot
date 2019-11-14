let
  buildDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (build dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  sysDepError = pkg:
    builtins.throw ''
      The Nixpkgs package set does not contain the package: ${pkg} (system dependency).
      
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      '';
  pkgConfDepError = pkg:
    builtins.throw ''
      The pkg-conf packages does not contain the package: ${pkg} (pkg-conf dependency).
      
      You may need to augment the pkg-conf package mapping in haskell.nix so that it can be found.
      '';
  exeDepError = pkg:
    builtins.throw ''
      The local executable components do not include the component: ${pkg} (executable dependency).
      '';
  legacyExeDepError = pkg:
    builtins.throw ''
      The Haskell package set does not contain the package: ${pkg} (executable dependency).
      
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
  buildToolDepError = pkg:
    builtins.throw ''
      Neither the Haskell package set or the Nixpkgs package set contain the package: ${pkg} (build tool dependency).
      
      If this is a system dependency:
      You may need to augment the system package mapping in haskell.nix so that it can be found.
      
      If this is a Haskell dependency:
      If you are using Stackage, make sure that you are using a snapshot that contains the package. Otherwise you may need to update the Hackage snapshot you are using, usually by updating haskell.nix.
      '';
in { system, compiler, flags, pkgs, hsPkgs, pkgconfPkgs, ... }:
  ({
    flags = {};
    package = {
      specVersion = "0";
      identifier = { name = "nixfmt-bot"; version = "0.1.0.0"; };
      license = "MPL-2.0";
      copyright = "2019 Serokell, 2019 Ilya Peresadin";
      maintainer = "pva@serokell.io";
      author = "Ilya Peresadin";
      homepage = "https://github.com/serokell/nixfmt-bot";
      url = "";
      synopsis = "Nixfmt bot";
      description = "Nixfmt bot formats all your code in PR where you summoned him.";
      buildType = "Simple";
      isLocal = true;
      };
    components = {
      "library" = {
        depends = [
          (hsPkgs."aeson-options" or (buildDepError "aeson-options"))
          (hsPkgs."async" or (buildDepError "async"))
          (hsPkgs."base-noprelude" or (buildDepError "base-noprelude"))
          (hsPkgs."bytestring" or (buildDepError "bytestring"))
          (hsPkgs."directory" or (buildDepError "directory"))
          (hsPkgs."github" or (buildDepError "github"))
          (hsPkgs."github-webhooks" or (buildDepError "github-webhooks"))
          (hsPkgs."hpack" or (buildDepError "hpack"))
          (hsPkgs."http-client" or (buildDepError "http-client"))
          (hsPkgs."http-client-tls" or (buildDepError "http-client-tls"))
          (hsPkgs."http-types" or (buildDepError "http-types"))
          (hsPkgs."loot-prelude" or (buildDepError "loot-prelude"))
          (hsPkgs."megaparsec" or (buildDepError "megaparsec"))
          (hsPkgs."microlens-platform" or (buildDepError "microlens-platform"))
          (hsPkgs."mtl" or (buildDepError "mtl"))
          (hsPkgs."servant" or (buildDepError "servant"))
          (hsPkgs."servant-github-webhook" or (buildDepError "servant-github-webhook"))
          (hsPkgs."servant-server" or (buildDepError "servant-server"))
          (hsPkgs."shelly" or (buildDepError "shelly"))
          (hsPkgs."text" or (buildDepError "text"))
          (hsPkgs."time" or (buildDepError "time"))
          (hsPkgs."universum" or (buildDepError "universum"))
          (hsPkgs."unliftio" or (buildDepError "unliftio"))
          (hsPkgs."wai" or (buildDepError "wai"))
          (hsPkgs."warp" or (buildDepError "warp"))
          (hsPkgs."yaml" or (buildDepError "yaml"))
          ];
        build-tools = [
          (hsPkgs.buildPackages.autoexporter or (pkgs.buildPackages.autoexporter or (buildToolDepError "autoexporter")))
          ];
        buildable = true;
        };
      exes = {
        "nixfmt-bot" = {
          depends = [
            (hsPkgs."aeson" or (buildDepError "aeson"))
            (hsPkgs."base-noprelude" or (buildDepError "base-noprelude"))
            (hsPkgs."bytestring" or (buildDepError "bytestring"))
            (hsPkgs."github" or (buildDepError "github"))
            (hsPkgs."loot-prelude" or (buildDepError "loot-prelude"))
            (hsPkgs."microlens-platform" or (buildDepError "microlens-platform"))
            (hsPkgs."nixfmt-bot" or (buildDepError "nixfmt-bot"))
            (hsPkgs."optparse-applicative" or (buildDepError "optparse-applicative"))
            (hsPkgs."universum" or (buildDepError "universum"))
            ];
          buildable = true;
          };
        };
      };
    } // rec { src = (pkgs.lib).mkDefault ./.; }) // {
    cabal-generator = "hpack";
    }