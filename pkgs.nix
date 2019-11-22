{
  pkgs = hackage:
    {
      packages = {
        "warp".revision = (((hackage."warp")."3.3.3").revisions).default;
        "warp".flags.allow-sendfilefd = true;
        "warp".flags.network-bytestring = false;
        "warp".flags.warp-debug = false;
        "http-client".revision = (((hackage."http-client")."0.6.4").revisions).default;
        "http-client".flags.network-uri = true;
        "cookie".revision = (((hackage."cookie")."0.4.4").revisions).default;
        "void".revision = (((hackage."void")."0.7.3").revisions).default;
        "void".flags.safe = false;
        "semigroupoids".revision = (((hackage."semigroupoids")."5.3.3").revisions).default;
        "semigroupoids".flags.comonad = true;
        "semigroupoids".flags.doctests = true;
        "semigroupoids".flags.unordered-containers = true;
        "semigroupoids".flags.distributive = true;
        "semigroupoids".flags.tagged = true;
        "semigroupoids".flags.containers = true;
        "semigroupoids".flags.contravariant = true;
        "byteorder".revision = (((hackage."byteorder")."1.0.4").revisions).default;
        "singleton-bool".revision = (((hackage."singleton-bool")."0.1.5").revisions).default;
        "free".revision = (((hackage."free")."5.1.2").revisions).default;
        "tf-random".revision = (((hackage."tf-random")."0.5").revisions).default;
        "cereal".revision = (((hackage."cereal")."0.5.8.1").revisions).default;
        "cereal".flags.bytestring-builder = false;
        "exceptions".revision = (((hackage."exceptions")."0.10.3").revisions).default;
        "microlens-platform".revision = (((hackage."microlens-platform")."0.4.0").revisions).default;
        "binary".revision = (((hackage."binary")."0.8.6.0").revisions).default;
        "microlens-ghc".revision = (((hackage."microlens-ghc")."0.4.11.1").revisions).default;
        "http-link-header".revision = (((hackage."http-link-header")."1.0.3.1").revisions).default;
        "attoparsec-iso8601".revision = (((hackage."attoparsec-iso8601")."1.0.1.0").revisions).default;
        "attoparsec-iso8601".flags.fast = false;
        "attoparsec-iso8601".flags.developer = false;
        "ghc-prim".revision = (((hackage."ghc-prim")."0.5.3").revisions).default;
        "text-metrics".revision = (((hackage."text-metrics")."0.3.0").revisions).default;
        "text-metrics".flags.dev = false;
        "utf8-string".revision = (((hackage."utf8-string")."1.0.1.1").revisions).default;
        "old-time".revision = (((hackage."old-time")."1.1.0.3").revisions).default;
        "bifunctors".revision = (((hackage."bifunctors")."5.5.5").revisions).default;
        "bifunctors".flags.semigroups = true;
        "bifunctors".flags.tagged = true;
        "x509-validation".revision = (((hackage."x509-validation")."1.6.11").revisions).default;
        "bytestring-conversion".revision = (((hackage."bytestring-conversion")."0.3.1").revisions).default;
        "split".revision = (((hackage."split")."0.2.3.3").revisions).default;
        "stm".revision = (((hackage."stm")."2.5.0.0").revisions).default;
        "dec".revision = (((hackage."dec")."0.0.3").revisions).default;
        "unix-time".revision = (((hackage."unix-time")."0.4.7").revisions).default;
        "http2".revision = (((hackage."http2")."2.0.3").revisions).default;
        "http2".flags.devel = false;
        "appar".revision = (((hackage."appar")."0.1.8").revisions).default;
        "hourglass".revision = (((hackage."hourglass")."0.2.12").revisions).default;
        "case-insensitive".revision = (((hackage."case-insensitive")."1.2.1.0").revisions).default;
        "network-byte-order".revision = (((hackage."network-byte-order")."0.1.1.1").revisions).default;
        "unliftio".revision = (((hackage."unliftio")."0.2.12").revisions).default;
        "unix".revision = (((hackage."unix")."2.7.2.2").revisions).default;
        "mtl".revision = (((hackage."mtl")."2.2.2").revisions).default;
        "lifted-async".revision = (((hackage."lifted-async")."0.10.0.4").revisions).default;
        "network-uri".revision = (((hackage."network-uri")."2.6.1.0").revisions).default;
        "asn1-parse".revision = (((hackage."asn1-parse")."0.9.5").revisions).default;
        "zlib".revision = (((hackage."zlib")."0.6.2.1").revisions).default;
        "zlib".flags.non-blocking-ffi = false;
        "zlib".flags.pkg-config = false;
        "rts".revision = (((hackage."rts")."1.0").revisions).default;
        "mmorph".revision = (((hackage."mmorph")."1.1.3").revisions).default;
        "easy-file".revision = (((hackage."easy-file")."0.2.2").revisions).default;
        "cryptonite".revision = (((hackage."cryptonite")."0.26").revisions).default;
        "cryptonite".flags.support_sse = false;
        "cryptonite".flags.integer-gmp = true;
        "cryptonite".flags.support_rdrand = true;
        "cryptonite".flags.support_aesni = true;
        "cryptonite".flags.support_deepseq = true;
        "cryptonite".flags.support_pclmuldq = false;
        "cryptonite".flags.check_alignment = false;
        "cryptonite".flags.old_toolchain_inliner = false;
        "clock".revision = (((hackage."clock")."0.8").revisions).default;
        "clock".flags.llvm = false;
        "double-conversion".revision = (((hackage."double-conversion")."2.0.2.0").revisions).default;
        "double-conversion".flags.developer = false;
        "safe-exceptions".revision = (((hackage."safe-exceptions")."0.1.7.0").revisions).default;
        "adjunctions".revision = (((hackage."adjunctions")."4.4").revisions).default;
        "invariant".revision = (((hackage."invariant")."0.5.3").revisions).default;
        "enclosed-exceptions".revision = (((hackage."enclosed-exceptions")."1.0.3").revisions).default;
        "pem".revision = (((hackage."pem")."0.2.4").revisions).default;
        "http-api-data".revision = (((hackage."http-api-data")."0.4.1").revisions).default;
        "http-api-data".flags.use-text-show = false;
        "megaparsec".revision = (((hackage."megaparsec")."7.0.5").revisions).default;
        "megaparsec".flags.dev = false;
        "universum".revision = (((hackage."universum")."1.6.0").revisions).default;
        "distributive".revision = (((hackage."distributive")."0.6.1").revisions).default;
        "distributive".flags.semigroups = true;
        "distributive".flags.tagged = true;
        "asn1-encoding".revision = (((hackage."asn1-encoding")."0.9.6").revisions).default;
        "binary-orphans".revision = (((hackage."binary-orphans")."1.0.1").revisions).default;
        "QuickCheck".revision = (((hackage."QuickCheck")."2.13.2").revisions).default;
        "QuickCheck".flags.templatehaskell = true;
        "scientific".revision = (((hackage."scientific")."0.3.6.2").revisions).default;
        "scientific".flags.integer-simple = false;
        "scientific".flags.bytestring-builder = false;
        "time-manager".revision = (((hackage."time-manager")."0.0.0").revisions).default;
        "hspec-discover".revision = (((hackage."hspec-discover")."2.7.1").revisions).default;
        "deepseq".revision = (((hackage."deepseq")."1.4.4.0").revisions).default;
        "errors".revision = (((hackage."errors")."2.3.0").revisions).default;
        "random".revision = (((hackage."random")."1.1").revisions).default;
        "uuid-types".revision = (((hackage."uuid-types")."1.0.3").revisions).default;
        "string-conversions".revision = (((hackage."string-conversions")."0.4.0.1").revisions).default;
        "optparse-applicative".revision = (((hackage."optparse-applicative")."0.15.1.0").revisions).default;
        "network".revision = (((hackage."network")."3.1.1.0").revisions).default;
        "word8".revision = (((hackage."word8")."0.1.3").revisions).default;
        "base-noprelude".revision = (((hackage."base-noprelude")."4.12.0.0").revisions).default;
        "connection".revision = (((hackage."connection")."0.3.1").revisions).default;
        "splitmix".revision = (((hackage."splitmix")."0.0.3").revisions).default;
        "splitmix".flags.optimised-mixer = false;
        "splitmix".flags.random = true;
        "async".revision = (((hackage."async")."2.2.2").revisions).default;
        "async".flags.bench = false;
        "dlist".revision = (((hackage."dlist")."0.8.0.7").revisions).default;
        "conduit".revision = (((hackage."conduit")."1.3.1.1").revisions).default;
        "x509-store".revision = (((hackage."x509-store")."1.6.7").revisions).default;
        "constraints".revision = (((hackage."constraints")."0.11.2").revisions).default;
        "semigroups".revision = (((hackage."semigroups")."0.19.1").revisions).default;
        "semigroups".flags.bytestring = true;
        "semigroups".flags.unordered-containers = true;
        "semigroups".flags.text = true;
        "semigroups".flags.tagged = true;
        "semigroups".flags.containers = true;
        "semigroups".flags.binary = true;
        "semigroups".flags.hashable = true;
        "semigroups".flags.transformers = true;
        "semigroups".flags.deepseq = true;
        "semigroups".flags.bytestring-builder = false;
        "semigroups".flags.template-haskell = true;
        "HUnit".revision = (((hackage."HUnit")."1.6.0.0").revisions).default;
        "vector-instances".revision = (((hackage."vector-instances")."3.4").revisions).default;
        "vector-instances".flags.hashable = true;
        "lifted-base".revision = (((hackage."lifted-base")."0.2.3.12").revisions).default;
        "parsec".revision = (((hackage."parsec")."3.1.13.0").revisions).default;
        "http-media".revision = (((hackage."http-media")."0.8.0.0").revisions).default;
        "hsc2hs".revision = (((hackage."hsc2hs")."0.68.6").revisions).default;
        "hsc2hs".flags.in-ghc-tree = false;
        "directory".revision = (((hackage."directory")."1.3.3.0").revisions).default;
        "yaml".revision = (((hackage."yaml")."0.11.1.2").revisions).default;
        "yaml".flags.no-exe = true;
        "yaml".flags.no-examples = true;
        "transformers-compat".revision = (((hackage."transformers-compat")."0.6.5").revisions).default;
        "transformers-compat".flags.five = false;
        "transformers-compat".flags.generic-deriving = true;
        "transformers-compat".flags.two = false;
        "transformers-compat".flags.five-three = true;
        "transformers-compat".flags.mtl = true;
        "transformers-compat".flags.four = false;
        "transformers-compat".flags.three = false;
        "hpack".revision = (((hackage."hpack")."0.33.0").revisions).default;
        "template-haskell".revision = (((hackage."template-haskell")."2.14.0.0").revisions).default;
        "hspec-expectations".revision = (((hackage."hspec-expectations")."0.8.2").revisions).default;
        "mono-traversable".revision = (((hackage."mono-traversable")."1.0.13.0").revisions).default;
        "psqueues".revision = (((hackage."psqueues")."0.2.7.2").revisions).default;
        "vector".revision = (((hackage."vector")."0.12.0.3").revisions).default;
        "vector".flags.unsafechecks = false;
        "vector".flags.internalchecks = false;
        "vector".flags.wall = false;
        "vector".flags.boundschecks = true;
        "call-stack".revision = (((hackage."call-stack")."0.2.0").revisions).default;
        "primitive".revision = (((hackage."primitive")."0.7.0.0").revisions).default;
        "profunctors".revision = (((hackage."profunctors")."5.5").revisions).default;
        "safe".revision = (((hackage."safe")."0.3.17").revisions).default;
        "blaze-builder".revision = (((hackage."blaze-builder")."0.4.1.0").revisions).default;
        "base-compat".revision = (((hackage."base-compat")."0.11.0").revisions).default;
        "autoexporter".revision = (((hackage."autoexporter")."1.1.14").revisions).default;
        "time-compat".revision = (((hackage."time-compat")."1.9.2.2").revisions).default;
        "time-compat".flags.old-locale = false;
        "deepseq-generics".revision = (((hackage."deepseq-generics")."0.2.0.0").revisions).default;
        "x509-system".revision = (((hackage."x509-system")."1.6.6").revisions).default;
        "keys".revision = (((hackage."keys")."3.12.2").revisions).default;
        "ansi-terminal".revision = (((hackage."ansi-terminal")."0.10.1").revisions).default;
        "ansi-terminal".flags.example = false;
        "vector-binary-instances".revision = (((hackage."vector-binary-instances")."0.2.5.1").revisions).default;
        "tagged".revision = (((hackage."tagged")."0.8.6").revisions).default;
        "tagged".flags.transformers = true;
        "tagged".flags.deepseq = true;
        "x509".revision = (((hackage."x509")."1.7.5").revisions).default;
        "unliftio-core".revision = (((hackage."unliftio-core")."0.1.2.0").revisions).default;
        "containers".revision = (((hackage."containers")."0.6.0.1").revisions).default;
        "integer-logarithms".revision = (((hackage."integer-logarithms")."1.0.3").revisions).default;
        "integer-logarithms".flags.check-bounds = false;
        "integer-logarithms".flags.integer-gmp = true;
        "socks".revision = (((hackage."socks")."0.6.1").revisions).default;
        "servant-github-webhook".revision = (((hackage."servant-github-webhook")."0.4.2.0").revisions).default;
        "streaming-commons".revision = (((hackage."streaming-commons")."0.2.1.1").revisions).default;
        "streaming-commons".flags.use-bytestring-builder = false;
        "text-binary".revision = (((hackage."text-binary")."0.2.1.1").revisions).default;
        "shelly".revision = (((hackage."shelly")."1.9.0").revisions).default;
        "shelly".flags.build-examples = false;
        "shelly".flags.lifted = false;
        "bytestring".revision = (((hackage."bytestring")."0.10.8.2").revisions).default;
        "ansi-wl-pprint".revision = (((hackage."ansi-wl-pprint")."0.6.9").revisions).default;
        "ansi-wl-pprint".flags.example = false;
        "microlens-th".revision = (((hackage."microlens-th")."0.4.3.2").revisions).default;
        "wai".revision = (((hackage."wai")."3.2.2.1").revisions).default;
        "basement".revision = (((hackage."basement")."0.0.11").revisions).default;
        "setenv".revision = (((hackage."setenv")."0.1.1.3").revisions).default;
        "cryptohash-sha1".revision = (((hackage."cryptohash-sha1")."0.11.100.1").revisions).default;
        "old-locale".revision = (((hackage."old-locale")."1.0.0.7").revisions).default;
        "StateVar".revision = (((hackage."StateVar")."1.2").revisions).default;
        "vault".revision = (((hackage."vault")."0.3.1.3").revisions).default;
        "vault".flags.useghc = true;
        "mime-types".revision = (((hackage."mime-types")."0.1.0.9").revisions).default;
        "http-client-tls".revision = (((hackage."http-client-tls")."0.3.5.3").revisions).default;
        "contravariant".revision = (((hackage."contravariant")."1.5.2").revisions).default;
        "contravariant".flags.semigroups = true;
        "contravariant".flags.tagged = true;
        "contravariant".flags.statevar = true;
        "iso8601-time".revision = (((hackage."iso8601-time")."0.1.5").revisions).default;
        "iso8601-time".flags.new-time = true;
        "type-equality".revision = (((hackage."type-equality")."1").revisions).default;
        "pointed".revision = (((hackage."pointed")."5.0.1").revisions).default;
        "pointed".flags.semigroupoids = true;
        "pointed".flags.stm = true;
        "pointed".flags.comonad = true;
        "pointed".flags.unordered-containers = true;
        "pointed".flags.kan-extensions = true;
        "pointed".flags.semigroups = true;
        "pointed".flags.tagged = true;
        "pointed".flags.containers = true;
        "pointed".flags.transformers = true;
        "parser-combinators".revision = (((hackage."parser-combinators")."1.2.0").revisions).default;
        "parser-combinators".flags.dev = false;
        "blaze-markup".revision = (((hackage."blaze-markup")."0.8.2.3").revisions).default;
        "text".revision = (((hackage."text")."1.2.3.1").revisions).default;
        "Cabal".revision = (((hackage."Cabal")."2.4.0.1").revisions).default;
        "unordered-containers".revision = (((hackage."unordered-containers")."0.2.10.0").revisions).default;
        "unordered-containers".flags.debug = false;
        "base64-bytestring".revision = (((hackage."base64-bytestring")."1.0.0.2").revisions).default;
        "base".revision = (((hackage."base")."4.12.0.0").revisions).default;
        "comonad".revision = (((hackage."comonad")."5.0.5").revisions).default;
        "comonad".flags.distributive = true;
        "comonad".flags.test-doctests = true;
        "comonad".flags.containers = true;
        "hspec".revision = (((hackage."hspec")."2.7.1").revisions).default;
        "time".revision = (((hackage."time")."1.8.0.2").revisions).default;
        "data-default-class".revision = (((hackage."data-default-class")."0.1.2.0").revisions).default;
        "base16-bytestring".revision = (((hackage."base16-bytestring")."0.1.1.6").revisions).default;
        "vector-algorithms".revision = (((hackage."vector-algorithms")."0.8.0.1").revisions).default;
        "vector-algorithms".flags.unsafechecks = false;
        "vector-algorithms".flags.internalchecks = false;
        "vector-algorithms".flags.llvm = false;
        "vector-algorithms".flags.boundschecks = true;
        "vector-algorithms".flags.bench = true;
        "vector-algorithms".flags.properties = true;
        "iproute".revision = (((hackage."iproute")."1.7.7").revisions).default;
        "transformers".revision = (((hackage."transformers")."0.5.6.2").revisions).default;
        "hashable".revision = (((hackage."hashable")."1.3.0.0").revisions).default;
        "hashable".flags.sse2 = true;
        "hashable".flags.integer-gmp = true;
        "hashable".flags.sse41 = false;
        "hashable".flags.examples = false;
        "quickcheck-io".revision = (((hackage."quickcheck-io")."0.2.0").revisions).default;
        "wai-extra".revision = (((hackage."wai-extra")."3.0.28").revisions).default;
        "wai-extra".flags.build-example = false;
        "attoparsec".revision = (((hackage."attoparsec")."0.13.2.3").revisions).default;
        "attoparsec".flags.developer = false;
        "blaze-html".revision = (((hackage."blaze-html")."0.9.1.2").revisions).default;
        "infer-license".revision = (((hackage."infer-license")."0.2.0").revisions).default;
        "binary-instances".revision = (((hackage."binary-instances")."1").revisions).default;
        "colour".revision = (((hackage."colour")."2.3.5").revisions).default;
        "transformers-base".revision = (((hackage."transformers-base")."0.4.5.2").revisions).default;
        "transformers-base".flags.orphaninstances = true;
        "file-embed".revision = (((hackage."file-embed")."0.0.11").revisions).default;
        "github-webhooks".revision = (((hackage."github-webhooks")."0.11.0").revisions).default;
        "github-webhooks".flags.ci = false;
        "filepath".revision = (((hackage."filepath")."1.4.2.1").revisions).default;
        "auto-update".revision = (((hackage."auto-update")."0.1.6").revisions).default;
        "asn1-types".revision = (((hackage."asn1-types")."0.3.3").revisions).default;
        "aeson-options".revision = (((hackage."aeson-options")."0.1.0").revisions).default;
        "hspec-core".revision = (((hackage."hspec-core")."2.7.1").revisions).default;
        "unix-compat".revision = (((hackage."unix-compat")."0.5.2").revisions).default;
        "unix-compat".flags.old-time = false;
        "monad-control".revision = (((hackage."monad-control")."1.0.2.3").revisions).default;
        "process".revision = (((hackage."process")."1.6.5.0").revisions).default;
        "tls".revision = (((hackage."tls")."1.5.2").revisions).default;
        "tls".flags.compat = true;
        "tls".flags.network = true;
        "tls".flags.hans = false;
        "kan-extensions".revision = (((hackage."kan-extensions")."5.2").revisions).default;
        "wai-logger".revision = (((hackage."wai-logger")."2.3.6").revisions).default;
        "microlens-mtl".revision = (((hackage."microlens-mtl")."0.2.0.1").revisions).default;
        "libyaml".revision = (((hackage."libyaml")."0.1.1.0").revisions).default;
        "libyaml".flags.system-libyaml = false;
        "libyaml".flags.no-unicode = false;
        "resourcet".revision = (((hackage."resourcet")."1.2.2").revisions).default;
        "pretty".revision = (((hackage."pretty")."1.1.3.6").revisions).default;
        "cabal-doctest".revision = (((hackage."cabal-doctest")."1.0.8").revisions).default;
        "Glob".revision = (((hackage."Glob")."0.10.0").revisions).default;
        "microlens".revision = (((hackage."microlens")."0.4.11.2").revisions).default;
        "aeson".revision = (((hackage."aeson")."1.4.5.0").revisions).default;
        "aeson".flags.cffi = false;
        "aeson".flags.fast = false;
        "aeson".flags.bytestring-builder = false;
        "aeson".flags.developer = false;
        "wai-app-static".revision = (((hackage."wai-app-static")."3.1.6.3").revisions).default;
        "wai-app-static".flags.print = false;
        "http-types".revision = (((hackage."http-types")."0.12.3").revisions).default;
        "ghc-boot-th".revision = (((hackage."ghc-boot-th")."8.6.5").revisions).default;
        "servant-server".revision = (((hackage."servant-server")."0.16.2").revisions).default;
        "base-orphans".revision = (((hackage."base-orphans")."0.8.1").revisions).default;
        "http-date".revision = (((hackage."http-date")."0.0.8").revisions).default;
        "servant".revision = (((hackage."servant")."0.16.2").revisions).default;
        "th-abstraction".revision = (((hackage."th-abstraction")."0.3.1.0").revisions).default;
        "memory".revision = (((hackage."memory")."0.15.0").revisions).default;
        "memory".flags.support_bytestring = true;
        "memory".flags.support_basement = true;
        "memory".flags.support_foundation = true;
        "memory".flags.support_deepseq = true;
        "fast-logger".revision = (((hackage."fast-logger")."3.0.0").revisions).default;
        "bsb-http-chunked".revision = (((hackage."bsb-http-chunked")."0.0.0.4").revisions).default;
        "array".revision = (((hackage."array")."0.5.3.0").revisions).default;
        "simple-sendfile".revision = (((hackage."simple-sendfile")."0.2.30").revisions).default;
        "simple-sendfile".flags.allow-bsd = true;
        "integer-gmp".revision = (((hackage."integer-gmp")."1.0.2.0").revisions).default;
        };
      compiler = {
        version = "8.6.5";
        nix-name = "ghc865";
        packages = {
          "binary" = "0.8.6.0";
          "ghc-prim" = "0.5.3";
          "stm" = "2.5.0.0";
          "unix" = "2.7.2.2";
          "mtl" = "2.2.2";
          "rts" = "1.0";
          "deepseq" = "1.4.4.0";
          "parsec" = "3.1.13.0";
          "directory" = "1.3.3.0";
          "template-haskell" = "2.14.0.0";
          "containers" = "0.6.0.1";
          "bytestring" = "0.10.8.2";
          "text" = "1.2.3.1";
          "Cabal" = "2.4.0.1";
          "base" = "4.12.0.0";
          "time" = "1.8.0.2";
          "transformers" = "0.5.6.2";
          "filepath" = "1.4.2.1";
          "process" = "1.6.5.0";
          "pretty" = "1.1.3.6";
          "ghc-boot-th" = "8.6.5";
          "array" = "0.5.3.0";
          "integer-gmp" = "1.0.2.0";
          };
        };
      };
  extras = hackage:
    {
      packages = {
        nixfmt-bot = ./.plan.nix/nixfmt-bot.nix;
        github = ./.plan.nix/github.nix;
        };
      };
  modules = [
    ({ lib, ... }:
      {
        packages = {
          "nixfmt-bot" = { flags = {}; };
          "github" = { flags = { "openssl" = lib.mkOverride 900 false; }; };
          };
        })
    ];
  }