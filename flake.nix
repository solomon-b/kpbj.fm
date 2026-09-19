{
  description = "kpbj.fm";

  inputs = {
    # This pin also fixes the Liquidsoap version, currently 2.3.3, which
    # nixos/streaming.nix deploys as pkgs.liquidsoap.
    #
    # Re-check services/liquidsoap/radio.liq when this moves. Liquidsoap makes
    # breaking language changes between minor releases, and the script only fails
    # at load time, so a bad bump takes the stream off the air rather than the
    # build. 2.4 already removes source.on_track, which radio.liq calls to log
    # track changes and POST them to /played.
    #
    #   nix eval --impure --expr \
    #     '(builtins.getFlake (builtins.toString ./.)).inputs.nixpkgs.legacyPackages.x86_64-linux.liquidsoap.version'
    #   $(nix build --print-out-paths nixpkgs#liquidsoap)/bin/liquidsoap \
    #     --check services/liquidsoap/radio.liq
    nixpkgs.url = github:NixOS/nixpkgs/nixos-25.11;
    flake-utils.url = github:numtide/flake-utils;

    web-server-core = {
      url = github:solomon-b/web-server;
    };

    sops-nix = {
      url = "github:Mic92/sops-nix";
      inputs.nixpkgs.follows = "nixpkgs";
    };

    friendly-ghost = {
      url = "github:solomon-b/friendly-ghost";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, web-server-core, sops-nix, friendly-ghost }:
    (flake-utils.lib.eachDefaultSystem
      (system:
        let
          pkgs = import nixpkgs {
            inherit system;
            overlays = [
              web-server-core.overlays.web-server-core
            ];
          };
          amazonkaSrc = pkgs.fetchgit {
            url = "https://github.com/brendanhay/amazonka.git";
            rev = "a7d699be1076e2aad05a1930ca3937ffea954ad8";
            sha256 = "sha256-cCRhHH/IgM7tPy8rXHTSRec1zxohO8NWxSVZEG1OjQw=";
          };
          hsPkgs = pkgs.haskellPackages.override {
            overrides = hfinal: hprev: {
              amazonka = pkgs.haskell.lib.dontCheck
                (hfinal.callCabal2nix "amazonka" (amazonkaSrc + "/lib/amazonka") { });

              amazonka-core = pkgs.haskell.lib.dontCheck
                (hfinal.callCabal2nix "amazonka-core" (amazonkaSrc + "/lib/amazonka-core") { });

              amazonka-s3 = pkgs.haskell.lib.dontCheck
                (hfinal.callCabal2nix "amazonka-s3" (amazonkaSrc + "/lib/services/amazonka-s3") { });

              amazonka-sso = pkgs.haskell.lib.dontCheck
                (hfinal.callCabal2nix "amazonka-sso" (amazonkaSrc + "/lib/services/amazonka-sso") { });

              amazonka-sts = pkgs.haskell.lib.dontCheck
                (hfinal.callCabal2nix "amazonka-sts" (amazonkaSrc + "/lib/services/amazonka-sts") { });

              kpbj-types = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "kpbj-types" ./lib/kpbj-types { });

              kpbj-database = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "kpbj-database" ./lib/kpbj-database { });

              kpbj-email = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "kpbj-email" ./lib/kpbj-email { });

              stripe-http = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "stripe-http" ./lib/stripe-http { });

              easypost-http = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "easypost-http" ./lib/easypost-http { });

              mailchimp-http = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "mailchimp-http" ./lib/mailchimp-http { });

              kpbj-web = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "kpbj-web" ./services/web { });

              sync-host-emails = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "sync-host-emails" ./jobs/sync-host-emails { });

              token-cleanup = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "token-cleanup" ./jobs/token-cleanup { });

              episode-check = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "episode-check" ./jobs/episode-check { });

              listener-snapshots = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "listener-snapshots" ./jobs/listener-snapshots { });

              ga-poller = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "ga-poller" ./jobs/ga-poller { });

              mailchimp-reconcile = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "mailchimp-reconcile" ./jobs/mailchimp-reconcile { });

              order-cleanup = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "order-cleanup" ./jobs/order-cleanup { });

              lucid-form-builder = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "lucid-form-builder" ./services/web/lib/lucid-form-builder { });

              lucid-htmx-alpine = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "lucid-htmx-alpine" ./services/web/lib/lucid-htmx-alpine { });

              lucid-tailwind = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "lucid-tailwind" ./services/web/lib/lucid-tailwind { });

              # rel8 1.7.0.0 for hasql 1.9 compatibility
              rel8 = pkgs.haskell.lib.dontCheck (hfinal.callHackageDirect
                {
                  pkg = "rel8";
                  ver = "1.7.0.0";
                  sha256 = "sha256-B6I+y19vWCc0xh6tzCKLzHo7TB+91E7aZHPepm23/jI=";
                }
                { });

              # Use tmp-postgres from master to match cabal.project pin
              tmp-postgres = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "tmp-postgres"
                (pkgs.fetchgit {
                  url = "https://github.com/jfischoff/tmp-postgres.git";
                  rev = "7f2467a6d6d5f6db7eed59919a6773fe006cf22b";
                  sha256 = "sha256-dE1OQN7I4Lxy6RBdLCvm75Z9D/Hu+9G4ejV2pEtvL1A=";
                })
                { });

              web-server-core = web-server-core.packages.${system}.web-server-core;

              # xmlhtml packages from web-server repo (same source as
              # web-server-core, but these packages aren't currently in the
              # flake output)
              xmlhtml-qq = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "xmlhtml-qq"
                (pkgs.fetchgit
                  {
                    url = "https://github.com/solomon-b/web-server";
                    rev = "c6df09aa3607452662b9f6d8713502c595df9511";
                    sha256 = "sha256-11whPVqlHH9u9qiNIx5Zi+4AP/eazxPlDyspJH+QMHY=";
                  } + "/xmlhtml-qq")
                { });

              xmlhtml-lens = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "xmlhtml-lens"
                (pkgs.fetchgit
                  {
                    url = "https://github.com/solomon-b/web-server";
                    rev = "c6df09aa3607452662b9f6d8713502c595df9511";
                    sha256 = "sha256-11whPVqlHH9u9qiNIx5Zi+4AP/eazxPlDyspJH+QMHY=";
                  } + "/xmlhtml-lens")
                { });
            };
          };
        in
        rec {
          devShell = hsPkgs.shellFor {
            packages = p: map pkgs.haskell.lib.doCheck [ p.kpbj-types p.kpbj-database p.kpbj-email p.kpbj-web p.sync-host-emails p.token-cleanup p.episode-check p.listener-snapshots p.ga-poller p.mailchimp-reconcile ];
            withHoogle = false;
            buildInputs = [
              pkgs.cabal-install
              hsPkgs.haskell-language-server
              hsPkgs.hlint
              hsPkgs.weeder
              pkgs.file
              pkgs.ffmpeg
              pkgs.jq
              pkgs.just
              pkgs.nixpkgs-fmt
              pkgs.ormolu
              pkgs.openssl
              pkgs.postgresql_17
              pkgs.rclone
              pkgs.shellcheck
              pkgs.sqlx-cli
              pkgs.google-cloud-sdk
              pkgs.sops
              pkgs.age
              pkgs.ssh-to-age
              pkgs.nixos-rebuild
              pkgs.playwright-test
              pkgs.sloc
              pkgs.pkg-config
            ];
          };

          formatter = pkgs.nixpkgs-fmt;
          packages = flake-utils.lib.flattenTree {
            kpbj-web = hsPkgs.kpbj-web;
            sync-host-emails = hsPkgs.sync-host-emails;
            token-cleanup = hsPkgs.token-cleanup;
            episode-check = hsPkgs.episode-check;
            listener-snapshots = hsPkgs.listener-snapshots;
            ga-poller = hsPkgs.ga-poller;
            mailchimp-reconcile = hsPkgs.mailchimp-reconcile;
            order-cleanup = hsPkgs.order-cleanup;
          };

          defaultPackage = packages.kpbj-web;

          # Every package above is built with dontCheck, so a deploy never waits
          # on the suites and a broken test never blocks a build. These run them.
          #
          # The database suites start their own Postgres through tmp-postgres, so
          # they need the server binaries and sqlx on PATH. They also need the
          # migrations, which live under services/web and are therefore outside
          # the source tree of kpbj-database, so MIGRATIONS_DIR names them.
          #
          # Profiling is off here. Nixpkgs turns it on by default, which compiles
          # every module a second time, and nothing reads the profiling libraries.
          # The packages and the devShell keep it, so profiling a build by hand
          # still works.
          checks =
            let
              runTests = pkg:
                pkgs.haskell.lib.overrideCabal
                  (pkgs.haskell.lib.doCheck
                    (pkgs.haskell.lib.disableLibraryProfiling pkg))
                  (old: {
                    testToolDepends = (old.testToolDepends or [ ]) ++ [ pkgs.postgresql_17 pkgs.sqlx-cli ];
                    preCheck = (old.preCheck or "") + ''
                      export MIGRATIONS_DIR=${./services/web/migrations}
                      export LC_ALL=C
                    '';
                  });
            in
            {
              kpbj-web = runTests hsPkgs.kpbj-web;
              kpbj-database = runTests hsPkgs.kpbj-database;
              stripe-http = runTests hsPkgs.stripe-http;
              easypost-http = runTests hsPkgs.easypost-http;
              mailchimp-http = runTests hsPkgs.mailchimp-http;
              sync-host-emails = runTests hsPkgs.sync-host-emails;
            };

          apps = {
            kpbj-web = {
              type = "app";
              program = "${self.packages.${system}.kpbj-web}/bin/kpbj-web";
            };

            default = self.apps.${system}.kpbj-web;
          };
        }))
    //
    {
      nixosConfigurations =
        let
          sync-host-emails = self.packages.x86_64-linux.sync-host-emails;
          token-cleanup = self.packages.x86_64-linux.token-cleanup;
          episode-check = self.packages.x86_64-linux.episode-check;
          listener-snapshots = self.packages.x86_64-linux.listener-snapshots;
          ga-poller = self.packages.x86_64-linux.ga-poller;
          mailchimp-reconcile = self.packages.x86_64-linux.mailchimp-reconcile;
          order-cleanup = self.packages.x86_64-linux.order-cleanup;
          kpbj-web = self.packages.x86_64-linux.kpbj-web;
        in
        {
          kpbj-prod = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit sync-host-emails token-cleanup episode-check listener-snapshots ga-poller mailchimp-reconcile order-cleanup kpbj-web; };
            modules = [
              sops-nix.nixosModules.sops
              friendly-ghost.nixosModules.default
              ./nixos/prod.nix
            ];
          };
          kpbj-staging = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit sync-host-emails token-cleanup episode-check listener-snapshots ga-poller mailchimp-reconcile order-cleanup kpbj-web; };
            modules = [
              sops-nix.nixosModules.sops
              friendly-ghost.nixosModules.default
              ./nixos/staging.nix
            ];
          };
          kpbj-stream-dev = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            modules = [
              sops-nix.nixosModules.sops
              ./nixos/dev.nix
            ];
          };
        };
    };
}
