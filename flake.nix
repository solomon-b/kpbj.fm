{
  description = "kpbj.fm";

  inputs = {
    # This pin also fixes the Liquidsoap version, currently 2.4.4, which
    # nixos/streaming.nix deploys as pkgs.liquidsoap.
    #
    # Re-check services/liquidsoap/radio.liq when this moves. Liquidsoap makes
    # breaking language changes between minor releases, and the script only fails
    # at load time, so a bad bump takes the stream off the air rather than the
    # build. For example, 2.4 made on_track require the synchronous argument.
    #
    #   nix eval --impure --expr \
    #     '(builtins.getFlake (builtins.toString ./.)).inputs.nixpkgs.legacyPackages.x86_64-linux.liquidsoap.version'
    #   $(nix build --print-out-paths nixpkgs#liquidsoap)/bin/liquidsoap \
    #     --check services/liquidsoap/radio.liq
    nixpkgs.url = github:NixOS/nixpkgs/nixos-26.05;
    flake-utils.url = github:numtide/flake-utils;

    web-server-core = {
      url = github:solomon-b/web-server;
      # web-server tracks a newer nixpkgs. Its package must use the GHC of
      # this flake, or the package database of the dev shell breaks.
      inputs.nixpkgs.follows = "nixpkgs";
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
          servantAuthRolesSrc = pkgs.fetchgit {
            url = "https://github.com/solomon-b/servant-auth-roles.git";
            rev = "9300751f126ff375f36b57ebf33fa4fb1fcbb820";
            sha256 = "sha256-YgIU5yXaO9AoF1QW7k/CQFjoLbRcUEOhCvFjvSskd1c=";
          };
          # The local packages build with their test suites on, so `nix build`
          # compiles each package once and runs its tests in the same build. The
          # deployed binary is then the one that passed the tests. A failing test
          # stops the build of that package and of every package that needs it.
          #
          # The database suites start their own Postgres through tmp-postgres, so
          # they need the server binaries and sqlx on PATH. They also need the
          # migrations, which live under services/web and are therefore outside
          # the source tree of kpbj-database, so MIGRATIONS_DIR names them.
          #
          # The locale is UTF-8. Hedgehog prints coverage bars with a non-ASCII
          # character, and an ASCII locale makes that write fail.
          #
          # Library profiling is off. Nixpkgs turns it on by default, which
          # compiles every module a second time, and nothing reads the profiling
          # libraries. The devShell builds the local packages through cabal, so
          # profiling a build by hand still works.
          localPackage = hfinal: name: src:
            pkgs.haskell.lib.overrideCabal
              (pkgs.haskell.lib.doCheck
                (pkgs.haskell.lib.disableLibraryProfiling
                  (hfinal.callCabal2nix name src { })))
              (old: {
                testToolDepends = (old.testToolDepends or [ ]) ++ [ pkgs.postgresql_17 pkgs.sqlx-cli ];
                preCheck = (old.preCheck or "") + ''
                  export MIGRATIONS_DIR=${./services/web/migrations}
                  export LC_ALL=C.UTF-8
                '';
              });
          hsPkgs = pkgs.haskellPackages.override {
            overrides = hfinal: hprev: {
              servant-auth-roles = pkgs.haskell.lib.dontCheck
                (hfinal.callCabal2nix "servant-auth-roles" servantAuthRolesSrc { });

              kpbj-types = localPackage hfinal "kpbj-types" ./lib/kpbj-types;

              kpbj-storage = localPackage hfinal "kpbj-storage" ./lib/kpbj-storage;

              kpbj-database = localPackage hfinal "kpbj-database" ./lib/kpbj-database;

              kpbj-email = localPackage hfinal "kpbj-email" ./lib/kpbj-email;

              kpbj-job = localPackage hfinal "kpbj-job" ./lib/kpbj-job;

              stripe-http = localPackage hfinal "stripe-http" ./lib/stripe-http;

              easypost-http = localPackage hfinal "easypost-http" ./lib/easypost-http;

              mailchimp-http = localPackage hfinal "mailchimp-http" ./lib/mailchimp-http;

              google-service-account = localPackage hfinal "google-service-account" ./lib/google-service-account;

              # The audio upload runs ffprobe to measure each file and refuses one
              # it cannot measure. The wrapper puts ffprobe on the binary's PATH,
              # so the server finds it wherever it runs: the NixOS service, CI,
              # or a local `nix build`.
              #
              # justStaticExecutables removes the library and its docs from the
              # output. With them, the closure held GHC and every Haskell
              # library (1.3 GiB), and CI downloaded it for the E2E job. No
              # package depends on the kpbj-web library, and the test suite
              # still runs in this build.
              kpbj-web = pkgs.haskell.lib.justStaticExecutables
                (pkgs.haskell.lib.overrideCabal
                  (localPackage hfinal "kpbj-web" ./services/web)
                  (old: {
                    buildTools = (old.buildTools or [ ]) ++ [ pkgs.makeWrapper ];
                    postInstall = (old.postInstall or "") + ''
                      wrapProgram $out/bin/kpbj-web --prefix PATH : ${pkgs.lib.makeBinPath [ pkgs.ffmpeg ]}
                    '';
                  }));

              sync-host-emails = localPackage hfinal "sync-host-emails" ./jobs/sync-host-emails;

              sync-schedule-calendar = localPackage hfinal "sync-schedule-calendar" ./jobs/sync-schedule-calendar;

              token-cleanup = localPackage hfinal "token-cleanup" ./jobs/token-cleanup;

              episode-check = localPackage hfinal "episode-check" ./jobs/episode-check;

              listener-snapshots = localPackage hfinal "listener-snapshots" ./jobs/listener-snapshots;

              ga-poller = localPackage hfinal "ga-poller" ./jobs/ga-poller;

              mailchimp-reconcile = localPackage hfinal "mailchimp-reconcile" ./jobs/mailchimp-reconcile;

              order-cleanup = localPackage hfinal "order-cleanup" ./jobs/order-cleanup;

              staged-upload-cleanup = localPackage hfinal "staged-upload-cleanup" ./jobs/staged-upload-cleanup;

              lucid-form-builder = localPackage hfinal "lucid-form-builder" ./lib/lucid-form-builder;

              lucid-htmx-alpine = localPackage hfinal "lucid-htmx-alpine" ./lib/lucid-htmx-alpine;

              lucid-tailwind = localPackage hfinal "lucid-tailwind" ./lib/lucid-tailwind;

              # rel8 1.7.0.0 for hasql 1.9 compatibility
              rel8 = pkgs.haskell.lib.dontCheck (hfinal.callHackageDirect
                {
                  pkg = "rel8";
                  ver = "1.7.0.0";
                  sha256 = "sha256-B6I+y19vWCc0xh6tzCKLzHo7TB+91E7aZHPepm23/jI=";
                }
                { });

              # The last Hackage release of tmp-postgres (1.34.1.0) needs the
              # deprecated ansi-wl-pprint, and nixpkgs marks it broken. This
              # master commit moves it to prettyprinter.
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
                    rev = "27d2cbde2d2a9dd0c6d5bec0664ccb5e5e60f215";
                    sha256 = "sha256-8ti4bIGF+jMlpEY9KUS7cHx4VbKmj+uqjagDa52ZWHw=";
                  } + "/xmlhtml-qq")
                { });

              xmlhtml-lens = pkgs.haskell.lib.dontCheck (hfinal.callCabal2nix "xmlhtml-lens"
                (pkgs.fetchgit
                  {
                    url = "https://github.com/solomon-b/web-server";
                    rev = "27d2cbde2d2a9dd0c6d5bec0664ccb5e5e60f215";
                    sha256 = "sha256-8ti4bIGF+jMlpEY9KUS7cHx4VbKmj+uqjagDa52ZWHw=";
                  } + "/xmlhtml-lens")
                { });
            };
          };
        in
        rec {
          devShell = hsPkgs.shellFor {
            packages = p: map pkgs.haskell.lib.doCheck [ p.kpbj-types p.kpbj-storage p.kpbj-database p.kpbj-email p.kpbj-job p.kpbj-web p.stripe-http p.easypost-http p.mailchimp-http p.google-service-account p.lucid-form-builder p.lucid-htmx-alpine p.lucid-tailwind p.sync-host-emails p.sync-schedule-calendar p.token-cleanup p.episode-check p.listener-snapshots p.ga-poller p.mailchimp-reconcile ];
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
            sync-schedule-calendar = hsPkgs.sync-schedule-calendar;
            token-cleanup = hsPkgs.token-cleanup;
            episode-check = hsPkgs.episode-check;
            listener-snapshots = hsPkgs.listener-snapshots;
            ga-poller = hsPkgs.ga-poller;
            mailchimp-reconcile = hsPkgs.mailchimp-reconcile;
            order-cleanup = hsPkgs.order-cleanup;
            staged-upload-cleanup = hsPkgs.staged-upload-cleanup;
          };

          defaultPackage = packages.kpbj-web;

          # The packages with a test suite. Each one runs its suite as part of its
          # own build, so a check is the same derivation as the package.
          checks = {
            inherit (hsPkgs)
              kpbj-web
              kpbj-database
              kpbj-storage
              kpbj-job
              stripe-http
              easypost-http
              mailchimp-http
              google-service-account
              sync-host-emails
              sync-schedule-calendar;
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
          sync-schedule-calendar = self.packages.x86_64-linux.sync-schedule-calendar;
          token-cleanup = self.packages.x86_64-linux.token-cleanup;
          episode-check = self.packages.x86_64-linux.episode-check;
          listener-snapshots = self.packages.x86_64-linux.listener-snapshots;
          ga-poller = self.packages.x86_64-linux.ga-poller;
          mailchimp-reconcile = self.packages.x86_64-linux.mailchimp-reconcile;
          order-cleanup = self.packages.x86_64-linux.order-cleanup;
          staged-upload-cleanup = self.packages.x86_64-linux.staged-upload-cleanup;
          kpbj-web = self.packages.x86_64-linux.kpbj-web;
        in
        {
          kpbj-prod = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit sync-host-emails sync-schedule-calendar token-cleanup episode-check listener-snapshots ga-poller mailchimp-reconcile order-cleanup staged-upload-cleanup kpbj-web; };
            modules = [
              sops-nix.nixosModules.sops
              friendly-ghost.nixosModules.default
              ./nixos/prod.nix
            ];
          };
          kpbj-staging = nixpkgs.lib.nixosSystem {
            system = "x86_64-linux";
            specialArgs = { inherit sync-host-emails sync-schedule-calendar token-cleanup episode-check listener-snapshots ga-poller mailchimp-reconcile order-cleanup staged-upload-cleanup kpbj-web; };
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
