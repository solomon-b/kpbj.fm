{
  description = "A very basic static site";

  inputs = {
    nixpkgs.url = github:NixOS/nixpkgs/nixpkgs-unstable;
    flake-utils.url = github:numtide/flake-utils;
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachSystem [ "x86_64-linux" ]
      (system:
        let
          pkgs = import nixpkgs { inherit system; config.allowUnfree = true; };
        in
        rec {
          devShell = pkgs.mkShell {
            buildInputs = [
              pkgs.nodejs
              pkgs.nodePackages.serve
              pkgs.nodePackages.tailwindcss
              pkgs.just
            ];
          };

          formatter = pkgs.nixpkgs-fmt;
        });
}
