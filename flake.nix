{
  description = "Ema template app";
  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixpkgs-unstable";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs.follows = "nixpkgs";
    haskell-flake.url = "github:srid/haskell-flake";

    # Haskell overrides
    ema.url = "github:EmaApps/ema/multisite";
    ema.flake = false;
    emanote.url = "github:EmaApps/emanote";
    emanote.flake = false;
    tailwind.url = "github:srid/tailwind-haskell";
    tailwind.flake = false;
  };
  outputs = inputs@{ self, nixpkgs, flake-parts, haskell-flake, ... }:
    flake-parts.lib.mkFlake { inherit self; } {
      systems = nixpkgs.lib.systems.flakeExposed;
      imports = [
        haskell-flake.flakeModule
      ];
      perSystem = { self', inputs', pkgs, ... }:
        let
          tailwind = pkgs.nodePackages.tailwindcss.overrideAttrs
            (oa: {
              plugins = [
                pkgs.nodePackages."@tailwindcss/aspect-ratio"
                pkgs.nodePackages."@tailwindcss/forms"
                pkgs.nodePackages."@tailwindcss/language-server"
                pkgs.nodePackages."@tailwindcss/line-clamp"
                pkgs.nodePackages."@tailwindcss/typography"
              ];
            });
        in
        {
          # "haskellProjects" comes from https://github.com/srid/haskell-flake
          haskellProjects.default = {
            root = ./.;
            haskellPackages = pkgs.haskell.packages.ghc923;
            buildTools = hp: {
              inherit (pkgs)
                treefmt
                nixpkgs-fmt
                foreman;
              inherit (hp)
                # cabal-fmt (Broken on GHC 9.2.3)
                fourmolu;

              inherit tailwind;

              # https://github.com/NixOS/nixpkgs/issues/140774 reoccurs in GHC 9.2
              ghcid = pkgs.haskell.lib.overrideCabal hp.ghcid (drv: {
                enableSeparateBinOutput = false;
              });
            };
            source-overrides = {
              inherit (inputs)
                ema emanote tailwind;
            };
            overrides = self: super: with pkgs.haskell.lib; {
              # GHC 9.2 overrides below:
              relude = dontCheck (self.callHackage "relude" "1.1.0.0" { }); # 1.1 not in nixpkgs yet 
              retry = dontCheck super.retry; # For GHC 9.2.
              streaming-commons = dontCheck super.streaming-commons; # Fails on darwin
              http2 = dontCheck super.http2; # Fails on darwin

              # Ema overrides below:
              type-errors-pretty = dontCheck (doJailbreak super.type-errors-pretty);

              # Emanote overrides below:
              heist-emanote = dontCheck (doJailbreak (unmarkBroken super.heist-emanote));
              ixset-typed = unmarkBroken super.ixset-typed;
              pandoc-link-context = unmarkBroken super.pandoc-link-context;
              generic-data = dontCheck super.generic-data; # Needed on GHC 9.2
              tailwind = addBuildTools super.tailwind [ tailwind ];
            };
          };
        };
    };
}
