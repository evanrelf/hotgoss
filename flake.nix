{
  description = "hotgoss";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = inputs@{ flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        hackage = {
          rev = "90320254f3fc3c45ae2f22ed028c8cafe1642f15";
          sha256 = "sha256-/lgPJetm3gqM47BLrYvy6b/OXcW3TUlp2McFH6+9kSM=";
        };

        overlays = [
          (final: prev: {
            maelstrom = final.callPackage ./maelstrom.nix { };
            haskellPackages =
              prev.haskellPackages.override {
                overrides = hfinal: hprev: {
                  large-anon =
                    final.haskell.lib.appendConfigureFlag
                      (hfinal.callHackage "large-anon" "0.3.0" { })
                      "-fdisableFourmoluExec";
                  typelet = final.haskell.lib.unmarkBroken hprev.typelet;
                };
              };
            all-cabal-hashes =
              prev.fetchurl {
                url = "https://github.com/commercialhaskell/all-cabal-hashes/archive/${hackage.rev}.tar.gz";
                sha256 = hackage.sha256;
              };
          })
        ];

        pkgs = import nixpkgs { inherit overlays system; };
      in
      rec {
        packages = rec {
          default = hotgoss;
          hotgoss = pkgs.haskellPackages.callCabal2nix "hotgoss" ./. { };
        };

        devShells = {
          default =
            packages.hotgoss.env.overrideAttrs (prev: {
              buildInputs = [
                pkgs.cabal-install
                pkgs.ghcid
                pkgs.maelstrom
              ];
            });
        };
      }
    );
}
