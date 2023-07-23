{
  description = "hotgoss";

  inputs = {
    flake-utils.url = "github:numtide/flake-utils";
    nixpkgs.url = "github:NixOS/nixpkgs";
  };

  outputs = inputs@{ flake-utils, nixpkgs, ... }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        overlays = [
          (final: prev: {
            maelstrom = final.callPackage ./maelstrom.nix { };
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
