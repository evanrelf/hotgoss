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
            haskellPackages =
              prev.haskellPackages.override {
                overrides = hfinal: hprev: {
                  text-display =
                    final.lib.pipe hprev.text-display [
                      final.haskell.lib.compose.unmarkBroken
                      final.haskell.lib.compose.doJailbreak
                      (final.haskell.lib.compose.appendConfigureFlag "-f-book")
                    ];
                };
              };
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
