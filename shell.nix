let
  pkgs = import <nixpkgs> { };

  hotgoss = pkgs.haskellPackages.callCabal2nix "hotgoss" ./. { };

in
hotgoss.env.overrideAttrs (prev: {
  buildInputs = [
    pkgs.cabal-install
    pkgs.ghcid
  ];
})
