{ pkgs ? import ((import <nixpkgs> {}).fetchFromGitHub {
    owner = "NixOS";
    repo = "nixpkgs";
    rev = "45ffca6ff84ca5e00842900802af577dcfb3e84f";
    sha256 = "11vnmlix4xkifrlpz4a13r6dnncrwnjibnd2j5sl7zb9vklj40lc";
  }) {} }:
let
  ghcjs = pkgs.haskell.packages.ghcjsHEAD.override {
    overrides = self: super: {
      fail = dontHaddock super.fail;
      diagrams-lib = dontCheck super.diagrams-lib;
      miso = self.callPackage ./miso.nix {};
    };
  };
  dontHaddock = pkgs.haskell.lib.dontHaddock;
  dontCheck = pkgs.haskell.lib.dontCheck;
in
  {
    client = dontHaddock (ghcjs.callPackage ./diagrams-miso.nix {});
  }
