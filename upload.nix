{ pkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
with pkgs.haskell.lib;
{
   upload = sdistTarball (buildStrictly (pkgs.haskell.packages.${compiler}.callPackage ./google-translate.nix {}));
}
