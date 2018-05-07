{ pkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:
with pkgs.haskell.lib;
{
   upload = sdistTarball (buildStrictly (pkgs.haskell.packages.${compiler}.callPackage ./google-translate.nix {}));
}
