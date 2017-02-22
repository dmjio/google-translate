{ pkgs ? import <nixpkgs> {}, compiler ? "ghc802" }:
{
   upload = pkgs.haskell.lib.sdistTarball (pkgs.haskell.packages.${compiler}.callPackage ./google-translate.nix {});
}
